module Main exposing (main)

import Ansi
import Ansi.Color
import Ansi.Cursor
import Ansi.Font
import Ansi.String
import Capabilities
import Cli
import Command exposing (Command)
import Common
import Dict exposing (Dict)
import Env
import Extra.List
import Extra.String
import Fs
import Fs.Location
import Fs.Path
import Json.Decode
import Os
import Os.Process
import Os.Pty
import Regex exposing (Regex)
import Stdin
import Task exposing (Task)


main : Cli.Program Model Msg
main =
    Cli.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        }


type Model
    = ExitEarly
    | Initializing Cli.Env Fs.FileSystem Capabilities.Stdin
    | RunInteractive RunningModel


type alias RunningModel =
    { stdout : Capabilities.Console
    , stderr : Capabilities.Console
    }


init : Cli.Env -> ( Model, Cmd Msg )
init env =
    case Fs.require env of
        Err _ ->
            ( ExitEarly
            , Cli.dieCmd env 1 (filesystemAccessText env.programName)
            )

        Ok fs ->
            case env.stdin of
                Nothing ->
                    ( ExitEarly
                    , Cli.dieCmd env 1 "[TODO]: missing stdin"
                    )

                Just stdin ->
                    case Os.requireProcess env of
                        Err _ ->
                            ( ExitEarly
                            , Cli.dieCmd env 1 "[TODO] requre process info"
                            )

                        Ok process ->
                            ( Initializing env fs stdin
                            , systemInfoTask env process
                                |> Task.andThen (\systemInfo -> parseArgs env fs systemInfo env.args)
                                |> Task.andThen
                                    (\startTag ->
                                        case startTag of
                                            Parsed _ _ ->
                                                if env.terminalInfo.stdinIsTerminal then
                                                    Stdin.getWindowSize stdin
                                                        |> Task.mapError StdinError
                                                        |> Task.map (\winSize -> ( startTag, Just winSize ))

                                                else
                                                    Task.succeed ( startTag, Nothing )

                                            _ ->
                                                Task.succeed ( startTag, Nothing )
                                    )
                                |> Task.attempt ArgsParsed
                            )


type Error
    = FsError Fs.FsError
    | StdinError Stdin.StdinError
    | JsonError Json.Decode.Error
    | ProcessError Os.Process.ProcessError
    | Other String
    | Impossible Never
    | ExpectedFail


parseArgs : Cli.Env -> Fs.FileSystem -> SystemInfo -> List String -> Task Error StartTag
parseArgs env fs systemInfo args =
    case args of
        [] ->
            Cli.printlnTask env.stdout helpText
                |> Task.mapError Impossible
                |> Task.andThen (\_ -> Task.fail ExpectedFail)

        _ ->
            let
                ( flags, restArgs ) =
                    partitionArgs args
            in
            forEachFlag env systemInfo flags
                |> Task.andThen
                    (\autoExit ->
                        parseRestArgs fs autoExit restArgs
                    )


parseRestArgs : Fs.FileSystem -> Maybe Int -> List String -> Task Error StartTag
parseRestArgs fs autoExit args =
    case args of
        [] ->
            Debug.todo "parseRestArgs []"

        [ configPath ] ->
            Fs.readTextFile fs (Fs.Location.file configPath)
                |> Task.mapError FsError
                |> Task.andThen (parseInputFile autoExit)

        delimiter :: rest ->
            gatherCommands autoExit delimiter rest


gatherCommands : Maybe Int -> String -> List String -> Task Error StartTag
gatherCommands autoExit delimiter args =
    List.foldl
        (\arg ( command, commands ) ->
            if arg == delimiter then
                if List.length command > 0 then
                    ( [], command :: commands )

                else
                    ( command, commands )

            else
                ( arg :: command, commands )
        )
        ( [], [] )
        args
        |> (\( command, commands ) ->
                if List.length command > 0 then
                    command :: commands

                else
                    commands
           )
        |> (\commands ->
                case commands of
                    [] ->
                        NoCommands

                    cmds ->
                        Parsed
                            (List.filterMap
                                (\command ->
                                    case command of
                                        [] ->
                                            Nothing

                                        cmd :: rest ->
                                            Just
                                                { title = Command.toPresentationName command
                                                , cwd = "."
                                                , command = ( cmd, rest )
                                                , status = []
                                                , defaultStatus = Nothing
                                                , killAllSequence = Common.keyCodes.kill
                                                }
                                )
                                commands
                            )
                            autoExit
           )
        |> Task.succeed


parseInputFile : Maybe Int -> String -> Task Error StartTag
parseInputFile autoExit config =
    case Json.Decode.decodeString (Json.Decode.list Command.descriptionDecoder) config of
        Err err ->
            Task.fail (JsonError err)

        Ok [] ->
            Task.succeed NoCommands

        Ok commands ->
            Task.succeed (Parsed commands autoExit)


forEachFlag : Cli.Env -> SystemInfo -> List String -> Task Error (Maybe Int)
forEachFlag env systemInfo flags =
    case flags of
        [] ->
            Task.succeed Nothing

        flag :: restFlags ->
            if flag == "-h" || flag == "--help" then
                Cli.printlnTask env.stdout helpText
                    |> Task.mapError Impossible
                    |> Task.andThen (\_ -> Task.fail ExpectedFail)

            else
                case Regex.find autoExitRegex flag of
                    [ { submatches } ] ->
                        case submatches of
                            [ Nothing ] ->
                                Task.succeed <| Just systemInfo.cpus

                            [ Just "auto" ] ->
                                Task.succeed <| Just systemInfo.cpus

                            [ Just "0" ] ->
                                Cli.printlnTask env.stdout "--auto-exit=0 will never finish."
                                    |> Task.mapError Impossible
                                    |> Task.andThen (\_ -> Task.fail ExpectedFail)

                            [ Just numStr ] ->
                                case String.toInt numStr of
                                    Nothing ->
                                        Cli.printlnTask env.stdout ("Bad flag: " ++ flag ++ "\nOnly these forms are accepted:\n" ++ autoExitHelp)
                                            |> Task.mapError Impossible
                                            |> Task.andThen (\_ -> Task.fail ExpectedFail)

                                    Just cpuCount ->
                                        Task.succeed <| Just cpuCount

                            _ ->
                                Cli.printlnTask env.stdout ("Bad flag: " ++ flag ++ "\nOnly these forms are accepted:\n" ++ autoExitHelp)
                                    |> Task.mapError Impossible
                                    |> Task.andThen (\_ -> Task.fail ExpectedFail)

                    _ ->
                        Cli.printlnTask env.stdout ("Bad flag: " ++ flag ++ "\nOnly these forms are accepted:\n" ++ autoExitHelp)
                            |> Task.mapError Impossible
                            |> Task.andThen (\_ -> Task.fail ExpectedFail)


type alias SystemInfo =
    { os : OS
    , cpus : Int
    , architecture : String
    }


type OS
    = Darwin
    | Linux
    | Windows
    | OtherOS String


systemInfoTask : Cli.Env -> Os.ProcessCapability -> Task Error SystemInfo
systemInfoTask env process =
    Os.Process.spawn process
        "uname"
        { args = [ "-sm" ]
        , cwd = Nothing
        , env = Nothing
        , stdin = Os.Process.NullStdin
        , stdout =
            Os.Process.CaptureStdout
                { maxBytes = 1024
                , onOverflow = Os.Process.TruncateOutput
                }
        , stderr = Os.Process.InheritStderr
        }
        |> Task.andThen (\s -> Os.Process.wait process s.pid)
        |> Task.mapError ProcessError
        |> Task.andThen (unixLike env process)
        |> Task.onError (possiblyWindows env)


unixLike : Cli.Env -> Os.ProcessCapability -> Os.Process.Completed -> Task Error SystemInfo
unixLike env process { stdout } =
    case stdout of
        Nothing ->
            Task.fail (Other "Failed to get uname")

        Just uname ->
            case String.words uname of
                [ "Linux", arch ] ->
                    linuxDetails process arch

                [ "Darwin", arch ] ->
                    macDetails process arch

                _ ->
                    Task.fail (Other ("Unknown OS and arch: " ++ uname))


linuxDetails : Os.ProcessCapability -> String -> Task Error SystemInfo
linuxDetails process arch =
    Os.Process.spawn process
        "nproc"
        { args = []
        , cwd = Nothing
        , env = Nothing
        , stdin = Os.Process.NullStdin
        , stdout =
            Os.Process.CaptureStdout
                { maxBytes = 1024
                , onOverflow = Os.Process.TruncateOutput
                }
        , stderr = Os.Process.InheritStderr
        }
        |> Task.andThen (\s -> Os.Process.wait process s.pid)
        |> Task.mapError ProcessError
        |> Task.andThen
            (\{ stdout } ->
                case stdout of
                    Nothing ->
                        Task.fail (Other "Failed to get cpu count")

                    Just cpuCountStr ->
                        case String.toInt (String.trim cpuCountStr) of
                            Nothing ->
                                Task.fail (Other "Unable to get CPU count - Linux")

                            Just cpus ->
                                Task.succeed
                                    { os = Linux
                                    , cpus = cpus
                                    , architecture = arch
                                    }
            )


macDetails : Os.ProcessCapability -> String -> Task Error SystemInfo
macDetails process arch =
    Os.Process.spawn process
        "sysctl"
        { args = [ "-n", "hw.logicalcpu" ]
        , cwd = Nothing
        , env = Nothing
        , stdin = Os.Process.NullStdin
        , stdout =
            Os.Process.CaptureStdout
                { maxBytes = 1024
                , onOverflow = Os.Process.TruncateOutput
                }
        , stderr = Os.Process.InheritStderr
        }
        |> Task.andThen (\s -> Os.Process.wait process s.pid)
        |> Task.mapError ProcessError
        |> Task.andThen
            (\{ stdout } ->
                case stdout of
                    Nothing ->
                        Task.fail (Other "Failed to get cpu count")

                    Just cpuCountStr ->
                        case String.toInt (String.trim cpuCountStr) of
                            Nothing ->
                                Task.fail (Other "Unable to get CPU count - Mac")

                            Just cpus ->
                                Task.succeed
                                    { os = Darwin
                                    , cpus = cpus
                                    , architecture = arch
                                    }
            )


possiblyWindows : Cli.Env -> Error -> Task Error SystemInfo
possiblyWindows env error =
    case error of
        ProcessError (Os.Process.ProcessError _) ->
            case ( Env.getString (Env.name "PROCESSOR_ARCHITECTURE") env, Env.getString (Env.name "NUMBER_OF_PROCESSORS") env |> Maybe.andThen String.toInt ) of
                ( Just arch, Just cpus ) ->
                    Task.succeed
                        { os = Windows
                        , cpus = cpus
                        , architecture = arch
                        }

                ( Nothing, _ ) ->
                    Task.fail (Other "Unknown Windows arch")

                ( _, Nothing ) ->
                    Task.fail (Other "Unable to get CPU count - Windows")

        _ ->
            Task.fail error


partitionArgs : List String -> ( List String, List String )
partitionArgs args =
    partitionArgsHelper [] args


partitionArgsHelper : List String -> List String -> ( List String, List String )
partitionArgsHelper flags args =
    case args of
        [] ->
            ( List.reverse flags, args )

        arg :: restArgs ->
            if not (Regex.contains looksLikeFlagRegex arg) then
                ( List.reverse flags, args )

            else
                partitionArgsHelper (arg :: flags) restArgs


looksLikeFlagRegex : Regex
looksLikeFlagRegex =
    Regex.fromString "^--?\\w"
        |> Maybe.withDefault Regex.never


autoExitRegex : Regex
autoExitRegex =
    Regex.fromString "^--auto-exit(?:=(\\d+|auto))?$"
        |> Maybe.withDefault Regex.never


type StartTag
    = NoCommands
    | Parsed (List Command.Description) (Maybe Int)


helpText : String
helpText =
    """Run several commands concurrently.
Show output for one command at a time.
Kill all at once.

Separate the commands with a character of choice:

    """ ++ appName ++ """ """ ++ percent ++ """ npm start """ ++ percent ++ """ make watch """ ++ percent ++ """ some_command arg1 arg2 arg3

    """ ++ appName ++ """ """ ++ atSymbol ++ """ ./report_progress.bash --root / --unit % """ ++ atSymbol ++ """ ping localhost

Alternatively, specify the commands in a JSON file:

    """ ++ appName ++ """ run-pty.json

You can tell run-pty to exit once all commands have exited with status 0:

    """ ++ appName ++ """ --auto-exit """ ++ percent ++ """ npm ci """ ++ percent ++ """ dotnet restore """ ++ atSymbol ++ """ node build.js

""" ++ autoExitHelp ++ """

Keyboard shortcuts:

    """ ++ shortcut keys.dashboard ++ """ Dashboard
    """ ++ shortcut keys.kill ++ """ Kill all or focused command
    Other keyboard shortcuts are shown as needed.

Environment variables:

    """ ++ Ansi.Font.bold "RUN_PTY_MAX_HISTORY" ++ """
        Number of characters of output to remember.
        Higher → more command scrollback
        Lower  → faster switching between commands
        Default: """ ++ String.fromInt maxHistoryDefault ++ """

    """ ++ Ansi.Font.bold "NO_COLOR" ++ """
        Disable colored output."""


appName : String
appName =
    Ansi.Font.bold "run-pty"


percent : String
percent =
    Ansi.Font.faint "%"


atSymbol : String
atSymbol =
    Ansi.Font.faint "@"


andSymbol : String
andSymbol =
    Ansi.Font.faint "&&"


autoExitHelp : String
autoExitHelp =
    """    --auto-exit=<number>   auto exit when done, with at most <number> parallel processes
    --auto-exit=auto       uses the number of logical CPU cores
    --auto-exit            defaults to auto"""


shortcut : String -> String
shortcut str =
    Ansi.Font.faint "["
        ++ Ansi.Font.bold str
        ++ Ansi.Font.faint "]"


shortcutPadded : String -> String
shortcutPadded str =
    shortcut str ++ String.repeat (max 0 (String.length keys.kill - String.length str)) " "


keys =
    { kill = "ctrl+c"
    , restart = "enter"
    , dashboard = "ctrl+z"
    , navigate = "↑↓←→"
    , navigateVerticallyOnly = "↑↓"
    , enter = "enter"
    , unselect = "escape"
    }


maxHistoryDefault =
    1000000


filesystemAccessText : String -> String
filesystemAccessText progName =
    String.join "\n"
        [ "Usage: run --allow-fs-ro=. " ++ progName ++ " [PATH]"
        , ""
        , "List directory contents. Defaults to current directory."
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = ArgsParsed (Result Error ( StartTag, Maybe Stdin.WindowSize ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initializing env fs stdin, ArgsParsed (Err ExpectedFail) ) ->
            ( ExitEarly
            , Cmd.none
            )

        ( Initializing env fs stdin, ArgsParsed (Err err) ) ->
            ( ExitEarly
            , Debug.todo ("update ArgsParsed error: " ++ Debug.toString err)
            )

        ( Initializing env fs stdin, ArgsParsed (Ok ( tag, windowSize )) ) ->
            case tag of
                NoCommands ->
                    ( ExitEarly
                    , Cli.exit 0
                    )

                Parsed commandDescriptions autoExit ->
                    if env.terminalInfo.stdinIsTerminal then
                        case windowSize of
                            Nothing ->
                                Debug.todo ""

                            Just winSize ->
                                ( model, runInteractively winSize env commandDescriptions autoExit )

                    else
                        case autoExit of
                            Just maxParallel ->
                                ( model, runNonInteractively commandDescriptions maxParallel )

                            Nothing ->
                                ( ExitEarly
                                , Cli.dieCmd env 1 "run-pty requires stdin to be a TTY to run properly (unless --auto-exit is used)."
                                )

        _ ->
            ( model, Cmd.none )


runInteractively : Stdin.WindowSize -> Cli.Env -> List Command.Description -> Maybe Int -> Cmd Msg
runInteractively windowSize env commandDescriptions autoExit =
    let
        commands =
            List.indexedMap
                (\index commandDescription ->
                    Command.new env
                        { label = Just (Extra.String.atIndex index allLabels)
                        , addHistoryStart = True
                        , commandDescription = commandDescription
                        , onData = ()
                        , onRequest = ()
                        , onSynchronizedOutputChange = ()
                        , onExit = ()
                        }
                )
                commandDescriptions
    in
    switchToDashboard windowSize env commands autoExit False (Dashboard [])


alphabet : String
alphabet =
    "abcdefghijklmnopqrstuvwxyz"


labelGroups : List String
labelGroups =
    [ "123456789", alphabet, String.toUpper alphabet ]


allLabels : String
allLabels =
    String.concat labelGroups


type State
    = Dashboard (List String)


type Selection
    = Invisible Int
    | ByIndicator String
    | MouseDown Int
    | Keyboard Int


switchToDashboard : Stdin.WindowSize -> Cli.Env -> List Command -> Maybe Int -> Bool -> State -> Cmd Msg
switchToDashboard windowSize env commands autoExit forceClearScrollback state =
    let
        previousRender =
            case state of
                Dashboard current ->
                    if
                        (List.length current <= windowSize.rows)
                            && (Maybe.withDefault 0 (List.maximum (List.map Ansi.String.width current)) <= windowSize.cols)
                            && not forceClearScrollback
                    then
                        current

                    else
                        []

        currentRender =
            drawDashboard env
                { commands = commands
                , width = windowSize.cols
                , attemptedKillAll = False
                , autoExit = autoExit
                , selection = Invisible 0
                }
                |> String.lines
                |> List.take windowSize.rows

        clear =
            if List.length previousRender == 0 then
                Ansi.clearScreen

            else
                ""

        numLinesToClear =
            List.length previousRender - List.length currentRender

        -- current =
        --     Dashboard currentRender
    in
    Cli.print env.stdout
        (beginSyncUpdate
            ++ Ansi.Cursor.hide
            ++ disableAlternateScreen
            ++ disableApplicationCursorKeys
            ++ enableMouse
            ++ Common.resetColor
            ++ clear
            ++ (currentRender
                    |> List.indexedMap
                        (\index line ->
                            case listAt index previousRender of
                                Nothing ->
                                    cursorAbsolute (index + 1) 1 ++ Ansi.eraseLineAfter ++ line

                                Just atIdx ->
                                    if atIdx == line then
                                        ""

                                    else
                                        cursorAbsolute (index + 1) 1 ++ Ansi.eraseLineAfter ++ line
                        )
                    |> String.join ""
               )
            ++ (List.range 0 (numLinesToClear - 1)
                    |> List.map
                        (\index ->
                            cursorAbsolute (List.length currentRender + index + 1) 1 ++ Ansi.eraseLineAfter
                        )
                    |> String.join ""
               )
            ++ endSyncUpdate
        )


drawDashboard : Cli.Env -> { commands : List Command, width : Int, attemptedKillAll : Bool, autoExit : Maybe Int, selection : Selection } -> String
drawDashboard env { commands, width, attemptedKillAll, autoExit, selection } =
    -- const done = isDone({ commands, attemptedKillAll, autoExit });
    let
        finalLines =
            drawDashboardCommandLines env
                commands
                -- done ? { tag: "Invisible", index: 0 } : selection,
                selection
                { width = width
                , useSeparateKilledIndicator = autoExit /= Nothing
                }
                |> List.map (\{ line } -> line)
                |> String.join "\n"
    in
    -- if (done) {
    --     return `${finalLines}\n`;
    -- }
    -- const label = summarizeLabels(commands.map((command) => command.label));
    -- const enter =
    --     selection.tag === "Keyboard"
    --     ? `${shortcut(KEYS.enter)} focus selected${getPid(
    --         commands[selection.index],
    --         )}\n${shortcut(KEYS.unselect)} unselect`
    --     : selection.tag === "ByIndicator"
    --         ? `${shortcut(KEYS.enter)} ${
    --             commands.some(
    --             (command) =>
    --                 getIndicatorChoice(command) === selection.indicator &&
    --                 command.status.tag === "Killing",
    --             )
    --             ? "force "
    --             : ""
    --         }restart selected\n${shortcut(KEYS.unselect)} unselect`
    --         : autoExit.tag === "AutoExit"
    --         ? commands.some(
    --             (command) =>
    --                 command.status.tag === "Exit" &&
    --                 (command.status.exitCode !== 0 || command.status.wasKilled),
    --             )
    --             ? `${shortcut(KEYS.enter)} restart failed`
    --             : ""
    --         : commands.some((command) => command.status.tag === "Exit")
    --             ? `${shortcut(KEYS.enter)} restart exited`
    --             : "";
    -- const navigationKeys =
    --     autoExit.tag === "AutoExit" ? KEYS.navigateVerticallyOnly : KEYS.navigate;
    -- const sessionEnds = "The session ends automatically once all commands are ";
    -- const autoExitText =
    --     autoExit.tag === "AutoExit"
    --     ? [
    --         enter === "" ? undefined : "",
    --         `At most ${autoExit.maxParallel} ${
    --             autoExit.maxParallel === 1 ? "command runs" : "commands run"
    --         } at a time.`,
    --         `${sessionEnds}${exitIndicator(0)}${cursorHorizontalAbsolute(
    --             sessionEnds.length + ICON_WIDTH + 1,
    --         )} ${bold("exit 0")}.`,
    --         ]
    --         .filter((x) => x !== undefined)
    --         .join("\n")
    --     : "";
    -- return `
    -- ${finalLines}
    -- ${shortcut(label)} focus command ${dim("(or click)")}
    -- ${shortcut(KEYS.kill)} ${killAllLabel(commands)}
    -- ${shortcut(navigationKeys)} move selection
    -- ${enter}
    -- ${autoExitText}
    -- `.trim();
    finalLines


drawDashboardCommandLines : Cli.Env -> List Command -> Selection -> { width : Int, useSeparateKilledIndicator : Bool } -> List { length : Int, line : String }
drawDashboardCommandLines env commands selection { width, useSeparateKilledIndicator } =
    let
        lines =
            List.map
                (\command ->
                    let
                        ( icon, status ) =
                            statusText env
                                command.status
                                { statusFromRules =
                                    case command.statusFromRules of
                                        Just statusFromRules ->
                                            statusFromRules

                                        Nothing ->
                                            runningIndicator env
                                , useSeparateKilledIndicator = useSeparateKilledIndicator
                                }
                    in
                    { label = shortcut command.label
                    , icon = icon
                    , status = status
                    , title = titlePossiblyWithGraphicRenditions env command
                    }
                )
                commands

        separator =
            "  "

        widestStatus =
            lines
                |> List.map
                    (\{ status } ->
                        case status of
                            Nothing ->
                                0

                            Just s ->
                                Ansi.String.width s
                    )
                |> List.maximum
                |> Maybe.withDefault 0

        selectedIndicator =
            case selection of
                ByIndicator indicator ->
                    Just indicator

                _ ->
                    Nothing

        ( iconWidth, _ ) =
            Common.iconEmojiFix env
    in
    List.indexedMap
        (\index { label, icon, status, title } ->
            let
                finalIcon =
                    if Just icon == selectedIndicator then
                        if env.terminalInfo.noColor then
                            String.slice 0 -1 separator ++ "→" ++ icon

                        else
                            -- Add spaces at the end to make sure that two terminal slots get
                            -- inverted, no matter the actual width of the icon (which may even
                            -- be the empty string).
                            String.slice 0 -1 separator ++ Ansi.Color.invert (" " ++ icon ++ String.repeat iconWidth " ")

                    else
                        separator ++ icon

                start =
                    truncateString (label ++ finalIcon) width

                separatorLength =
                    Ansi.String.width separator

                startLength =
                    Ansi.String.width (Common.removeGraphicRenditions label) + separatorLength + iconWidth

                end =
                    case status of
                        Nothing ->
                            title

                        Just s ->
                            String.padRight widestStatus ' ' (s ++ separator ++ title)

                truncatedEnd =
                    truncateString end (width - startLength - separatorLength)

                length =
                    startLength
                        + separatorLength
                        + Ansi.String.width (Common.removeGraphicRenditions truncatedEnd)

                highlightedSeparator =
                    if Just icon == selectedIndicator && not env.terminalInfo.noColor then
                        Ansi.Color.invert " " ++ String.slice 0 1 separator

                    else
                        separator

                finalEnd =
                    case selection of
                        MouseDown idx ->
                            if index == idx then
                                if env.terminalInfo.noColor then
                                    String.slice 0 -1 highlightedSeparator ++ "→" ++ truncatedEnd

                                else
                                    highlightedSeparator ++ Ansi.Color.invert truncatedEnd

                            else
                                highlightedSeparator ++ truncatedEnd

                        Keyboard idx ->
                            if index == idx then
                                if env.terminalInfo.noColor then
                                    String.slice 0 -1 highlightedSeparator ++ "→" ++ truncatedEnd

                                else
                                    highlightedSeparator ++ Ansi.Color.invert truncatedEnd

                            else
                                highlightedSeparator ++ truncatedEnd

                        _ ->
                            highlightedSeparator ++ truncatedEnd
            in
            { line = start ++ Common.resetColor ++ Common.cursorHorizontalAbsolute (startLength + 1) ++ Ansi.eraseLineAfter ++ finalEnd ++ Common.resetColor
            , length = length
            }
        )
        lines


truncateString : String -> Int -> String
truncateString string maxLength =
    string
        |> Regex.split Common.graphicRenditions
        |> Extra.List.foldlOrBreak
            (\part ( result, index, length ) ->
                if modBy 2 index == 0 then
                    let
                        partLength : Int
                        partLength =
                            Ansi.String.width part

                        diff : Int
                        diff =
                            maxLength - length - partLength
                    in
                    if diff < 0 then
                        Extra.List.Break ( result ++ String.slice 0 (diff - 1) part ++ "…", index, length )

                    else
                        Extra.List.Continue ( result ++ part, index + 1, length + partLength )

                else
                    Extra.List.Continue ( result ++ part, index + 1, length )
            )
            ( "", 0, 0 )
        |> (\( res, foo, bar ) -> res)


statusText : Cli.Env -> Command.Status -> { statusFromRules : String, useSeparateKilledIndicator : Bool } -> ( String, Maybe String )
statusText env status { statusFromRules, useSeparateKilledIndicator } =
    case status of
        Command.Waiting ->
            ( Common.waitingIndicator env, Nothing )

        Command.Running ->
            ( statusFromRules, Nothing )

        Command.Killing restartAfterKill ->
            ( if restartAfterKill then
                restartingIndicator env

              else
                killingIndicator env
            , Nothing
            )

        Command.Exit wasKilled exitCode ->
            ( if wasKilled && useSeparateKilledIndicator then
                abortedIndicator env

              else
                exitIndicator env exitCode
            , Just <| Ansi.Font.bold ("exit " ++ String.fromInt exitCode)
            )


titlePossiblyWithGraphicRenditions : Cli.Env -> Command -> String
titlePossiblyWithGraphicRenditions env command =
    if env.terminalInfo.noColor then
        Common.removeGraphicRenditions command.title

    else
        command.title


runningIndicator : Cli.Env -> String
runningIndicator env =
    if env.terminalInfo.noColor then
        "›"

    else if Common.supportsEmoji then
        "🟢"

    else
        "\\x1B[92m●" ++ Common.resetColor


killingIndicator : Cli.Env -> String
killingIndicator env =
    if env.terminalInfo.noColor then
        "○"

    else if Common.supportsEmoji then
        "⭕"

    else
        "\\x1B[91m○" ++ Common.resetColor


restartingIndicator : Cli.Env -> String
restartingIndicator env =
    if env.terminalInfo.noColor then
        "◌"

    else if Common.supportsEmoji then
        "🔄"

    else
        "\\x1B[96m◌" ++ Common.resetColor


abortedIndicator : Cli.Env -> String
abortedIndicator env =
    if env.terminalInfo.noColor then
        "▲"

    else if Common.supportsEmoji then
        "⛔️"

    else
        "\\x1B[91m▲" ++ Common.resetColor


exitIndicator : Cli.Env -> Int -> String
exitIndicator env exitCode =
    -- 130 (128 + 2 (SIGINT)) commonly means exit by ctrl+c.
    if exitCode == 0 || exitCode == 130 then
        if env.terminalInfo.noColor then
            "●"

        else if Common.supportsEmoji then
            "⚪"

        else
            "\\x1B[97m●" ++ Common.resetColor

    else if env.terminalInfo.noColor then
        "×"

    else if Common.supportsEmoji then
        "🔴"

    else
        "\\x1B[91m●" ++ Common.resetColor


beginSyncUpdate : String
beginSyncUpdate =
    "\\x1B[?2026h"


endSyncUpdate : String
endSyncUpdate =
    "\\x1B[?2026l"


disableAlternateScreen : String
disableAlternateScreen =
    "\\x1B[?1049l"


disableApplicationCursorKeys : String
disableApplicationCursorKeys =
    "\\x1B[?1l"


{-| <https://www.vt100.net/docs/vt510-rm/DECCKM.html>
-}
enableMouse : String
enableMouse =
    "\\x1B[?1000;1006h"


cursorAbsolute : Int -> Int -> String
cursorAbsolute y x =
    "\\x1B[" ++ String.fromInt y ++ ";" ++ String.fromInt x ++ "H"


listAt : Int -> List a -> Maybe a
listAt index list =
    List.head (List.drop index list)



-- runNonInteractively : List Command -> Int -> ()


runNonInteractively commands maxParallel =
    Debug.todo "runNonInteractively"
