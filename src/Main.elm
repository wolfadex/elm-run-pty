module Main exposing (main)

import Ansi
import Ansi.Color
import Ansi.Cursor
import Ansi.Font
import Capabilities
import Cli
import Command
import Common
import Dict exposing (Dict)
import Fs
import Fs.Location
import Fs.Path
import Json.Decode
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
                    ( Initializing env fs stdin
                    , parseArgs env fs env.args
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
    | Other String
    | Impossible Never
    | ExpectedFail


parseArgs : Cli.Env -> Fs.FileSystem -> List String -> Task Error StartTag
parseArgs env fs args =
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
            forEachFlag env flags
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
                                                { title = Just (Command.toPresentationName command)
                                                , cwd = Just "."
                                                , command = ( cmd, rest )
                                                , status = []
                                                , defaultStatus = Nothing
                                                , killAllSequence = Just Common.keyCodes.kill
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


type alias Command =
    { command : ( String, List String )
    , title : Maybe String
    , cwd : Maybe String
    , status : List ( Regex, String )
    , defaultStatus : Maybe ( String, String )
    , killAllSequence : Maybe String
    }


forEachFlag : Cli.Env -> List String -> Task Error (Maybe Int)
forEachFlag env flags =
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
                                Task.succeed <| Just (Debug.todo "os.cpus.length")

                            [ Just "auto" ] ->
                                Task.succeed <| Just (Debug.todo "os.cpus.length")

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
    | Parsed (List Command) (Maybe Int)


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

                Parsed commands autoExit ->
                    if env.terminalInfo.stdinIsTerminal then
                        case windowSize of
                            Nothing ->
                                Debug.todo ""

                            Just winSize ->
                                runInteractively winSize env commands autoExit

                    else
                        case autoExit of
                            Just maxParallel ->
                                runNonInteractively commands maxParallel

                            Nothing ->
                                ( ExitEarly
                                , Cli.dieCmd env 1 "run-pty requires stdin to be a TTY to run properly (unless --auto-exit is used)."
                                )

        _ ->
            ( model, Cmd.none )



-- runInteractively : List Command -> Maybe Int -> ()


runInteractively windowSize env commands autoExit =
    switchToDashboard windowSize env commands autoExit False (Dashboard [])


type State
    = Dashboard (List String)


type Selection
    = Invisible Int


switchToDashboard windowSize env commands autoExit forceClearScrollback state =
    let
        previousRender =
            case state of
                Dashboard current ->
                    if
                        (List.length current <= env.stdout.rows)
                            && (Maybe.withDefault 0 (List.maximum (List.map String.length current)) <= env.stdout.columns)
                            && not forceClearScrollback
                    then
                        current

                    else
                        []

        currentRender =
            drawDashboard env
                { commands = commands
                , width = env.stdout.columns
                , attemptedKillAll = False
                , autoExit = autoExit
                , selection = Invisible 0
                }
                |> String.lines
                |> List.take env.stdout.rows

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
                                    cursorAbsolute ( index + 1, 1 ) ++ Ansi.eraseLineAfter ++ line

                                Just atIdx ->
                                    if atIdx == line then
                                        ""

                                    else
                                        cursorAbsolute ( index + 1, 1 ) ++ Ansi.eraseLineAfter ++ line
                        )
                    |> String.join ""
               )
            ++ (List.range 0 (numLinesToClear - 1)
                    |> List.map
                        (\index ->
                            cursorAbsolute (currentRender.length + index + 1) 1 ++ Ansi.eraseLineAfter
                        )
                    |> String.join ""
               )
            ++ endSyncUpdate
        )


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


drawDashboardCommandLines env commands selection { width, useSeparateKilledIndicator } =
    let
        --   const lines = commands.map((command) => {
        --     const [icon, status] = statusText(command.status, {
        --       statusFromRules: command.statusFromRules ?? runningIndicator,
        --       useSeparateKilledIndicator,
        --     });
        --     const { label = " " } = command;
        --     return {
        --       label: shortcut(label, { pad: false }),
        --       icon,
        --       status,
        --       title: command.titlePossiblyWithGraphicRenditions,
        --     };
        --   });
        lines =
            List.map
                (\command ->
                    let
                        ( icon, status ) =
                            statusText env
                                command.status
                                { statusFromRules = command.statusFromRules || runningIndicator
                                , -- command.statusFromRules ?? runningIndicator,
                                  useSeparateKilledIndicator = useSeparateKilledIndicator
                                }
                    in
                    { label = shortcut (Maybe.withDefault " " command.label)
                    , icon = icon
                    , status = status
                    , title = titlePossiblyWithGraphicRenditions command
                    }
                )
                commands
    in
    --   const separator = "  ";
    --   const widestStatus = Math.max(
    --     0,
    --     ...lines.map(({ status }) => (status === undefined ? 0 : status.length)),
    --   );
    --   const selectedIndicator =
    --     selection.tag === "ByIndicator" ? selection.indicator : undefined;
    --   return lines.map(({ label, icon, status, title }, index) => {
    --     const finalIcon =
    --       icon === selectedIndicator
    --         ? NO_COLOR
    --           ? `${separator.slice(0, -1)}→${icon}`
    --           : // Add spaces at the end to make sure that two terminal slots get
    --             // inverted, no matter the actual width of the icon (which may even
    --             // be the empty string).
    --             `${separator.slice(0, -1)}${invert(
    --               ` ${icon}${" ".repeat(ICON_WIDTH)}`,
    --             )}`
    --         : `${separator}${icon}`;
    --     const start = truncate(`${label}${finalIcon}`, width);
    --     const startLength =
    --       removeGraphicRenditions(label).length + separator.length + ICON_WIDTH;
    --     const end =
    --       status === undefined
    --         ? title
    --         : `${status.padEnd(widestStatus, " ")}${separator}${title}`;
    --     const truncatedEnd = truncate(end, width - startLength - separator.length);
    --     const length =
    --       startLength +
    --       separator.length +
    --       removeGraphicRenditions(truncatedEnd).length;
    --     const highlightedSeparator =
    --       icon === selectedIndicator && !NO_COLOR
    --         ? invert(" ") + separator.slice(1)
    --         : separator;
    --     const finalEnd =
    --       (selection.tag === "Mousedown" || selection.tag === "Keyboard") &&
    --       index === selection.index
    --         ? NO_COLOR
    --           ? `${highlightedSeparator.slice(0, -1)}→${truncatedEnd}`
    --           : `${highlightedSeparator}${invert(truncatedEnd)}`
    --         : `${highlightedSeparator}${truncatedEnd}`;
    --     return {
    --       line: `${start}${RESET_COLOR}${cursorHorizontalAbsolute(
    --         startLength + 1,
    --       )}${CLEAR_RIGHT}${finalEnd}${RESET_COLOR}`,
    --       length,
    --     };
    --   });
    -- };
    List.map
        ()
        lines


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


titlePossiblyWithGraphicRenditions env command =
    if env.terminalInfo.noColor then
        Common.removeGraphicRenditions command.title

    else
        command.title


runningIndicator env =
    if env.terminalInfo.noColor then
        "›"

    else if Common.supportsEmoji then
        "🟢"

    else
        "\\x1B[92m●" ++ Common.resetColor


killingIndicator env =
    if env.terminalInfo.noColor then
        "○"

    else if Common.supportsEmoji then
        "⭕"

    else
        "\\x1B[91m○" ++ Common.resetColor


restartingIndicator env =
    if env.terminalInfo.noColor then
        "◌"

    else if Common.supportsEmoji then
        "🔄"

    else
        "\\x1B[96m◌" ++ Common.resetColor


abortedIndicator env =
    if env.terminalInfo.noColor then
        "▲"

    else if Common.supportsEmoji then
        "⛔️"

    else
        "\\x1B[91m▲" ++ Common.resetColor


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


beginSyncUpdate =
    "\\x1B[?2026h"


endSyncUpdate =
    "\\x1B[?2026l"


disableAlternateScreen =
    "\\x1B[?1049l"


disableApplicationCursorKeys =
    "\\x1B[?1l"



-- https://www.vt100.net/docs/vt510-rm/DECCKM.html


enableMouse =
    "\\x1B[?1000;1006h"


cursorAbsolute ( y, x ) =
    "\\x1B[" ++ String.fromInt y ++ ";" ++ String.fromInt x ++ "H"


listAt : Int -> List a -> Maybe a
listAt index list =
    List.head (List.drop index list)



-- runNonInteractively : List Command -> Int -> ()


runNonInteractively commands maxParallel =
    Debug.todo "runNonInteractively"
