module Main exposing (main)

import Ansi.Color
import Ansi.Font
import Capabilities
import Cli
import Dict exposing (Dict)
import Fs
import Fs.Location
import Fs.Path
import Json.Decode
import Regex exposing (Regex)
import Task exposing (Task)


main : Cli.Program Model Msg
main =
    Cli.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        }


type Model
    = Done
    | Parsing Cli.Env Fs.FileSystem
    | Running RunningModel


type alias RunningModel =
    { stdout : Capabilities.Console
    , stderr : Capabilities.Console
    }


init : Cli.Env -> ( Model, Cmd Msg )
init env =
    case Fs.require env of
        Err _ ->
            ( Done
            , Cli.dieCmd env 1 (filesystemAccessText env.programName)
            )

        Ok fs ->
            ( Parsing env fs
            , parseArgs fs env.args
                |> Task.attempt ArgsParsed
            )


parseArgs : Fs.FileSystem -> List String -> Task Fs.FsError StartTag
parseArgs fs args =
    case args of
        [] ->
            Task.succeed Help

        _ ->
            let
                ( flags, restArgs ) =
                    partitionArgs args
            in
            case forEachFlag flags of
                AutoExit cpuCount ->
                    parseRestArgs fs (Just cpuCount) restArgs

                NoAutoExit ->
                    parseRestArgs fs Nothing restArgs

                tag ->
                    Task.succeed tag


parseRestArgs : Fs.FileSystem -> Maybe Int -> List String -> Task Fs.FsError StartTag
parseRestArgs fs autoExit args =
    case args of
        [] ->
            Debug.todo ""

        [ configPath ] ->
            Fs.readTextFile fs (Fs.Location.file configPath)
                |> Task.andThen (parseInputFile autoExit)

        delimiter :: rest ->
            gatherCommands autoExit delimiter rest


gatherCommands : Maybe Int -> String -> List String -> Task Fs.FsError StartTag
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
                Task.succeed <|
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
                                                    { title = Just (commandToPresentationName command)
                                                    , cwd = Just "."
                                                    , command = ( cmd, rest )
                                                    , status = []
                                                    , defaultStatus = Nothing
                                                    , killAllSequence = Just keyCodes.kill
                                                    }
                                    )
                                    commands
                                )
                                autoExit
           )


commandToPresentationName : List String -> String
commandToPresentationName command =
    command
        |> List.map partToPresentationName
        |> String.join " "


partToPresentationName : String -> String
partToPresentationName part =
    if part == "" then
        "''"

    else
        part
            |> splitOnQuote
            |> List.map subPartToPresentationName
            |> String.join ""


subPartToPresentationName : String -> String
subPartToPresentationName subPart =
    if subPart == "" then
        ""

    else if subPart == "'" then
        "\\'"

    else if isUnquoted subPart then
        subPart

    else
        "'" ++ subPart ++ "'"


isUnquoted : String -> Bool
isUnquoted subPart =
    case Regex.fromString "^[\\w.,:/=@%+-]+$" of
        Nothing ->
            False

        Just regex ->
            Regex.contains regex subPart


splitOnQuote : String -> List String
splitOnQuote part =
    case Regex.fromString "(')" of
        Nothing ->
            [ part ]

        Just regex ->
            Regex.split regex part


keyCodes =
    { kill = "\u{0003}"
    , restart = "\u{000D}"
    , dashboard = "\u{001A}"
    , up = "\u{001B}[A"
    , down = "\u{001B}[B"
    , left = "\u{001B}[D"
    , right = "\u{001B}[C"
    , enter = "\u{000D}"
    , esc = "\u{001B}"
    }


parseInputFile : Maybe Int -> String -> Task Fs.FsError StartTag
parseInputFile autoExit config =
    case Json.Decode.decodeString configDecoder config of
        Err _ ->
            Debug.todo ""

        Ok [] ->
            Task.succeed NoCommands

        Ok commands ->
            Task.succeed (Parsed commands autoExit)


configDecoder : Json.Decode.Decoder (List Command)
configDecoder =
    Json.Decode.list
        (Json.Decode.map6 Command
            (Json.Decode.field "command" (decodeNonEmptyList Json.Decode.string))
            (Json.Decode.maybe (Json.Decode.field "title" Json.Decode.string))
            (Json.Decode.maybe (Json.Decode.field "cwd" Json.Decode.string))
            (Json.Decode.maybe (Json.Decode.field "status" decodeStatuses)
                |> Json.Decode.map (Maybe.withDefault [])
            )
            (Json.Decode.maybe (Json.Decode.field "defaultStatus" decodeStatus)
                |> Json.Decode.map (Maybe.andThen identity)
            )
            (Json.Decode.maybe (Json.Decode.field "killAllSequence" Json.Decode.string))
        )


decodeStatuses : Json.Decode.Decoder (List ( Regex, String ))
decodeStatuses =
    Json.Decode.dict Json.Decode.string
        |> Json.Decode.andThen
            (\dict ->
                case dictToRegexPairs dict of
                    Ok pairs ->
                        Json.Decode.succeed pairs

                    Err key ->
                        Json.Decode.fail ("Invalid regex: " ++ key)
            )


dictToRegexPairs : Dict String String -> Result String (List ( Regex, String ))
dictToRegexPairs dict =
    dictToRegexPairsHelper (Dict.toList dict) []


dictToRegexPairsHelper : List ( String, String ) -> List ( Regex, String ) -> Result String (List ( Regex, String ))
dictToRegexPairsHelper todo done =
    case todo of
        [] ->
            Ok done

        ( key, value ) :: rest ->
            case Regex.fromString key of
                Nothing ->
                    Err key

                Just keyReg ->
                    dictToRegexPairsHelper rest (( keyReg, value ) :: done)


decodeStatus : Json.Decode.Decoder (Maybe ( String, String ))
decodeStatus =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.andThen
            (\list ->
                case list of
                    [ a, b ] ->
                        Json.Decode.succeed ( a, b )

                    _ ->
                        Json.Decode.fail "Expected a tuple of statuses"
            )
        |> Json.Decode.nullable


decodeNonEmptyList : Json.Decode.Decoder a -> Json.Decode.Decoder ( a, List a )
decodeNonEmptyList decoder =
    Json.Decode.list decoder
        |> Json.Decode.andThen
            (\list ->
                case list of
                    [] ->
                        Json.Decode.fail "Expected a non-empty list"

                    a :: rest ->
                        Json.Decode.succeed ( a, rest )
            )


type alias Command =
    { command : ( String, List String )
    , title : Maybe String
    , cwd : Maybe String
    , status : List ( Regex, String )
    , defaultStatus : Maybe ( String, String )
    , killAllSequence : Maybe String
    }


forEachFlag : List String -> StartTag
forEachFlag flags =
    case flags of
        [] ->
            Debug.todo ""

        flag :: restFlags ->
            if flag == "-h" || flag == "--help" then
                Help

            else
                case Regex.find autoExitRegex flag of
                    [ { submatches } ] ->
                        case submatches of
                            [ Nothing ] ->
                                AutoExit (Debug.todo "os.cpus.length")

                            [ Just "auto" ] ->
                                AutoExit (Debug.todo "os.cpus.length")

                            [ Just "0" ] ->
                                Error "--auto-exit=0 will never finish."

                            [ Just numStr ] ->
                                case String.toInt numStr of
                                    Nothing ->
                                        Error ("Bad flag: " ++ flag ++ "\nOnly these forms are accepted:\n" ++ autoExitHelp)

                                    Just cpuCount ->
                                        AutoExit cpuCount

                            _ ->
                                Error ("Bad flag: " ++ flag ++ "\nOnly these forms are accepted:\n" ++ autoExitHelp)

                    _ ->
                        Error ("Bad flag: " ++ flag ++ "\nOnly these forms are accepted:\n" ++ autoExitHelp)


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
    = Help
    | Error String
    | AutoExit Int
    | NoAutoExit
    | NoCommands
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
    = ArgsParsed (Result Fs.FsError StartTag)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Parsing env fs, ArgsParsed (Err err) ) ->
            ( Done
            , Debug.todo ""
            )

        ( Parsing env fs, ArgsParsed (Ok tag) ) ->
            case tag of
                Help ->
                    ( Done
                    , Cli.dieCmd env 0 helpText
                    )

                Error message ->
                    ( Done
                    , Cli.dieCmd env 1 message
                    )

                AutoExit cpuCount ->
                    Debug.todo ""

                NoAutoExit ->
                    Debug.todo ""

                NoCommands ->
                    ( Done
                    , Cli.exit 0
                    )

                Parsed commands autoExit ->
                    if env.terminalInfo.stdinIsTerminal then
                        runInteractively commands autoExit

                    else
                        case autoExit of
                            Just maxParallel ->
                                runNonInteractively commands maxParallel

                            Nothing ->
                                ( Done
                                , Cli.dieCmd env 1 "run-pty requires stdin to be a TTY to run properly (unless --auto-exit is used)."
                                )

        _ ->
            ( model, Cmd.none )



-- runInteractively : List Command -> Maybe Int -> ()


runInteractively commands autoExit =
    Debug.todo ""



-- runNonInteractively : List Command -> Int -> ()


runNonInteractively commands maxParallel =
    Debug.todo ""
