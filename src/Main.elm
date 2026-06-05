module Main exposing (main)

import Ansi.Color
import Ansi.Font
import Capabilities
import Cli
import Fs
import Fs.Location
import Task


main : Cli.Program Model Msg
main =
    Cli.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        }


type Model
    = Done
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
            case parseArgs env.args of
                Help ->
                    ( Done
                    , Cli.dieCmd env 0 helpText
                    )


parseArgs : List String -> StartTag
parseArgs args =
    case args of
        [] ->
            Help

        _ ->
            Help


type StartTag
    = Help


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
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
