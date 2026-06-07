module Common exposing
    ( cursorHorizontalAbsolute
    , folder
    , graphicRenditions
    , iconEmojiFix
    , keyCodes
    , removeGraphicRenditions
    , resetColor
    , supportsEmoji
    , waitingIndicator
    )

import Cli
import Regex exposing (Regex)


{-| -}
removeGraphicRenditions : String -> String
removeGraphicRenditions =
    Regex.replace graphicRenditions (\_ -> "")


graphicRenditions : Regex
graphicRenditions =
    Regex.fromString "(\\x1B\\[(?:\\d+(?:;\\d+)*)?m)"
        |> Maybe.withDefault Regex.never


resetColor : String
resetColor =
    "\\x1B[m"


supportsEmoji =
    -- not IS_WINDOWS || IS_WINDOWS_TERMINAL
    Debug.todo "supportsEmoji"


waitingIndicator : Cli.Env -> String
waitingIndicator env =
    if env.terminalInfo.noColor then
        "■"

    else if supportsEmoji then
        "🥱"

    else
        "\\x1B[93m■" ++ resetColor


folder : Cli.Env -> String
folder env =
    if env.terminalInfo.noColor then
        "⌂"

    else if supportsEmoji then
        "📂"

    else
        "\\x1B[2m⌂" ++ resetColor


{-| ICON\_WIDTH, EMOJI\_WIDTH\_FIX
-}
iconEmojiFix : Cli.Env -> ( Int, String )
iconEmojiFix env =
    if not supportsEmoji || env.terminalInfo.noColor then
        ( 1, "" )

    else
        ( 2, cursorHorizontalAbsolute 3 )


cursorHorizontalAbsolute : Int -> String
cursorHorizontalAbsolute n =
    "\\x1B[" ++ String.fromInt n ++ "G"


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
