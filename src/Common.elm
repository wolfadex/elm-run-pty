module Common exposing
    ( folder
    , keyCodes
    , removeGraphicRenditions
    , resetColor
    , supportsEmoji
    , waitingIndicator
    )

import Regex exposing (Regex)


{-| -}
removeGraphicRenditions : String -> String
removeGraphicRenditions =
    Regex.replace graphicRenditions (\_ -> "")


graphicRenditions =
    Regex.fromString "(\\x1B\\[(?:\\d+(?:;\\d+)*)?m)"
        |> Maybe.withDefault Regex.never


resetColor =
    "\\x1B[m"


supportsEmoji =
    -- not IS_WINDOWS || IS_WINDOWS_TERMINAL
    Debug.todo "supportsEmoji"


waitingIndicator env =
    if env.terminalInfo.noColor then
        "■"

    else if supportsEmoji then
        "🥱"

    else
        "\\x1B[93m■" ++ resetColor


folder env =
    if env.terminalInfo.noColor then
        "⌂"

    else if supportsEmoji then
        "📂"

    else
        "\\x1B[2m⌂" ++ resetColor


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
