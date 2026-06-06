module Command exposing
    ( Command
    , Description
    , Status(..)
    , descriptionDecoder
    , toPresentationName
    )

import Ansi.Font
import Cli
import Common
import Dict exposing (Dict)
import Fs.Path
import Json.Decode
import Regex exposing (Regex)


type alias Command =
    { label : String
    , addHistoryStart : Bool
    , file : String
    , args : List String
    , cwd : String
    , killAllSequence : String
    , title : String
    , titlePossiblyWithGraphicRenditions : String
    , formattedCommandWithTitle : String

    -- onData: (data: string, statusFromRulesChanged: boolean) => undefined,
    , onData : ()

    -- onRequest: (data: string) => undefined,
    , onRequest : ()

    -- onSynchronizedOutputChange: (data: string) => undefined,
    , onSynchronizedOutputChange : ()

    -- onExit: (exitCode: number) => undefined,
    , onExit : ()
    , isSimpleLog : Bool
    , isOnAlternateScreen : Bool
    , status : Status
    , defaultStatus : Maybe ( String, String )
    , statusFromRules : Maybe String
    , windowsConptyCursorMoveWorkaround : Bool
    , historyAlternateScreen : String
    , unfinishedEscapeBuffer : String
    , statusRules : List ( Regex, String )
    , history : String
    }


type Status
    = Waiting
    | Running
    | Killing Bool
    | Exit Bool Int


type alias Description =
    { command : ( String, List String )
    , title : String
    , cwd : String
    , status : List ( Regex, String )
    , defaultStatus : Maybe ( String, String )
    , killAllSequence : String
    }


new :
    Cli.Env
    ->
        { label : Maybe String
        , addHistoryStart : Bool
        , commandDescription : Description
        , onData : ()
        , onRequest : ()
        , onSynchronizedOutputChange : ()
        , onExit : ()
        }
    -> Command
new env opts =
    let
        ( file, args ) =
            opts.commandDescription.command

        uncleanTitle =
            opts.commandDescription.title

        cleanTitle =
            Common.removeGraphicRenditions uncleanTitle

        formattedCommand =
            toPresentationName (file :: args)

        formattedCommandWithTitle =
            if uncleanTitle == formattedCommand then
                formattedCommand

            else if env.terminalInfo.noColor then
                cleanTitle ++ ": " ++ formattedCommand

            else
                Ansi.Font.bold uncleanTitle ++ ": " ++ formattedCommand
    in
    { label = Maybe.withDefault " " opts.label
    , addHistoryStart = opts.addHistoryStart
    , file = file
    , args = args
    , cwd = opts.commandDescription.cwd
    , killAllSequence = opts.commandDescription.killAllSequence
    , title = cleanTitle
    , titlePossiblyWithGraphicRenditions =
        if env.terminalInfo.noColor then
            cleanTitle

        else
            uncleanTitle
    , formattedCommandWithTitle = formattedCommandWithTitle
    , onData = opts.onData
    , onRequest = opts.onRequest
    , onSynchronizedOutputChange = opts.onSynchronizedOutputChange
    , onExit = opts.onExit
    , isSimpleLog = True
    , isOnAlternateScreen = False
    , status = Waiting
    , defaultStatus = opts.commandDescription.defaultStatus
    , windowsConptyCursorMoveWorkaround = False
    , historyAlternateScreen = ""
    , unfinishedEscapeBuffer = ""
    , statusFromRules = extractStatus env opts.commandDescription.defaultStatus
    , statusRules = opts.commandDescription.status

    -- When adding --auto-exit, Lydell first tried to always set `this.history = ""`
    -- and add `historyStart()` in `joinHistory`. However, that doesn’t work
    -- properly because `this.history` can be truncated based on `CLEAR_REGEX`
    -- and `MAX_HISTORY` – and that should include the `historyStart` bit.
    -- (We don’t want `historyStart` for --auto-exit.)
    , history =
        if opts.addHistoryStart then
            historyStart env (Common.waitingIndicator env) formattedCommandWithTitle opts.commandDescription.cwd

        else
            ""
    }


historyStart env indicator formattedCommandWithTitle cwd =
    commandTitleWithIndicator env indicator formattedCommandWithTitle ++ "\n" ++ cwdText env cwd


commandTitleWithIndicator env indicator formattedCommandWithTitle =
    let
        ( _, emojiWidthFix ) =
            iconEmojiFix env
    in
    indicator ++ emojiWidthFix ++ " " ++ formattedCommandWithTitle ++ Common.resetColor


cwdText env cwd =
    let
        ( _, emojiWidthFix ) =
            iconEmojiFix env
    in
    if Fs.Path.fromString cwd == Debug.todo "process.cwd" then
        ""

    else
        Common.folder env ++ emojiWidthFix ++ " " ++ Ansi.Font.faint cwd ++ "\n"


{-| ICON\_WIDTH, EMOJI\_WIDTH\_FIX
-}
iconEmojiFix : Cli.Env -> ( Int, String )
iconEmojiFix env =
    if not Common.supportsEmoji || env.terminalInfo.noColor then
        ( 1, "" )

    else
        ( 2, cursorHorizontalAbsolute 3 )


cursorHorizontalAbsolute n =
    "\\x1B[" ++ String.fromInt n ++ "G"


extractStatus : Cli.Env -> Maybe ( String, String ) -> Maybe String
extractStatus env status =
    case status of
        Nothing ->
            Nothing

        Just ( left, right ) ->
            Just <|
                if env.terminalInfo.noColor then
                    Common.removeGraphicRenditions right

                else if Common.supportsEmoji then
                    left

                else
                    right



-- class Command {
--   /**
--    * @param {{
--       label: string | undefined,
--       addHistoryStart: boolean,
--       commandDescription: CommandDescription,
--       onData: (data: string, statusFromRulesChanged: boolean) => undefined,
--       onRequest: (data: string) => undefined,
--       onSynchronizedOutputChange: (data: string) => undefined,
--       onExit: (exitCode: number) => undefined,
--      }} commandInit
--    */
--
--
--
--
--   }
--   /**
--    * @param {{ needsToWait: boolean }} options
--    * @returns {void}
--    */
--   start({ needsToWait }) {
--     if ("terminal" in this.status) {
--       throw new Error(
--         `Cannot start command because the command is ${this.status.tag} with pid ${this.status.terminal.pid} for: ${this.title}`,
--       );
--     }
--     this.history = this.addHistoryStart
--       ? historyStart(needsToWait ? waitingIndicator : runningIndicator, this)
--       : "";
--     this.historyAlternateScreen = "";
--     this.isSimpleLog = true;
--     this.isOnAlternateScreen = false;
--     this.statusFromRules = extractStatus(this.defaultStatus);
--     // See the comment for `CONPTY_CURSOR_MOVE`.
--     this.windowsConptyCursorMoveWorkaround = IS_WINDOWS;
--     if (needsToWait) {
--       this.status = { tag: "Waiting" };
--       return;
--     }
--     const [file, args] = IS_WINDOWS
--       ? [
--           "cmd.exe",
--           [
--             "/d",
--             "/s",
--             "/q",
--             "/c",
--             cmdEscapeMetaChars(this.file),
--             ...this.args.map(cmdEscapeArg),
--           ].join(" "),
--         ]
--       : [this.file, this.args];
--     const terminal = pty.spawn(file, args, {
--       cwd: path.resolve(this.cwd),
--       cols: process.stdout.columns,
--       rows: process.stdout.rows,
--       // Avoid conpty adding escape sequences to clear the screen:
--       conptyInheritCursor: true,
--     });
--     const disposeOnData = terminal.onData((rawData) => {
--       const rawDataWithBuffer = this.unfinishedEscapeBuffer + rawData;
--       const match = UNFINISHED_ESCAPE.exec(rawDataWithBuffer);
--       const [data, unfinishedEscapeBuffer] =
--         match === null
--           ? [rawDataWithBuffer, ""]
--           : [rawDataWithBuffer.slice(0, match.index), match[0]];
--       this.unfinishedEscapeBuffer = unfinishedEscapeBuffer;
--       for (const [index, rawPart] of data.split(ESCAPES_REQUEST).entries()) {
--         let part = rawPart;
--         if (
--           this.windowsConptyCursorMoveWorkaround &&
--           CONPTY_CURSOR_MOVE.test(rawPart)
--         ) {
--           part = rawPart.replace(
--             CONPTY_CURSOR_MOVE,
--             CONPTY_CURSOR_MOVE_REPLACEMENT,
--           );
--           this.windowsConptyCursorMoveWorkaround = false;
--         }
--         if (index % 2 === 0) {
--           if (part !== "") {
--             const statusFromRulesChanged = this.pushHistory(part);
--             this.onData(part, statusFromRulesChanged);
--           }
--         } else if (part === BEGIN_SYNC_UPDATE || part === END_SYNC_UPDATE) {
--           this.onSynchronizedOutputChange(part);
--         } else {
--           this.onRequest(part);
--         }
--       }
--     });
--     const disposeOnExit = terminal.onExit(
--       ({ exitCode: actualExitCode, signal }) => {
--         disposeOnData.dispose();
--         disposeOnExit.dispose();
--         // There’s a convention to use 128 + signal for the exit code when a
--         // process is killed by a signal.
--         const exitCode =
--           signal === undefined || signal === 0 ? actualExitCode : 128 + signal;
--         const previousStatus = this.status;
--         this.status = {
--           tag: "Exit",
--           exitCode,
--           wasKilled: this.status.tag === "Killing",
--         };
--         if (
--           previousStatus.tag === "Killing" &&
--           previousStatus.restartAfterKill
--         ) {
--           this.start({ needsToWait: false });
--         }
--         this.onExit(exitCode);
--       },
--     );
--     this.status = { tag: "Running", terminal };
--   }
--   /**
--    * @params {{ restartAfterKill?: boolean }} options
--    * @returns {undefined}
--    */
--   kill({ restartAfterKill = false } = {}) {
--     switch (this.status.tag) {
--       case "Running":
--         this.status = {
--           tag: "Killing",
--           terminal: this.status.terminal,
--           slow: false,
--           lastKillPress: undefined,
--           restartAfterKill,
--         };
--         setTimeout(() => {
--           if (this.status.tag === "Killing") {
--             this.status.slow = true;
--             // Ugly way to redraw:
--             this.onData("", false);
--           }
--         }, SLOW_KILL);
--         this.status.terminal.write(this.killAllSequence);
--         return undefined;
--       case "Killing": {
--         const now = Date.now();
--         if (
--           this.status.lastKillPress !== undefined &&
--           now - this.status.lastKillPress <= DOUBLE_PRESS
--         ) {
--           if (IS_WINDOWS) {
--             this.status.terminal.kill();
--           } else {
--             this.status.terminal.kill("SIGKILL");
--           }
--         } else {
--           this.status.terminal.write(this.killAllSequence);
--         }
--         this.status.lastKillPress = now;
--         this.status.restartAfterKill = restartAfterKill;
--         return undefined;
--       }
--       case "Waiting":
--       case "Exit":
--         throw new Error(
--           `Cannot kill ${this.status.tag} pty for: ${this.title}`,
--         );
--     }
--   }
--   /**
--    * @param {string} data
--    * @returns {boolean}
--    */
--   pushHistory(data) {
--     const previousStatusFromRules = this.statusFromRules;
--     for (const part of data.split(ALTERNATE_SCREEN_REGEX)) {
--       switch (part) {
--         case ENABLE_ALTERNATE_SCREEN:
--           this.isOnAlternateScreen = true;
--           break;
--         case DISABLE_ALTERNATE_SCREEN:
--           this.isOnAlternateScreen = false;
--           break;
--         default:
--           this.updateStatusFromRules(part);
--           if (this.isOnAlternateScreen) {
--             this.historyAlternateScreen += part;
--             if (CLEAR_REGEX.test(this.historyAlternateScreen)) {
--               this.historyAlternateScreen = "";
--             } else {
--               if (this.historyAlternateScreen.length > MAX_HISTORY) {
--                 this.historyAlternateScreen =
--                   this.historyAlternateScreen.slice(-MAX_HISTORY);
--               }
--             }
--           } else {
--             this.history += part;
--             // Take one extra character so `NOT_SIMPLE_LOG_ESCAPE` can match the
--             // `\n${CURSOR_UP}` pattern.
--             const matches = this.history
--               .slice(-part.length - 1)
--               .matchAll(NOT_SIMPLE_LOG_ESCAPE);
--             for (const match of matches) {
--               const clearAll = match[1] !== undefined;
--               const clearDown = match[2] !== undefined;
--               if (clearAll) {
--                 this.history = match.input.slice(match.index + match[0].length);
--                 this.isSimpleLog = true;
--               } else {
--                 this.isSimpleLog = clearDown;
--               }
--             }
--             if (this.history.length > MAX_HISTORY) {
--               this.history = this.history.slice(-MAX_HISTORY);
--             }
--           }
--       }
--     }
--     const statusFromRulesChanged =
--       this.statusFromRules !== previousStatusFromRules;
--     return statusFromRulesChanged;
--   }
--   /**
--    * @param {string} data
--    * @returns {void}
--    */
--   updateStatusFromRules(data) {
--     const lastLine = getLastLine(
--       this.isOnAlternateScreen ? this.historyAlternateScreen : this.history,
--     );
--     const lines = (lastLine + data).split(/(?:\r?\n|\r)/);
--     for (const line of lines) {
--       for (const [regex, status] of this.statusRules) {
--         if (regex.test(removeGraphicRenditions(line))) {
--           this.statusFromRules = extractStatus(status);
--         }
--       }
--     }
--   }
-- }
--
--
--
--
--
--
--
--
--
--
--


descriptionDecoder : Json.Decode.Decoder Description
descriptionDecoder =
    Json.Decode.field "command" (decodeNonEmptyList Json.Decode.string)
        |> Json.Decode.andThen
            (\(( file, args ) as command) ->
                Json.Decode.map5 (Description command)
                    (Json.Decode.maybe (Json.Decode.field "title" Json.Decode.string)
                        |> Json.Decode.map (Maybe.withDefault (toPresentationName (file :: args)))
                    )
                    (Json.Decode.maybe (Json.Decode.field "cwd" Json.Decode.string)
                        |> Json.Decode.map (Maybe.withDefault ".")
                    )
                    (Json.Decode.maybe (Json.Decode.field "status" decodeStatuses)
                        |> Json.Decode.map (Maybe.withDefault [])
                    )
                    (Json.Decode.maybe (Json.Decode.field "defaultStatus" decodeStatus)
                        |> Json.Decode.map (Maybe.andThen identity)
                    )
                    (Json.Decode.maybe (Json.Decode.field "killAllSequence" Json.Decode.string)
                        |> Json.Decode.map (Maybe.withDefault Common.keyCodes.kill)
                    )
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



--


{-| -}
toPresentationName : List String -> String
toPresentationName command =
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
