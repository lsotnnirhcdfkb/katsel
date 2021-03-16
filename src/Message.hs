module Message
    ( Section(SimpleText, TreeSection)
    , makeUnderlinesSection
    , SimpleDiagType(..)
    , SimpleDiag(..)
    , ToDiagnostic
    , report
    , toDiagnostic
    , makeCode
    , UnderlineImportance(..)
    , UnderlineType(..)
    , UnderlineMessage(..)
    ) where

import Location
import File

import Data.List(nub, sortBy, foldl', find)

import qualified System.Console.ANSI as ANSI

boldSGR :: ANSI.SGR
boldSGR = ANSI.SetConsoleIntensity ANSI.BoldIntensity

vividForeColorSGR :: ANSI.Color -> ANSI.SGR
vividForeColorSGR = ANSI.SetColor ANSI.Foreground ANSI.Vivid

filePathSGR :: [ANSI.SGR]
filePathSGR = [boldSGR, vividForeColorSGR ANSI.Cyan]

data UnderlineImportance = Primary | Secondary | Tertiary
data UnderlineType = ErrorUnderline | WarningUnderline | NoteUnderline | HintUnderline
data UnderlineMessage = UnderlineMessage Span UnderlineType UnderlineImportance String

locMinus1 :: Location -> Location
locMinus1 loc = makeLocation (fileOfLoc loc) (indOfLoc loc - 1)

lineOfMinus1 :: Location -> Int
lineOfMinus1 loc = lnnOfLoc $ locMinus1 loc
colOfMinus1 :: Location -> Int
colOfMinus1 loc = colnOfLoc $ locMinus1 loc

makeUnderlinesSection :: [UnderlineMessage] -> Section
makeUnderlinesSection msgs = Underlines msgs lineNumbers
    where
        lineNumbers = sortBy comparator $ nub $ concatMap linenrsof msgs
        linenrsof (UnderlineMessage (Span start end) _ _ _) = [(fileOfLoc start, lnnOfLoc start), (fileOfLoc start, lineOfMinus1 end)]

        comparator (fl1, nr1) (fl2, nr2) =
            if fl1 == fl2
            then nr1 `compare` nr2
            else name fl1 `compare` name fl2

data Section
    = SimpleText String
    | Underlines [UnderlineMessage] [(File, Int)]
    | TreeSection (Maybe String) [(Maybe String, Section)]

data SimpleDiagType
    = Error
    | Warning
    | DebugMessage

instance Show SimpleDiagType where
    show Error = "error"
    show Warning = "warning"
    show DebugMessage = "debug message"

sgrOfDiagType :: SimpleDiagType -> [ANSI.SGR]
sgrOfDiagType Error = [boldSGR, vividForeColorSGR ANSI.Red]
sgrOfDiagType Warning = [boldSGR, vividForeColorSGR ANSI.Magenta]
sgrOfDiagType DebugMessage = [boldSGR, vividForeColorSGR ANSI.Green]

newtype DiagCode = DiagCode String

makeCode :: String -> Maybe DiagCode
makeCode str = Just $ DiagCode str

data SimpleDiag = SimpleDiag SimpleDiagType (Maybe Span) (Maybe DiagCode) (Maybe String) [Section]

class ToDiagnostic e where
    toDiagnostic :: e -> SimpleDiag
instance ToDiagnostic SimpleDiag where
    toDiagnostic = id

report :: (ToDiagnostic e) => e -> String
report = report' . toDiagnostic

report' :: SimpleDiag -> String
report' (SimpleDiag ty maybeSpan maybeDiagCode maybeName sections) =
    header ++ "\n" ++
    shownSections ++
    footer
    where
        header =
            ANSI.setSGRCode (sgrOfDiagType ty) ++ show ty ++ ANSI.setSGRCode [] ++
            (case maybeSpan of
                Just sp -> " at " ++ ANSI.setSGRCode filePathSGR ++ fmtSpan sp ++ ANSI.setSGRCode []
                Nothing -> ""
            ) ++ ":"

        footer =
            case (maybeDiagCode, maybeName) of
                (Just (DiagCode diagCode), Just diagName) -> prefix ++ diagCodeFmt diagCode ++ ": " ++ diagName ++ "\n"
                (Nothing                 , Just diagName) -> prefix ++ diagName ++ "\n"
                (Just (DiagCode diagCode), Nothing      ) -> prefix ++ diagCodeFmt diagCode ++ "\n"

                _ -> ""
            where
                diagCodeFmt code = "[" ++ ANSI.setSGRCode [boldSGR] ++ code ++ ANSI.setSGRCode [] ++ "]"
                prefix = indentStr ++ "==> "

        shownSections = concat $ map (showSection indentAmt) sections

        indentAmt = maximum $ map indentOf sections
        indentStr = makeIndentStr indentAmt

indentOf :: Section -> Int
indentOf (SimpleText _) = 4
indentOf (Underlines _ fllnnrs) = 1 + (maximum $ map (length . show . snd) fllnnrs)
indentOf (TreeSection _ _) = 0

showSection :: Int -> Section -> String
showSection indent (SimpleText text) = makeIndentStr indent ++ " " ++ text ++ "\n"
-- show Underlines {{{
showSection indent (Underlines msgs linenrs) =
    foldl' concatLine "" $ zip linenrs (Nothing:(map Just linenrs))
    where
        showLine flnr@(fl, nr) = quoteLine ++ firstRow ++ nextRows
            where
                quote = case drop (nr - 1) $ lines (source fl) of
                    x:_ -> x
                    [] -> ""
                quoteLine = makeIndentWithDivider '|' (show nr) indent ++ quote ++ "\n"

                lineMessages = filter isCorrectLine msgs
                    where
                        isCorrectLine (UnderlineMessage (Span _ msgloc) _ _ _) = (fileOfLoc msgloc, lineOfMinus1 msgloc) == flnr

                lineUnderlines = filter isCorrectLine msgs
                    where
                        isCorrectLine (UnderlineMessage (Span start end) _ _ _) =
                            fl == fileOfLoc start && lnnOfLoc start <= nr && nr <= lineOfMinus1 end

                colorOfType ErrorUnderline = [boldSGR, vividForeColorSGR ANSI.Red]
                colorOfType WarningUnderline = [boldSGR, vividForeColorSGR ANSI.Magenta]
                colorOfType NoteUnderline = [boldSGR, vividForeColorSGR ANSI.Green]
                colorOfType HintUnderline = [boldSGR, vividForeColorSGR ANSI.Blue]

                assignedMessages =
                    assignMessages (sortBy (\ (UnderlineMessage (Span _ msg1l) _ _ _) (UnderlineMessage (Span _ msg2l) _ _ _) -> (colOfMinus1 msg1l) `compare` (colOfMinus1 msg2l)) lineMessages) []
                    where
                        assignMessages [] a = a
                        assignMessages (toAssign:rest) alreadyAssigned =
                            let assignment = assignRowNum toAssign 0 alreadyAssigned
                                restAssignments = assignMessages rest $ assignment:alreadyAssigned
                            in restAssignments

                        assignRowNum :: UnderlineMessage -> Int -> [(Int, Int, Int, String)] -> (Int, Int, Int, String)
                        assignRowNum msg currow already =
                            if overlapping
                                then assignRowNum msg (currow + 1) already
                                else makeAssignment msg currow

                            where
                                msgEndCol = endcolumn currow msg

                                assignmentsOnCurRow = filter ((currow==) . rowOfAssignment) already
                                overlapping = any ((msgEndCol>=) . startColOfAssignment) assignmentsOnCurRow

                                startColOfAssignment (_, c, _, _) = c
                                rowOfAssignment (r, _, _, _) = r

                        makeAssignment msg@(UnderlineMessage _ ty _ _) row =
                            (row, startcolumn row msg, endcolumn row msg, ANSI.setSGRCode (colorOfType ty) ++ augmentedMessage row msg ++ ANSI.setSGRCode [])

                        startcolumn row (UnderlineMessage (Span _ eloc) _ _ _) = (colOfMinus1 eloc) + if row == 0 then 1 else 0
                        endcolumn row msg = startcolumn row msg + length (augmentedMessage row msg)
                        augmentedMessage row (UnderlineMessage _ _ _ msgText) = (if row == 0 then "-- " else "`-- ") ++ msgText

                underlineLinePrefix = makeIndentWithDivider '|' "" indent

                underlinesForChars = map getUnderlineForChar $ take (length quote + 1) [1..]
                    where
                        getUnderlineForChar coln = find (\ msg -> startcol msg <= coln && coln < endcol msg) lineUnderlines
                        startcol (UnderlineMessage (Span start _) _ _ _) =
                            if lnnOfLoc start == nr
                            then colnOfLoc start
                            else 1
                        endcol (UnderlineMessage (Span _ end) _ _ _) =
                            if lnnOfLoc end == nr
                            then colnOfLoc end
                            else maxBound

                shownUnderlines = map drawUnderline underlinesForChars
                    where
                        drawUnderline Nothing = " "
                        drawUnderline (Just (UnderlineMessage _ ty importance _)) = ANSI.setSGRCode (colorOfType ty) ++ [charOfImportance importance] ++ ANSI.setSGRCode []

                        charOfImportance Primary = '^'
                        charOfImportance Secondary = '~'
                        charOfImportance Tertiary = '.'

                firstRow =
                    if length lineUnderlines > 0
                    then underlineLinePrefix ++ showRow 0 ++ "\n"
                    else ""

                showRow row =
                    let rowMessages = filter (\ (r, _, _, _) -> r == row) assignedMessages
                        maxCol = maximum $ (length shownUnderlines) : (map (\ (_, _, ec, _) -> ec) rowMessages)
                        rowUnderline = if row == 0 then shownUnderlines else []
                    in putMsgs rowMessages 1 maxCol rowUnderline ""

                putMsgs :: [(Int, Int, Int, String)] -> Int -> Int -> [String] -> String -> String
                putMsgs [] _ _ [] acc = acc
                putMsgs rowMessages col maxCol underline acc
                    | col > maxCol = acc
                    | otherwise =
                        let curmsgs = filter (\ (_, c, _, _) -> c == col) rowMessages
                            nextunderline = drop 1 underline
                        in case curmsgs of
                            [] ->
                                let curch =
                                        case underline of
                                            (u:_) -> u
                                            [] -> " "
                                in putMsgs rowMessages (col + 1) maxCol nextunderline (acc ++ curch)
                            [(_, _, endcol, text)] -> putMsgs rowMessages endcol maxCol nextunderline $ acc ++ text
                            _ -> error "multiple messages on same row, same column"

                nextRows = concat $ map (\ ln -> underlineLinePrefix ++ ln ++ "\n") $ takeWhile ((>0) . length) $ map showRow [1..]

        showFileLine fl = makeIndentWithDivider '>' "" indent ++ ANSI.setSGRCode filePathSGR ++ name fl ++ ANSI.setSGRCode [] ++ "\n"
        showElipsisLine = makeIndentWithDivider '|' (replicate (indent - 1) '.') indent ++ "...\n"

        concatLine acc (flln@(fl, curlnr), maybeLastFileNr) =
            let needFileLine = case maybeLastFileNr of
                    Just (lastfl, _) | lastfl == fl -> False
                    _ -> True
                fileLine = if needFileLine then showFileLine fl else ""

                needElipsisLine =
                    if needFileLine
                    then False
                    else
                        case maybeLastFileNr of
                            Just (_, lastnr) | lastnr + 1 /= curlnr -> True
                            _ -> False
                elipsisLine = if needElipsisLine then showElipsisLine else ""

            in acc ++ fileLine ++ elipsisLine ++ showLine flln
-- }}}
showSection indent (TreeSection heading sections) =
    headingStr ++
    sectionsStr
    where
        headingStr = case heading of
            Just s -> makeIndentWithDivider '|' "" indent ++ s ++ "\n"
            Nothing -> ""

        indentChild = unlines . (map ((makeIndentWithDivider '|' "" indent ++ "    ")++)) . lines

        sectionsStr = concat $ map (showChild) sections

        showChild (sectionHeading, section) =
            let h = case sectionHeading of
                        Just x -> makeIndentWithDivider '|' "" indent ++ x ++ "\n"
                        Nothing -> ""
            in h ++ (indentChild $ showSection (indentOf section) section)

makeIndentWithDivider :: Char -> String -> Int -> String
makeIndentWithDivider divider left indent =
    (replicate (indent - length left - 1) ' ') ++ left ++ " " ++ [divider] ++ " "

makeIndentStr :: Int -> String
makeIndentStr x = replicate x ' '
