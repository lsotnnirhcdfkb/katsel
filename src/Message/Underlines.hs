module Message.Underlines
    ( UnderlinesSection(..)
    , Message(..)
    , Importance(..)
    , Type(..)
    , showUnderlinesSection
    , indentOfUnderlinesSection
    ) where

import Location
import File

import Message.Utils

import Data.List(nubBy, sortBy, partition, find)

import Data.Maybe(maybeToList)

import qualified System.Console.ANSI as ANSI

data UnderlinesSection = UnderlinesSection [Message]
data Message = Message Span Type Importance String
data Importance = Primary | Secondary | Tertiary
data Type = Error | Warning | Note | Hint

locMinus1 :: Location -> Location
locMinus1 loc = makeLocation (fileOfLoc loc) (indOfLoc loc - 1)
lineMinus1 :: Location -> Int
lineMinus1 loc = lnnOfLoc $ locMinus1 loc
colMinus1 :: Location -> Int
colMinus1 loc = colnOfLoc $ locMinus1 loc

data ShowLine = ShowLine File Int Dimness
data Dimness = Dim | Normal

linenrsOfMessages :: [Message] -> [ShowLine]
linenrsOfMessages msgs = sortBy sortComparator $ nubBy nubComparator linesWithDim
    where
        -- nub keeps the first occurance, so it will keep all the normal lines if there are duplicate dim lines
        -- since the dim lines are all appended to the end of the list
        linesWithDim = linesWithoutDim ++ concatMap getDimLines linesWithoutDim
        getDimLines (ShowLine fl nr _) = (ShowLine fl (nr+1) Dim):(if nr > 1 then [ShowLine fl (nr-1) Dim] else [])

        linesWithoutDim = concatMap linenrsof msgs
        linenrsof (Message (Span start end) _ _ _) = [ShowLine (fileOfLoc start) (lnnOfLoc start) Normal, ShowLine (fileOfLoc start) (lineMinus1 end) Normal]

        sortComparator (ShowLine fl1 nr1 _) (ShowLine fl2 nr2 _) =
            if fl1 == fl2
            then nr1 `compare` nr2
            else name fl1 `compare` name fl2

        nubComparator (ShowLine fl1 nr1 _) (ShowLine fl2 nr2 _) = (fl1, nr1) == (fl2, nr2)

indentOfUnderlinesSection :: UnderlinesSection -> Int
indentOfUnderlinesSection (UnderlinesSection msgs) = 1 + (maximum $ map getWidth $ linenrsOfMessages msgs)
    where
        getWidth (ShowLine _ ln _) = length . show $ ln

showUnderlinesSection :: Int -> UnderlinesSection -> String
showUnderlinesSection indent sec =
    concatMap (drawSectionLine indent) $ sectionLines sec

data DrawableMessage = DMessage [ANSI.SGR] Int String
data SectionLine
    = FileLine File
    | QuoteLine File Int
    | DimQuote File Int
    | ElipsisLine
    | UnderlineLine [Message] [DrawableMessage]

    -- TODO: MessageLine needs to draw a pipe over messages on rows below
    --       to do:
    --       > quote a
    --       > |     `-- this
    --       > `-- that
    --
    --       instead of:
    --       > quote a
    --       >       `-- this
    --       > `-- that
    | MessageLine [DrawableMessage]

assignMessages :: [Message] -> ([DrawableMessage], [SectionLine])
assignMessages messages = (firstrow, msglines)
    where
        assign [] already = already
        assign (toAssign:rest) already =
            let newAssignment = tryAssign toAssign (0 :: Int) already
            in assign rest $ newAssignment:already

        tryAssign curMsg currow already =
            if overlapping
            then tryAssign curMsg (currow + 1) already
            else (currow, curMsg)
            where
                onCurRow = filter ((currow==) . fst) already

                overlapping = any ((curMsgEndCol>=) . colOfAssignment) onCurRow

                curMsgEndCol = endColOfMsg curMsg
                colOfAssignment (_, (Message (Span _ eloc) _ _ _)) =  colMinus1 eloc
                endColOfMsg (Message (Span _ end) _ _ str) = colMinus1 end + length str + 3

        assigned = assign (sortBy comparator messages) []
        comparator (Message (Span _ end1) _ _ _) (Message (Span _ end2) _ _ _) = (colMinus1 end2) `compare` (colMinus1 end1)

        firstrow = findMsgsOnRow 0

        msglines = map MessageLine $ takeWhile ((>0) . length) $ map findMsgsOnRow [1..]
        findMsgsOnRow row = map (todmsg . snd) $ filter ((row==) . fst) assigned

        todmsg (Message (Span _ end) ty _ str) = DMessage (sgrOfTy ty) (colMinus1 end) str

sectionLines :: UnderlinesSection -> [SectionLine]
sectionLines (UnderlinesSection msgs) =
    makeLines [] $ zip flnrs $ Nothing:(map Just flnrs)
    where
        flnrs = linenrsOfMessages msgs

        makeLines acc (((ShowLine curfl curnr curdimn), lastshln):more) =
            makeLines nextAcc more
            where
                nextAcc = acc ++ maybeToList fileLine ++ maybeToList elipsisLine ++ contentLines
                    where
                        contentLines =
                            case curdimn of
                                Dim -> [DimQuote curfl curnr]
                                Normal -> [quoteLine] ++ maybeToList underlineLine ++ messageLines

                fileLine =
                    case lastshln of
                        Just (ShowLine lastfl _ _)
                            | lastfl == curfl -> Nothing
                        _ -> Just $ FileLine curfl

                elipsisLine =
                    case lastshln of
                        Just (ShowLine lastfl lastnr _)
                            | lastfl == curfl && lastnr + 1 /= curnr ->
                                Just ElipsisLine
                        _ -> Nothing

                quoteLine = QuoteLine curfl curnr

                underlineLine =
                    if null lineUnderlines
                    then Nothing
                    else Just $ UnderlineLine lineUnderlines firstRowMessages

                (firstRowMessages, messageLines) = assignMessages lineMessages

                lineMessages = filter isCorrectLine msgs
                    where
                        isCorrectLine (Message (Span _ msgloc) _ _ _) = (fileOfLoc msgloc, lineMinus1 msgloc) == (curfl, curnr)
                lineUnderlines = filter isCorrectLine msgs
                    where
                        isCorrectLine (Message (Span start end) _ _ _) = curfl == fileOfLoc start && lnnOfLoc start <= curnr && curnr <= lineMinus1 end

        makeLines acc [] = acc

charOfImp :: Importance -> Char
charOfImp Primary = '^'
charOfImp Secondary = '~'
charOfImp Tertiary = '.'

sgrOfTy :: Type -> [ANSI.SGR]
sgrOfTy Error = [boldSGR, vividForeColorSGR ANSI.Red]
sgrOfTy Warning = [boldSGR, vividForeColorSGR ANSI.Magenta]
sgrOfTy Note = [boldSGR, vividForeColorSGR ANSI.Green]
sgrOfTy Hint = [boldSGR, vividForeColorSGR ANSI.Blue]

drawSectionLine :: Int -> SectionLine -> String
drawSectionLine indent (FileLine fl) = makeIndentWithDivider '>' "" indent ++ ANSI.setSGRCode filePathSGR ++ name fl ++ ANSI.setSGRCode [] ++ "\n"
drawSectionLine indent (DimQuote fl ln) =
    case drop (ln - 1) $ lines (source fl) of
        -- it is called a dim line, but it is not drawn dimly
        quote:_ -> makeIndentWithDivider '|' (show ln) indent ++ quote ++ "\n"
        [] -> ""
drawSectionLine indent (QuoteLine fl ln) = makeIndentWithDivider '|' (show ln) indent ++ quote ++ "\n"
    where
        quote = case drop (ln - 1) $ lines (source fl) of
            x:_ -> x
            [] -> ""
drawSectionLine indent ElipsisLine = makeIndentWithDivider '|' (replicate (indent - 1) '.') indent ++ "...\n"

drawSectionLine indent (UnderlineLine underlines messages) =
    makeIndentWithDivider '|' "" indent ++ draw 0 "" ++ "\n"
    where
        draw ind acc
            | ind > length columnMessages && ind > length columnUnderlines = acc
            | otherwise =
                let curUnderline =
                        case drop ind columnUnderlines of
                            x:_ -> x
                            [] -> Nothing
                    curMsg =
                        case drop ind columnMessages of
                            x: _ -> x
                            [] -> Nothing
                in case (curUnderline, curMsg) of
                    -- messages have higher priority than underlines
                    (_, Just (DMessage sgr _ str)) ->
                        let len = length str
                        in draw (ind + len + 3) (acc ++ ANSI.setSGRCode sgr ++ "-- " ++ str ++ ANSI.setSGRCode [])

                    (Just (imp, ty), Nothing) ->
                        draw (ind + 1) (acc ++ ANSI.setSGRCode (sgrOfTy ty) ++ [charOfImp imp] ++ ANSI.setSGRCode [])

                    (Nothing, Nothing) -> draw (ind + 1) (acc ++ " ")

        columnMessages = helper messages 1 []
            where
                helper [] _ acc = acc
                helper curmessages col acc =
                    helper rest (col+1) (acc ++ [current])
                    where
                        (inCurCol, rest) = partition (\ (DMessage _ msgcol _) -> col == (msgcol + 1)) curmessages
                        current =
                            case inCurCol of
                                [] -> Nothing
                                [x] -> Just x
                                _ -> error "multiple messages for same column in UnderlineLine"

        columnUnderlines = helper 1 []
            where
                helper col acc =
                    if anyUnderlinesLeft
                    then helper (col+1) (acc ++ [current])
                    else acc
                    where
                        anyUnderlinesLeft = any (\ (Message (Span _ end) _ _ _) -> col <= colMinus1 end) underlines
                        -- TODO: inCurCol is kind of broken because it doesn't handle multiline underlines properly
                        --       it just uses the column number without regard for the line number, so a multiline underline would just highlight the columns that are inbetween the start and end columns
                        --       kind of like a visual block selection vs a visual selection
                        inCurCol = find (\ (Message (Span start end) _ _ _) -> colnOfLoc start <= col && col <= colMinus1 end) underlines
                        current = case inCurCol of
                            Nothing -> Nothing
                            Just (Message _ ty imp _) -> Just (imp, ty)

drawSectionLine indent (MessageLine msgs) =
    makeIndentWithDivider '|' "" indent ++ draw msgs 1 "" ++ "\n"
    where
        draw curmessages@(_:_) col acc =
            case curs of
                [] -> draw rest (col + 1) (acc ++ " ")
                [(DMessage sgr _ str)] ->
                    let len = length str
                    in draw rest (col + len + 4) (acc ++ ANSI.setSGRCode sgr ++ "`-- " ++ str ++ ANSI.setSGRCode [])

                _ -> error "multiple messages on the same column in MessageLine"

            where
                (curs, rest) = partition (\ (DMessage _ msgcol _) -> col == msgcol) curmessages

        draw [] _ acc = acc
