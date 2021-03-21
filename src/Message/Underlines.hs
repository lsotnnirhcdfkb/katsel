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

import Data.List(nub, sortBy, partition, find)

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

linenrsOfMessages :: [Message] -> [(File, Int)]
linenrsOfMessages msgs = sortBy comparator $ nub $ concatMap linenrsof msgs
    where
        linenrsof (Message (Span start end) _ _ _) = [(fileOfLoc start, lnnOfLoc start), (fileOfLoc start, lineMinus1 end)]

        comparator (fl1, nr1) (fl2, nr2) =
            if fl1 == fl2
            then nr1 `compare` nr2
            else name fl1 `compare` name fl2

indentOfUnderlinesSection :: UnderlinesSection -> Int
indentOfUnderlinesSection (UnderlinesSection msgs) = 1 + (maximum $ map (length . show . snd) $ linenrsOfMessages msgs)

showUnderlinesSection :: Int -> UnderlinesSection -> String
showUnderlinesSection indent sec =
    concatMap (drawSectionLine indent) $ sectionLines sec

data DrawableMessage = DMessage [ANSI.SGR] Int String
data SectionLine
    = FileLine File
    | QuoteLine File Int
    | ElipsisLine
    | UnderlineLine [Message] [DrawableMessage]
    | MessageLine [DrawableMessage]

sectionLines :: UnderlinesSection -> [SectionLine]
sectionLines (UnderlinesSection msgs) =
    makeLines [] $ zip flnrs $ Nothing:(map Just flnrs)
    where
        flnrs = linenrsOfMessages msgs

        makeLines acc ((curflnr@(curfl, curnr), lastflnr):more) =
            makeLines nextAcc more
            where
                nextAcc = acc ++ maybeToList fileLine ++ maybeToList elipsisLine ++ [quoteLine] ++ maybeToList underlineLine ++ messageLines

                fileLine =
                    case lastflnr of
                        Just (lastfl, _)
                            | lastfl == curfl -> Nothing
                        _ -> Just $ FileLine curfl

                elipsisLine =
                    case lastflnr of
                        Just (lastfl, lastnr)
                            | lastfl == curfl && lastnr + 1 /= curnr ->
                                Just ElipsisLine
                        _ -> Nothing

                quoteLine = QuoteLine curfl curnr

                underlineLine =
                    if null lineUnderlines
                    then Nothing
                    else Just $ UnderlineLine lineUnderlines []

                messageLines =
                    map (\ (Message (Span _ end) ty _ msg) -> MessageLine [DMessage (sgrOfTy ty) (colMinus1 end) msg]) lineMessages

                lineMessages = filter isCorrectLine msgs
                    where
                        isCorrectLine (Message (Span _ msgloc) _ _ _) = (fileOfLoc msgloc, lineMinus1 msgloc) == curflnr
                lineUnderlines = filter isCorrectLine msgs
                    where
                        isCorrectLine (Message (Span start end) _ _ _) =
                            curfl == fileOfLoc start && lnnOfLoc start <= curnr && curnr <= lineMinus1 end

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
                    (Just (imp, ty), Nothing) ->
                        draw (ind + 1) (acc ++ ANSI.setSGRCode (sgrOfTy ty) ++ [charOfImp imp] ++ ANSI.setSGRCode [])

                    (Nothing, Just (DMessage sgr _ str)) ->
                        let len = length str
                        in draw (ind + len + 3) (acc ++ ANSI.setSGRCode sgr ++ "-- " ++ str ++ ANSI.setSGRCode [])

                    (Nothing, Nothing) -> draw (ind + 1) (acc ++ " ")
                    (Just _, Just _) -> error "message and underline on same column"

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

{-
        showLine flnr@(fl, nr) = quoteLine ++ firstRow ++ nextRows
            where
                assignedMessages =
                    assignMessages (sortBy (\ (Message (Span _ msg1l) _ _ _) (Message (Span _ msg2l) _ _ _) -> (colOfMinus1 msg2l) `compare` (colOfMinus1 msg1l)) lineMessages) []
                    where
                        assignMessages [] a = a
                        assignMessages (toAssign:rest) alreadyAssigned =
                            let assignment = assignRowNum toAssign 0 alreadyAssigned
                                restAssignments = assignMessages rest $ assignment:alreadyAssigned
                            in restAssignments

                        assignRowNum :: Message -> Int -> [(Int, Int, Int, String)] -> (Int, Int, Int, String)
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

                        makeAssignment msg@(Message _ ty _ _) row =
                            (row, startcolumn row msg, endcolumn row msg, ANSI.setSGRCode (colorOfType ty) ++ augmentedMessage row msg ++ ANSI.setSGRCode [])

                        startcolumn row (Message (Span _ eloc) _ _ _) = (colOfMinus1 eloc) + if row == 0 then 1 else 0
                        endcolumn row msg = startcolumn row msg + length (augmentedMessage row msg)
                        augmentedMessage row (Message _ _ _ msgText) = (if row == 0 then "-- " else "`-- ") ++ msgText

                underlinesForChars = map getUnderlineForChar $ take (length quote + 1) [1..]
                    where
                        getUnderlineForChar coln = find (\ msg -> startcol msg <= coln && coln < endcol msg) lineUnderlines
                        startcol (Message (Span start _) _ _ _) =
                            if lnnOfLoc start == nr
                            then colnOfLoc start
                            else 1
                        endcol (Message (Span _ end) _ _ _) =
                            if lnnOfLoc end == nr
                            then colnOfLoc end
                            else maxBound

-}
