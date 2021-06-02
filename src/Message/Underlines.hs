module Message.Underlines
    ( UnderlinesSection(..)
    , Message(..)
    , Importance(..)
    , Type(..)
    , show_underlines_section
    , indent_of_underlines_section
    ) where

import Location
import File

import Message.Utils

import Data.List(nubBy, sortBy, partition, find, findIndex)

import Data.Maybe(maybeToList, fromMaybe, mapMaybe)

import Data.Char(isSpace)

import qualified System.Console.ANSI as ANSI

import qualified Colors

-- TODO: clean up all the messy code in this file

data UnderlinesSection = UnderlinesSection [Message]
data Message = Message Span Type Importance String
data Importance = Primary | Secondary | Tertiary
data Type = Error | Warning | Note | Hint

data ShowLine = ShowLine File Int Dimness
data Dimness = Dim | Normal

line_nrs_of_messages :: [Message] -> [ShowLine]
line_nrs_of_messages msgs = sortBy sort_comparator $ nubBy nub_comparator lines_with_dim
    where
        -- nub keeps the first occurance, so it will keep all the normal lines if there are duplicate dim lines
        -- since the dim lines are all appended to the end of the list
        lines_with_dim = lines_without_dim ++ concatMap get_dim_lines lines_without_dim
        get_dim_lines (ShowLine fl nr _) = mapMaybe make_dim [-2..2] -- will have a duplicate line at offset 0 that is filtered out by nub
            where
                make_dim n
                    | nr+n >= 1 = Just $ ShowLine fl (nr+n) Dim
                    | otherwise = Nothing

        lines_without_dim = concatMap linenrsof msgs
        linenrsof (Message (Span start before_end _) _ _ _) = [ShowLine (file_of_loc start) (lnn_of_loc start) Normal, ShowLine (file_of_loc start) (lnn_of_loc before_end) Normal]

        sort_comparator (ShowLine fl1 nr1 _) (ShowLine fl2 nr2 _) =
            if fl1 == fl2
            then nr1 `compare` nr2
            else name fl1 `compare` name fl2

        nub_comparator (ShowLine fl1 nr1 _) (ShowLine fl2 nr2 _) = (fl1, nr1) == (fl2, nr2)

indent_of_underlines_section :: UnderlinesSection -> Int
indent_of_underlines_section (UnderlinesSection msgs) = 1 + maximum (map get_width $ line_nrs_of_messages msgs)
    where
        get_width (ShowLine _ ln _) = length . show $ ln

show_underlines_section :: Int -> UnderlinesSection -> String
show_underlines_section indent sec =
    concatMap (draw_section_line indent) $ section_lines sec

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
    | MultilineMessageLines Message

assign_messages :: [Message] -> ([DrawableMessage], [SectionLine])
assign_messages messages = (firstrow, msglines)
    where
        coln_comparator (Message (Span _ before_end_1 _) _ _ _) (Message (Span _ before_end_2 _) _ _ _) = coln_of_loc before_end_2 `compare` coln_of_loc before_end_1
        assignments = map (\ (i, m) -> (assign m i, m)) $ zip [0..] (sortBy coln_comparator messages)

        assign :: Message -> Int -> Int
        assign msg cur_idx =
            let rows = [0..]
                msgs_on_row r = map snd $ filter ((r==) . fst) (take cur_idx assignments)

                col_of_msg (Message (Span _ before_end _) _ _ _) = coln_of_loc before_end
                end_col_of_msg m@(Message _ _ _ str) = col_of_msg m + length str + 3

                msg_end_col = end_col_of_msg msg
                overlapping = (msg_end_col >=) . col_of_msg

            in case findIndex (not . (any overlapping) . msgs_on_row) rows of
                Just x -> x
                Nothing -> error "unreachable"

        find_msgs_on_row row = map (todmsg . snd) $ filter ((row==) . fst) assignments

        msglines = map MessageLine $ takeWhile (not . null) $ map find_msgs_on_row [1..]
        firstrow = find_msgs_on_row 0

        todmsg (Message (Span _ before_end _) ty _ str) = DMessage (sgr_of_ty ty) (coln_of_loc before_end) str

section_lines :: UnderlinesSection -> [SectionLine]
section_lines (UnderlinesSection msgs) =
    concatMap make_lines (zip flnrs (Nothing : map Just flnrs)) ++ map MultilineMessageLines multiline_msgs
    where
        flnrs = line_nrs_of_messages msgs

        multiline_msgs = filter is_multiline msgs
            where
                is_multiline (Message (Span start before_end _) _ _ _) = lnn_of_loc start /= lnn_of_loc before_end


        make_lines (ShowLine curfl curnr curdimn, lastshln) = maybeToList file_line ++ maybeToList elipsis_line ++ content_lines
            where
                file_line =
                    case lastshln of
                        Just (ShowLine lastfl _ _)
                            | lastfl == curfl -> Nothing
                        _ -> Just $ FileLine curfl

                elipsis_line =
                    case lastshln of
                        Just (ShowLine lastfl lastnr _)
                            | lastfl == curfl && lastnr + 1 /= curnr ->
                                Just ElipsisLine
                        _ -> Nothing

                content_lines =
                    case curdimn of
                        Dim -> [DimQuote curfl curnr]
                        Normal -> [QuoteLine curfl curnr] ++ maybeToList underline_line ++ message_lines
                    where
                        underline_line =
                            if null line_underlines
                            then Nothing
                            else Just $ UnderlineLine line_underlines first_row_messages

                (first_row_messages, message_lines) = assign_messages line_messages

                is_single_line (Span start before_end _) = lnn_of_loc start == lnn_of_loc before_end
                line_messages = filter is_correct_line msgs
                    where
                        is_correct_line (Message sp@(Span _ before_end _) _ _ _) = (file_of_loc before_end, lnn_of_loc before_end) == (curfl, curnr) && is_single_line sp
                line_underlines = filter is_correct_line msgs
                    where
                        is_correct_line (Message sp@(Span start _ _) _ _ _) = curfl == file_of_loc start && lnn_of_loc start == curnr && is_single_line sp

char_of_imp :: Importance -> Char
char_of_imp Primary = '^'
char_of_imp Secondary = '~'
char_of_imp Tertiary = '.'

sgr_of_ty :: Type -> [ANSI.SGR]
sgr_of_ty Error = Colors.error_sgr
sgr_of_ty Warning = Colors.warning_sgr
sgr_of_ty Note = Colors.note_sgr
sgr_of_ty Hint = Colors.hint_sgr

elipsis_prefix :: Int -> String
elipsis_prefix indent = replicate (indent - 1) '.'

draw_section_line :: Int -> SectionLine -> String
draw_section_line indent (FileLine fl) = make_indent_with_divider '>' "" indent ++ ANSI.setSGRCode Colors.file_path_sgr ++ name fl ++ ANSI.setSGRCode [] ++ "\n"
draw_section_line indent (DimQuote fl ln) =
    case drop (ln - 1) $ lines (source fl) of
        -- it is called a dim line, but it is not drawn dimly
        -- cannot handle empty lines here because if some dim lines are hidden here then elipsis lines are not inserted when necessary
        quote:_ -> make_indent_with_divider '|' (show ln) indent ++ quote ++ "\n"
        [] -> ""
draw_section_line indent (QuoteLine fl ln) = make_indent_with_divider '|' (show ln) indent ++ quote ++ "\n"
    where
        quote = case drop (ln - 1) $ lines (source fl) of
            x:_ -> x
            [] -> ""
draw_section_line indent ElipsisLine = make_indent_with_divider '|' (elipsis_prefix indent) indent ++ "...\n"

draw_section_line indent (UnderlineLine underlines messages) =
    make_indent_with_divider '|' "" indent ++ draw 0 "" ++ "\n"
    where
        draw ind acc
            | ind > length column_messages && ind > length column_underlines = acc
            | otherwise =
                let cur_underline =
                        case drop ind column_underlines of
                            x:_ -> x
                            [] -> Nothing
                    cur_msg =
                        case drop ind column_messages of
                            x: _ -> x
                            [] -> Nothing
                in case (cur_underline, cur_msg) of
                    -- messages have higher priority than underlines
                    (_, Just (DMessage sgr _ str)) ->
                        let len = length str
                        in draw (ind + len + 3) (acc ++ ANSI.setSGRCode sgr ++ "-- " ++ str ++ ANSI.setSGRCode [])

                    (Just (imp, ty), Nothing) ->
                        draw (ind + 1) (acc ++ ANSI.setSGRCode (sgr_of_ty ty) ++ [char_of_imp imp] ++ ANSI.setSGRCode [])

                    (Nothing, Nothing) -> draw (ind + 1) (acc ++ " ")

        column_messages = helper messages 1 []
            where
                helper [] _ acc = acc
                helper curmessages col acc =
                    helper rest (col+1) (acc ++ [current])
                    where
                        (in_cur_col, rest) = partition (\ (DMessage _ msgcol _) -> col == (msgcol + 1)) curmessages
                        current =
                            case in_cur_col of
                                [] -> Nothing
                                [x] -> Just x
                                _ -> error "multiple messages for same column in UnderlineLine"

        column_underlines = helper 1 []
            where
                helper col acc =
                    if any_underlines_left
                    then helper (col+1) (acc ++ [current])
                    else acc
                    where
                        any_underlines_left = any (\ (Message (Span _ before_end _) _ _ _) -> col <= coln_of_loc before_end) underlines
                        in_cur_col = find (\ (Message (Span start before_end _) _ _ _) -> coln_of_loc start <= col && col <= coln_of_loc before_end) underlines
                        current = case in_cur_col of
                            Nothing -> Nothing
                            Just (Message _ ty imp _) -> Just (imp, ty)

draw_section_line indent (MessageLine msgs) =
    make_indent_with_divider '|' "" indent ++ draw msgs 1 "" ++ "\n"
    where
        draw curmessages@(_:_) col acc =
            case curs of
                [] -> draw rest (col + 1) (acc ++ " ")
                [DMessage sgr _ str] ->
                    let len = length str
                    in draw rest (col + len + 4) (acc ++ ANSI.setSGRCode sgr ++ "`-- " ++ str ++ ANSI.setSGRCode [])

                _ -> error "multiple messages on the same column in MessageLine"

            where
                (curs, rest) = partition (\ (DMessage _ msgcol _) -> col == msgcol) curmessages

        draw [] _ acc = acc

-- TODO: this is really messy and has a lot of magic numbers, refactor this maybe
draw_section_line indent (MultilineMessageLines (Message (Span spstart sp_before_end _) ty imp msg)) =
    fileline ++
    before_first_quote_line ++
    first_quote_line ++
    fromMaybe "" after_first_quote_line ++
    middle_quote_lines ++
    fromMaybe "" before_last_quote_line ++
    last_quote_line ++
    after_last_quote_line
    where
        prefix ch = make_indent_with_divider ch "" indent

        tycolor = ANSI.setSGRCode $ sgr_of_ty ty
        colorify x = tycolor ++ x ++ ANSI.setSGRCode []

        impchar = char_of_imp imp

        fileline = draw_section_line indent $ FileLine $ file_of_loc spstart

        startlnn = lnn_of_loc spstart
        endlnn = lnn_of_loc sp_before_end
        msglnns = [startlnn+1..endlnn-1]

        firstcol = coln_of_loc spstart
        mincol = 1 + minimum (map wh_in_line [startlnn+1..endlnn])
            where
                wh_in_line lnnr =
                    if all isSpace ln
                    then maxBound
                    else length $ takeWhile isSpace ln
                    where
                        ln = getlnn lnnr
        maxcol = 1 + maximum (map (length . getlnn) [startlnn..endlnn-1])
        lastcol = coln_of_loc sp_before_end

        getlnn n = case drop (n - 1) $ lines $ source $ file_of_loc spstart of
            x:_ -> x
            [] -> "after"

        surround_delim = [' ', impchar, ' ']
        surround str startcol endcol =
            not_surrounded_left ++ colorify surround_delim ++ surrounded ++ colorify surround_delim ++ not_surrounded_right
            where
                str_extended = str ++ repeat ' '
                (not_surrounded_left, rest) = splitAt (startcol - 1) str_extended
                                               -- +1 because end column is included in the box
                (surrounded, rest') = splitAt (endcol - startcol + 1) rest
                not_surrounded_right = take (length str - endcol) rest'
                                                                                   -- +4 for '^ ' and ' ^'
                                                                                   -- +1 for inclusive end
        topbottom startcol endcol = colorify $ replicate startcol ' ' ++ replicate (endcol-startcol+4+1) impchar
        transition_line a1 b1 a2 b2 =
            if a1 == b1 && a2 == b2
            then Nothing
            else Just $ make_indent_with_divider '|' "" indent ++ colorify (replicate abs1start ' ' ++ replicate abs1len impchar ++ replicate absdistbetween ' ' ++ replicate abs2len impchar) ++ "\n"
            where
                lowerupper a b = (min a b, max a b)
                (lower1, upper1) = lowerupper a1 b1
                (lower2, upper2) = lowerupper a2 b2

                abs1start = lower1
                abs1end = upper1
                abs2start = lower2 + 3 -- +3 for first divider, +1 for space before current divider, -1 for zero based columns
                abs2end = upper2 + 3

                abs1len = abs1end - abs1start + 1 -- +1 for inclusive end, if not then it's just the number of columsn in between, not including the end
                absdistbetween = abs2start - abs1end
                abs2len = abs2end - abs2start + 1 -- +1 also for inclusive end

        before_first_quote_line = prefix '|' ++ topbottom firstcol maxcol ++ "\n"
        first_quote_line = make_indent_with_divider '|' (show startlnn) indent ++ surround (getlnn startlnn) firstcol maxcol ++ "\n"
        after_first_quote_line = transition_line firstcol mincol maxcol maxcol

        middle_quote_lines = lines_trimmed
            where
                lines_trimmed =
                    if len <= 10
                    then concatMap make_line msglnns
                    else
                        concatMap make_line (take 5 msglnns) ++
                        make_indent_with_divider '|' (elipsis_prefix indent) indent ++ "...\n" ++
                        concatMap make_line (drop (len - 5) msglnns)
                    where
                        len = length msglnns
                make_line lnnr = make_indent_with_divider '|' (show lnnr) indent ++ surround (getlnn lnnr) mincol maxcol ++ "\n"

        before_last_quote_line = transition_line mincol mincol maxcol lastcol
        last_quote_line = make_indent_with_divider '|' (show endlnn) indent ++ surround (getlnn endlnn) mincol lastcol ++ "\n"
        after_last_quote_line = prefix '|' ++ topbottom mincol lastcol ++ colorify ("-- " ++ colorify msg) ++ "\n"
