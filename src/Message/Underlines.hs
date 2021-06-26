{-# LANGUAGE TupleSections #-}

module Message.Underlines
    ( Underline(..)
    , Message(..)
    , Importance(..)
    , Type(..)
    , show_underlines_section
    ) where

import Location
import File

import Message.Utils

import qualified Colors

import Data.List(nub, sortBy, partition, findIndex)
import Data.Maybe(maybeToList)
import Data.Char(isSpace)

import qualified System.Console.ANSI as ANSI

data Underline = Underline { get_span_of_underline :: Span, get_importance_of_underline :: Importance, get_messages_of_underline :: [Message] }
data Message = Message Type String
data Importance = Primary | Secondary | Tertiary
data Type = Error | Warning | Note | Hint

show_underlines_section :: [Underline] -> [DiagLine]
show_underlines_section underlines = singleline_underlines' ++ multiline_underlines'
    where
        (singleline_underlines, multiline_underlines) = partition (is_single_line . get_span_of_underline) underlines

        singleline_underlines' = show_singleline_underlines singleline_underlines
        multiline_underlines' = show_multiline_underlines multiline_underlines

is_single_line :: Span -> Bool
is_single_line (Span start before_end _) = lnn_of_loc start == lnn_of_loc before_end

show_singleline_underlines :: [Underline] -> [DiagLine]
show_singleline_underlines underlines = concat $ zipWith make_lines lines_shown (Nothing : map Just lines_shown)
    where
        lines_shown = get_lines_shown underlines

        make_lines (fl, nr) m_last_flnr = maybeToList file_line ++ maybeToList elipsis_line ++ content_lines
            where
                file_line =
                    case m_last_flnr of
                        Just (last_fl, _)
                            | last_fl == fl -> Nothing

                        _ -> Just $ DiagLine "" '>' (ANSI.setSGRCode Colors.file_path_sgr ++ name fl ++ ANSI.setSGRCode [])

                elipsis_line =
                    case m_last_flnr of
                        Just (last_fl, last_nr)
                            | last_fl == fl && last_nr + 1 /= nr -> Just $ DiagLine "..." '|' "..."

                        _ -> Nothing

                content_lines = show_quote_and_underlines fl nr underlines

show_quote_and_underlines :: File -> Int -> [Underline] -> [DiagLine]
show_quote_and_underlines file line_nr underlines =
    [DiagLine (show line_nr) '|' quote] ++
    underline_line ++ message_lines
    where
        underlines_on_line = filter (is_on_line . get_span_of_underline) underlines
        is_on_line (Span start _ _) = file_of_loc start == file && lnn_of_loc start == line_nr

        quote = case drop (line_nr - 1) $ lines $ source file of
            x:_ -> x
            [] -> ""

        underline_line
            | null underlines_on_line = []
            | otherwise = [DiagLine "" '|' $ concatMap get_und [1..max_col]]
                where
                    get_und col = head $ concatMap (in_col col) underlines_on_line ++ [" "]
                        where
                            in_col c (Underline (Span start before _) imp msgs)
                                | coln_of_loc start <= c && coln_of_loc before >= c =
                                    let sgr = sgr_of_msgs msgs
                                    in [ANSI.setSGRCode sgr ++ [char_of_imp imp] ++ ANSI.setSGRCode []]

                                | otherwise = []

                    max_col = maximum $ map (get_end_col . get_span_of_underline) underlines_on_line
                    get_end_col (Span _ before _) = coln_of_loc before

        message_assignments = assign_messages $ concatMap get_messages underlines_on_line
            where
                get_messages (Underline (Span _ before _) _ msgs) = map (coln_of_loc before,) msgs

        message_lines = map draw_message_line message_assignments
        draw_message_line msgs = DiagLine "" '|' $ draw msgs 1 ""
            where
                draw curmsgs col acc
                    | null curmsgs = acc
                    | otherwise =
                        let (curs, rest) = partition ((col==) . fst) curmsgs
                        in case curs of
                            [] -> draw rest (col + 1) (acc ++ " ")
                            [(_, Message ty str)] ->
                                let len = length str
                                    sgr = sgr_of_ty ty
                                in draw rest (col + len + 4) (acc ++ ANSI.setSGRCode sgr ++ "`-- " ++ str ++ ANSI.setSGRCode [])

                            _ -> error "multiple messages on the same column"

assign_messages :: [(Int, Message)] -> [[(Int, Message)]]
assign_messages messages = takeWhile (not . null) $ map find_msgs_on_row [0..]
    where
        coln_comparator (col1, _) (col2, _)  = col2 `compare` col1
        assignments = zipWith (\ ind msg -> (assign msg ind, msg)) [0..] (sortBy coln_comparator messages)

        assign :: (Int, Message) -> Int -> Int
        assign to_assign ind =
            let already_assigned = take ind assignments

                msgs_on_row r = map snd $ filter ((r==) . fst) already_assigned

                end_col_of_msg (start_col, Message _ str) = start_col + length str + 4

                overlapping = (end_col_of_msg to_assign >=) . fst

            in case findIndex (not . any overlapping . msgs_on_row) [0..] of
                Just x -> x
                Nothing -> error "unreachable"

        find_msgs_on_row row = map snd $ filter ((row==) . fst) assignments

char_of_imp :: Importance -> Char
char_of_imp Primary = '^'
char_of_imp Secondary = '~'
char_of_imp Tertiary = '.'

sgr_of_ty :: Type -> [ANSI.SGR]
sgr_of_ty Error = Colors.error_sgr
sgr_of_ty Warning = Colors.warning_sgr
sgr_of_ty Note = Colors.note_sgr
sgr_of_ty Hint = Colors.hint_sgr

sgr_of_msgs :: [Message] -> [ANSI.SGR]
sgr_of_msgs [] = Colors.empty_underline_sgr
sgr_of_msgs (Message ty _ : _) = sgr_of_ty ty

get_lines_shown :: [Underline] -> [(File, Int)]
get_lines_shown = sortBy sort_comparator . nub . concatMap (get_dim_lines . get_starts . get_span_of_underline)
    where
        get_dim_lines (fl, lnnr) = map (fl,) $ filter (>=1) $ map (lnnr+) [-2..2]
        get_starts (Span start _ _) = (file_of_loc start, lnn_of_loc start)

        sort_comparator (fl1, nr1) (fl2, nr2)
            | fl1 == fl2 = nr1 `compare` nr2
            | otherwise = name fl1 `compare` name fl2

-- TODO: clean up this code, lots of magic numbers and stuff
show_multiline_underlines :: [Underline] -> [DiagLine]
show_multiline_underlines = concatMap show_multiline_underline
    where
        show_multiline_underline (Underline (Span sp_start sp_before _) imp msgs) =
            concat
                [ file_line

                , before_first_quote_line
                , first_quote_line
                , after_first_quote_line

                , middle_quote_lines

                , before_last_quote_line
                , last_quote_line
                , after_last_quote_line
                , msgs_lines
                ]

            where
                tycolor = ANSI.setSGRCode $ sgr_of_msgs msgs
                colorify x = tycolor ++ x ++ ANSI.setSGRCode []

                impchar = char_of_imp imp

                first_line_nr = lnn_of_loc sp_start
                last_line_nr = lnn_of_loc sp_before
                first_col = coln_of_loc sp_start
                last_col = coln_of_loc sp_before

                min_col = 1 + minimum (map amt_wh [first_line_nr + 1 .. last_line_nr])
                    where
                        amt_wh lnnr
                            | all isSpace ln = maxBound
                            | otherwise = length $ takeWhile isSpace ln
                            where
                                ln = get_line lnnr
                max_col = 1 + maximum (map (length . get_line) [first_line_nr .. last_line_nr - 1])

                get_line n = case drop (n - 1) $ lines $ source $ file_of_loc sp_start of
                    x:_ -> x
                    [] -> ""

                padded_delim = [' ', impchar, ' ']

                file_line = [DiagLine "" '>' $ ANSI.setSGRCode Colors.file_path_sgr ++ name (file_of_loc sp_start) ++ ANSI.setSGRCode []]

                surround_line str startcol endcol =
                    not_surrounded_left ++ colorify padded_delim ++ surrounded ++ colorify padded_delim ++ not_surrounded_right
                    where
                        str_extended = str ++ repeat ' '
                        (not_surrounded_left, rest) = splitAt (startcol - 1) str_extended
                                                       -- +1 because end column is included in the box
                        (surrounded, rest') = splitAt (endcol - startcol + 1) rest
                        not_surrounded_right = take (length str - endcol) rest'

                                                                                                -- +4 for '^ ' and ' ^'
                                                                                                -- +1 for inclusive end
                top_or_bottom startcol endcol = colorify $ replicate startcol ' ' ++ replicate (endcol - startcol + 4 + 1) impchar

                transition_line a1 b1 a2 b2 =
                    if a1 == b1 && a2 == b2
                    then []
                    else [DiagLine "" '|' $ colorify (replicate abs1start ' ' ++ replicate abs1len impchar ++ replicate absdistbetween ' ' ++ replicate abs2len impchar)]
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

                before_first_quote_line = [DiagLine "" '|' (top_or_bottom first_col max_col)]
                first_quote_line = [DiagLine (show first_line_nr) '|' (surround_line (get_line first_line_nr) first_col max_col)]
                after_first_quote_line = transition_line first_col min_col max_col max_col

                middle_quote_lines = lines_trimmed
                    where
                        lines_trimmed =
                            let middle_line_nrs = [first_line_nr + 1 .. last_line_nr - 1]
                                amt_middle_lines = length middle_line_nrs
                            in if amt_middle_lines  <= 1
                                then map make_line middle_line_nrs
                                else
                                    map make_line (take 5 middle_line_nrs) ++
                                    [DiagLine "..." '|' "..."] ++
                                    map make_line (drop (amt_middle_lines - 5) middle_line_nrs)
                            where
                                make_line lnnr = DiagLine (show lnnr) '|' (surround_line (get_line lnnr) min_col max_col)

                before_last_quote_line = transition_line min_col min_col max_col last_col
                last_quote_line = [DiagLine (show last_line_nr) '|' (surround_line (get_line last_line_nr) min_col last_col)]
                after_last_quote_line = [DiagLine "" '|' (top_or_bottom min_col last_col)]

                msgs_lines = map (DiagLine "" '|' . show_msg) msgs
                    where
                        show_msg (Message ty str) = replicate (last_col + 4) ' ' ++ ANSI.setSGRCode (sgr_of_ty ty) ++ "`-- " ++ str ++ ANSI.setSGRCode []
