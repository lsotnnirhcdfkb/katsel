module Message.Arrows
    ( show_arrows
    ) where

import Location

import Message.Utils

import qualified Colors

import Data.Maybe (maybeToList, isJust)
import Data.List (mapAccumL)

show_arrows :: [(Span, String)] -> [DiagLine]
show_arrows places = concat $ snd $ mapAccumL f Nothing $ zip places $ add_file_and_elipsis_lines_grouped $ map into_flnr places
    where
        into_flnr (Span start _ _, _) = (file_of_loc start, line_of_loc start)

        f last_iter cur@((sp, _), _) =
            let (cur_lines, end_col) = show_place cur last_iter
            in (Just (sp, end_col), cur_lines)

show_place :: ((Span, String), [FileElipsisRes]) -> Maybe (Span, Int) -> ([DiagLine], Int)
show_place ((sp@(Span cur_start cur_before _), msg), file_elipsis_reses) m_last =
    ( concat
        [ maybeToList last_pipe_line
        , concatMap show_file_elipsis_res file_elipsis_reses
        , maybeToList transition_line
        , current_pipe_and_arrow_lines
        , underline_lines
        , message_lines
        ]
    , end_col)
    where
        show_file_elipsis_res (FileLine f) = [file_line f]
        show_file_elipsis_res ElipsisLine = [elipsis_line]
        show_file_elipsis_res (QuoteLine _ _) = []

        cur_file = file_of_loc cur_start
        cur_start_line = line_of_loc cur_start

        message_lines = [DiagLine "" '|' $ replicate (end_col - 1) ' ' ++ enclose_sgr Colors.empty_underline_sgr ("`-- " ++ msg)]

        replace_with_pipe last_ind = replace_at last_ind ' ' '|'

        last_pipe_line =
            case m_last of
                Just (_, last_end_col) -> Just $ DiagLine "" '|' $ replace_with_pipe (last_end_col - 1) ""
                Nothing -> Nothing

        transition_line =
            case m_last of
                Just (_, last_end_col)
                    | last_end_col == start_col -> Nothing
                    | otherwise -> Just $ DiagLine "" '|' $ horiz_line '-' lower upper
                        where
                            lower = min last_end_col start_col
                            upper = max last_end_col start_col

                Nothing -> Nothing

        current_pipe_and_arrow_lines
            | isJust m_last =
                DiagLine "" '|' <$>
                    (if isJust transition_line
                        then [replace_with_pipe (start_col - 1) ""]
                        else []
                    ) ++ [replace_at (start_col - 1) ' ' 'v' ""]
            | otherwise = []

        (underline_lines, start_col, end_col)
            | is_single_line sp =
                ( [ DiagLine (show cur_start_line) '|' $ get_line_from_file cur_file cur_start_line
                  , DiagLine "" '|' $ enclose_sgr Colors.empty_underline_sgr (horiz_line '^' (col_of_loc cur_start) (col_of_loc cur_before))
                  ]
                , col_of_loc cur_start
                , col_of_loc cur_before
                )

            | otherwise = draw_box sp '^' Colors.empty_underline_sgr
