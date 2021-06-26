module Message.Arrows
    ( show_arrows
    ) where

import Location
import File

import Message.Utils

import qualified Colors

import Data.Maybe(maybeToList, isJust)
import Data.List(foldl')

import qualified System.Console.ANSI as ANSI

show_arrows :: [(Span, String)] -> [DiagLine]
show_arrows places = fst $ foldl' f ([], Nothing) places
    where
        f (acc, last_iter) cur@(sp, _) =
            let (cur_lines, end_col) = show_place cur last_iter
            in (acc ++ cur_lines, Just (sp, end_col))

show_place :: (Span, String) -> Maybe (Span, Int) -> ([DiagLine], Int)
show_place (sp@(Span cur_start cur_before _), msg) m_last =
    ( concat
        [ maybeToList last_pipe_line 
        , maybeToList file_line 
        , maybeToList elipsis_line 
        , maybeToList transition_line
        , current_pipe_and_arrow_lines 
        , underline_lines 
        , message_lines
        ]
    , end_col)
    where
        cur_start_line = lnn_of_loc cur_start

        replace_with_pipe last_ind = replace_at last_ind ' ' '|'

        last_pipe_line =
            case m_last of
                Just (_, last_end_col) -> Just $ DiagLine "" '|' $ replace_with_pipe (last_end_col - 1) ""
                Nothing -> Nothing

        file_line =
            case m_last of
                Just (Span last_start _ _, _)
                    | file_of_loc last_start == file_of_loc cur_start && lnn_of_loc last_start <= cur_start_line -> Nothing

                _ -> Just $ DiagLine "" '>' (ANSI.setSGRCode Colors.file_path_sgr ++ name (file_of_loc cur_start) ++ ANSI.setSGRCode [])

        elipsis_line =
            case (m_last, file_line) of
                (Just (Span last_start _ _, last_end_col), Nothing)
                    | lnn_of_loc last_start + 1 /= cur_start_line -> Just $ DiagLine "..." '|' (replace_with_pipe (last_end_col - 1) "...")

                _ -> Nothing

        transition_line =
            case m_last of
                Just (_, last_end_col)
                    | last_end_col == start_col -> Nothing
                    | otherwise -> Just $ DiagLine "" '|' $ replicate (lower - 1) ' ' ++ replicate (upper - lower + 1) '-'
                        where
                            lower = min last_end_col start_col
                            upper = max last_end_col start_col
                    
                Nothing -> Nothing

        current_pipe_and_arrow_lines
            | isJust m_last =
                DiagLine "" '|' <$>
                    (case transition_line of
                        Just _ -> [replace_with_pipe (start_col - 1) ""]
                        Nothing -> []
                    ) ++ [replace_at (start_col - 1) ' ' 'v' ""]
            | otherwise = []

        (underline_lines, start_col, end_col)
            | is_single_line sp =
                ( (case drop (cur_start_line - 1) $ lines $ source $ file_of_loc cur_start of
                      x:_ -> [DiagLine (show cur_start_line) '|' x]
                      [] -> [DiagLine (show cur_start_line) '|' ""]
                  ) ++ [DiagLine "" '|' $ replicate (coln_of_loc cur_start - 1) ' ' ++ ANSI.setSGRCode Colors.empty_underline_sgr ++ replicate (coln_of_loc cur_before - coln_of_loc cur_start + 1) '^' ++ ANSI.setSGRCode []]
                , coln_of_loc cur_start
                , coln_of_loc cur_before
                )

            | otherwise = draw_box sp '^' Colors.empty_underline_sgr

        message_lines = [DiagLine "" '|' $ replicate (end_col - 1) ' ' ++ ANSI.setSGRCode Colors.empty_underline_sgr ++ "`-- " ++ msg ++ ANSI.setSGRCode []]

replace_at :: Int -> a -> a -> [a] -> [a]
replace_at ind fill change orig =
    let extended = orig ++ repeat fill
        (keep, _:keep2) = splitAt ind extended
    in take (max (ind + 1) (length orig)) (keep ++ change : keep2)
