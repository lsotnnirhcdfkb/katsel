module Location
    ( Location
    , LineCol
    , Span(..)
    , Located(..)

    , ind_of_loc
    , linecol_of_loc
    , file_of_loc
    , line_of_loc
    , col_of_loc

    , make_location
    , make_linecol
    , make_location_from_ind

    , join_span

    , is_single_line

    , fmt_location
    , fmt_linecol
    , fmt_span
    ) where

import File
import Data.List (minimumBy, maximumBy)
import Data.Function (on)

data Location
    = Location
      { file_of_loc :: File
      , ind_of_loc :: Int
      , linecol_of_loc :: LineCol
      }
    deriving Eq

line_of_loc :: Location -> Int
line_of_loc = line_of_linecol . linecol_of_loc
col_of_loc :: Location -> Int
col_of_loc = col_of_linecol . linecol_of_loc

data LineCol = LineCol { line_of_linecol :: Int, col_of_linecol :: Int } deriving Eq
data Span = Span Location Location Location deriving Eq

data Located a
    = Located
      { just_span :: Span
      , unlocate :: a
      }
    deriving Eq

instance Functor Located where
    fmap f (Located sp v) = Located sp (f v)

-- make location {{{1
make_location :: File -> Int -> LineCol -> Location
make_location = Location

make_linecol :: Int -> Int -> LineCol
make_linecol = LineCol

make_location_from_ind :: File -> Int -> Location
make_location_from_ind file ind = Location file ind (LineCol (find_lnn file ind) (find_coln file ind))

find_lnn :: File -> Int -> Int
find_lnn file ind = 1 + length (filter ('\n'==) (take ind $ file_source file))

find_coln :: File -> Int -> Int
find_coln file ind = 1 + length (takeWhile (/='\n') $ reverse $ take ind $ file_source file)
-- join_span {{{1
join_span :: Span -> Span -> Span
join_span (Span s1 b1 e1) (Span s2 b2 e2) =
    if all ((file_of_loc s1==) . file_of_loc) all_locs
    then Span minsp maxbefore maxsp
    else error "join two spans where some locations have different files"
    where
        all_locs = [s1, e1, s2, e2]
        comparator = compare `on` ind_of_loc
        minsp = minimumBy comparator [s1, s2]
        maxsp = maximumBy comparator [e1, e2]
        maxbefore = maximumBy comparator [b1, b2]
-- is_single_line {{{1
is_single_line :: Span -> Bool
is_single_line (Span (Location _ _ (LineCol start_line _)) (Location _ _ (LineCol before_end_line _)) _) = start_line == before_end_line
-- fmt functions {{{1
fmt_location :: Location -> String
fmt_location (Location file _ linecol) = file_name file ++ ":" ++ fmt_linecol linecol

fmt_linecol :: LineCol -> String
fmt_linecol (LineCol line col) = show line ++ ":" ++ show col

fmt_span :: Span -> String
fmt_span (Span (Location sfile _ slinecol) _ (Location efile _ elinecol)) =
    if sfile /= efile
    then error "span that spans over different files"
    else file_name sfile ++ ":(" ++ fmt_linecol slinecol ++ " " ++ fmt_linecol elinecol ++ ")"
