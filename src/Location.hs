module Location
    ( Location
    , Span(..)
    , Located(..)
    , make_location
    , join_span
    , fmt_location
    , fmt_span
    , ind_of_loc
    , lnn_of_loc
    , coln_of_loc
    , file_of_loc
    ) where

import File
import Data.List(minimumBy, maximumBy)
import Data.Function(on)

data Location
    = Location
      { file_of_loc :: File
      , ind_of_loc :: Int
      , lnn_of_loc :: Int
      , coln_of_loc :: Int
      }
    deriving Eq

fmt_location :: Location -> String
fmt_location (Location file _ lnnr coln) = name file ++ ":" ++ show lnnr ++ ":" ++ show coln

data Span = Span Location Location Location
    deriving Eq
fmt_span :: Span -> String
fmt_span (Span (Location sfile _ slnnr scoln) _ (Location efile _ elnnr ecoln)) =
    if sfile /= efile
    then error "span that spans over different files"
    else name sfile ++ ":(" ++ show slnnr ++ ":" ++ show scoln ++ " " ++ show elnnr ++ ":" ++ show ecoln ++ ")"

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

data Located a
    = Located
      { just_span :: Span
      , unlocate :: a
      }
    deriving Eq

make_location :: File -> Int -> Int -> Int -> Location
make_location = Location
