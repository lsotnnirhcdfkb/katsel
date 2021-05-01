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

data Span = Span Location Location
    deriving Eq
fmt_span :: Span -> String
fmt_span (Span (Location sfile _ slnnr scoln) (Location efile _ elnnr ecoln)) =
    if sfile /= efile
    then error "span that spans over different files"
    else name sfile ++ ":(" ++ show slnnr ++ ":" ++ show scoln ++ " " ++ show elnnr ++ ":" ++ show ecoln ++ ")"

join_span :: Span -> Span -> Span
join_span (Span s1 e1) (Span s2 e2) =
    if all (==(file_of_loc s1)) $ map file_of_loc all_locs
    then Span minsp maxsp
    else error "join two spans where some locations are different to others"
    where
        all_locs = [s1, e1, s2, e2]
        minsp = minimumBy (compare `on` ind_of_loc) all_locs
        maxsp = maximumBy (compare `on` ind_of_loc) all_locs

data Located a = Located Span a
    deriving Eq

make_location :: File -> Int -> Location
make_location file srci =
    if srci < 0
    then error "boo"
    else Location file srci (getlnn file srci) (getcoln file srci)

getlnn :: File -> Int -> Int
getlnn file ind = 1 + length (filter ('\n'==) (take ind $ source file))

getcoln :: File -> Int -> Int
getcoln file ind = 1 + length (takeWhile (/='\n') $ reverse $ take ind $ source file)
