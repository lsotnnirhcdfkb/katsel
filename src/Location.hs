module Location
    ( Location
    , Span(..)
    , Located(..)
    , makeLocation
    , joinSpan
    , fmtLocation
    , fmtSpan
    , indOfLoc
    , lnnOfLoc
    , colnOfLoc
    , fileOfLoc
    ) where

import File
import Data.List(minimumBy, maximumBy)
import Data.Function(on)

data Location
    = Location
      { fileOfLoc :: File
      , indOfLoc :: Int
      , lnnOfLoc :: Int
      , colnOfLoc :: Int
      }
    deriving Eq

fmtLocation :: Location -> String
fmtLocation (Location file _ lnnr coln) = name file ++ ":" ++ show lnnr ++ ":" ++ show coln

data Span = Span Location Location
    deriving Eq
fmtSpan :: Span -> String
fmtSpan (Span (Location sfile _ slnnr scoln) (Location efile _ elnnr ecoln)) =
    if sfile /= efile
    then error "span that spans over different files"
    else name sfile ++ ":(" ++ show slnnr ++ ":" ++ show scoln ++ " " ++ show elnnr ++ ":" ++ show ecoln ++ ")"

joinSpan :: Span -> Span -> Span
joinSpan (Span s1 e1) (Span s2 e2) =
    if all (==(fileOfLoc $ head allLocs)) $ map fileOfLoc allLocs
    then Span minsp maxsp
    else error "join two spans where some locations are different to others"
    where
        allLocs = [s1, e1, s2, e2]
        minsp = minimumBy (compare `on` indOfLoc) allLocs
        maxsp = maximumBy (compare `on` indOfLoc) allLocs

data Located a = Located Span a
    deriving Eq

makeLocation :: File -> Int -> Location
makeLocation file srci =
    if srci < 0
    then error "boo"
    else Location file srci (getlnn file srci) (getcoln file srci)

getlnn :: File -> Int -> Int
getlnn file ind = 1 + length (filter ('\n'==) (take ind $ source file))

getcoln :: File -> Int -> Int
getcoln file ind = 1 + length (takeWhile (/='\n') $ reverse $ take ind $ source file)
