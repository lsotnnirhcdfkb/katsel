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

data Location
    = Location
      { fileOfLoc :: File
      , indOfLoc :: Int
      , lnnOfLoc :: Int
      , colnOfLoc :: Int
      }

fmtLocation :: Location -> String
fmtLocation (Location file _ lnnr coln) = name file ++ ":" ++ show lnnr ++ ":" ++ show coln

data Span = Span Location Location
fmtSpan :: Span -> String
fmtSpan (Span (Location sfile _ slnnr scoln) (Location efile _ elnnr ecoln)) =
    if sfile /= efile
    then error "span that spans over different files"
    else name sfile ++ ":(" ++ show slnnr ++ ":" ++ show scoln ++ " " ++ show elnnr ++ ":" ++ show ecoln ++ ")"

joinSpan :: Span -> Span -> Span
joinSpan (Span s _) (Span _ e) = Span s e

data Located a = Located Span a

makeLocation :: File -> Int -> Location
makeLocation file srci =
    if srci < 0
    then error "boo"
    else Location file srci (getlnn file srci) (getcoln file srci)

getlnn :: File -> Int -> Int
getlnn file ind = 1 + (length $ filter ('\n'==) (take ind $ source file))

getcoln :: File -> Int -> Int
getcoln file ind = 1 + (length $ takeWhile (/='\n') $ reverse $ take ind $ source file)
