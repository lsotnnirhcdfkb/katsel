module Location
    ( Location
    , Span(..)
    , Located(..)
    , makeLocation
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

data Located a = Located Span a

makeLocation :: File -> Int -> Location
makeLocation file srci = Location file srci (getlnn file srci) (getcoln file srci)

getlnn :: File -> Int -> Int
getlnn file ind =
    case drop ind $ source file of
        "\n" -> getlnn file (ind-1)
        _ -> 1 + (length $ filter ('\n'==) (take (ind + 1) $ source file))

getcoln :: File -> Int -> Int
getcoln = helper (1 :: Int)
    where
        helper acc file ind =
            case reverse $ take ind $ source file of
                '\n':_ -> acc
                _ -> helper (acc + 1) file (ind - 1)
