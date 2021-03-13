module Location
    ( Location(..)
    , Span(..)
    , Located(..)
    , makeLocation
    , makeSpan
    , lineNumOfLocation
    , colNumOfLocation
    , endLocationOfSpan
    ) where

import File

newtype SourceIndex = SourceIndex Int
newtype ColumnNum = ColumnNum Int
newtype LineNum = LineNum Int

data Location = Location File SourceIndex LineNum ColumnNum
instance Show Location where
    show (Location file _ (LineNum lnnr) (ColumnNum coln)) =
        name file ++ ":" ++ show lnnr ++ ":" ++ show coln

data Span = Span Location Int LineNum ColumnNum
instance Show Span where
    show (Span start len _ _) = show start ++ "+" ++ show len

data Located a = Located Span a
instance Show a => Show (Located a) where
    show (Located sp a) = "<" ++ show sp ++ ": " ++ show a ++ ">"

makeLocation :: File -> Int -> Int -> Int -> Location
makeLocation file srci lnn coln = Location file (SourceIndex srci) (LineNum lnn) (ColumnNum coln)

makeSpan :: File -> Int -> Int -> Int -> Int -> Int -> Int -> Span
makeSpan file srci slnn scoln len elnn ecoln = Span (makeLocation file srci slnn scoln) len (LineNum elnn) (ColumnNum ecoln)

lineNumOfLocation :: Location -> Int
lineNumOfLocation  (Location _ _ (LineNum nr) _) = nr

colNumOfLocation :: Location -> Int
colNumOfLocation  (Location _ _ _ (ColumnNum cr)) = cr

endLocationOfSpan :: Span -> Location
endLocationOfSpan (Span (Location file (SourceIndex ind) _ _) len endlnr endcoln) = Location file (SourceIndex $ ind + len) endlnr endcoln
