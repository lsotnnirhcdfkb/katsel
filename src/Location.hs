module Location
    ( Location
    , Span
    , asRowCol
    ) where

import File

newtype SourceIndex = SourceIndex Int
newtype ColumnNum = ColumnNum Int
newtype LineNum = LineNum Int

data Location = Location File SourceIndex LineNum ColumnNum

data Span = Span Location Int

asRowCol :: Location -> String
asRowCol (Location file _ (LineNum lnnr) (ColumnNum coln)) =
    name file ++ ":" ++ show lnnr ++ show coln
