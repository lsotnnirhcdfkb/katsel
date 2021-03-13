module Location
    ( Location
    , Span
    , Located(..)
    ) where

import File

newtype SourceIndex = SourceIndex Int
newtype ColumnNum = ColumnNum Int
newtype LineNum = LineNum Int

data Location = Location File SourceIndex LineNum ColumnNum
instance Show Location where
    show (Location file _ (LineNum lnnr) (ColumnNum coln)) =
        name file ++ ":" ++ show lnnr ++ show coln

data Span = Span Location Int
instance Show Span where
    show (Span start len) = show start ++ "+" ++ show len

data Located a = Located Span a
instance Show a => Show (Located a) where
    show (Located sp a) = "<" ++ show sp ++ ": " ++ show a ++ ">"
