module IR.MapSynonyms
    ( StrMap
    , DSMap
    , VMap
    ) where

import Data.Map(Map)

import IR.DeclSymbol
import IR.Value

type StrMap = Map String
type DSMap = StrMap DeclSymbol
type VMap = StrMap Value
