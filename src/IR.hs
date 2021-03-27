module IR
    ( Unit(..)
    ) where

import File

import Data.Map(Map)
import qualified Data.Map as Map

type StrMap = Map String

data Unit = Unit File Module

data Module = Module (StrMap DeclSymbol)

data Type = Type (StrMap DeclSymbol)

data DeclSymbol
    = DSModule Module
    | DSType Type

data Value

getValues :: DeclSymbol -> StrMap ()
getValues (DSModule (Module map)) = error "values not implemented yet"
getValues (DSType (Type map)) = error "values not implemented yet"

getDeclSymbols :: DeclSymbol -> StrMap DeclSymbol
getDeclSymbols (DSModule (Module dsmap)) = dsmap
getDeclSymbols (DSType (Type dsmap)) = dsmap

getValue :: DeclSymbol -> String -> Maybe ()
getValue ds n = Map.lookup n $ getValues ds

getDeclSymbol :: DeclSymbol -> String -> Maybe DeclSymbol
getDeclSymbol ds n = Map.lookup n $ getDeclSymbols ds

-- TODO:
-- addValue =
-- addDeclSymbol =
