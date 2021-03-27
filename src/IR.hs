module IR
    ( Unit(..)
    ) where

import File

import qualified AST(BinOp, UnaryOp)

import Data.Map(Map)
import qualified Data.Map as Map

type StrMap = Map String

data Mutability
    = Mutable
    | Immutable

data Unit = Unit File Module

data Module = Module (StrMap DeclSymbol)

data Type
    = FloatType (StrMap DeclSymbol) Int
    | IntType (StrMap DeclSymbol) Int Bool
    | CharType (StrMap DeclSymbol)
    | BoolType (StrMap DeclSymbol)
    | FunctionType (StrMap DeclSymbol) Type [(Mutability, Type)]
    | VoidType (StrMap DeclSymbol)
    | PointerType (StrMap DeclSymbol) Bool Type
    | GenericIntType (StrMap DeclSymbol)
    | GenericFloatType (StrMap DeclSymbol)

data DeclSymbol
    = DSModule Module
    | DSType Type

-- DeclSymbol stuff {{{1
getValues :: DeclSymbol -> StrMap ()
getValues _ = error "values not implemented yet"

getDeclSymbols :: DeclSymbol -> StrMap DeclSymbol
getDeclSymbols (DSModule (Module dsmap)) = dsmap
getDeclSymbols (DSType (FloatType dsmap _)) = dsmap
getDeclSymbols (DSType (IntType dsmap _ _)) = dsmap
getDeclSymbols (DSType (CharType dsmap)) = dsmap
getDeclSymbols (DSType (BoolType dsmap)) = dsmap
getDeclSymbols (DSType (FunctionType dsmap _ _)) = dsmap
getDeclSymbols (DSType (VoidType dsmap)) = dsmap
getDeclSymbols (DSType (PointerType dsmap _ _)) = dsmap
getDeclSymbols (DSType (GenericIntType dsmap)) = dsmap
getDeclSymbols (DSType (GenericFloatType dsmap)) = dsmap

getValue :: DeclSymbol -> String -> Maybe ()
getValue ds n = Map.lookup n $ getValues ds

getDeclSymbol :: DeclSymbol -> String -> Maybe DeclSymbol
getDeclSymbol ds n = Map.lookup n $ getDeclSymbols ds

-- TODO:
-- addValue =
-- addDeclSymbol =
