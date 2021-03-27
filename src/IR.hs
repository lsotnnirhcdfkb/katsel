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

data Module = Module (StrMap DeclSymbol) (StrMap Value)

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

data Value
    = VFunction Function
    | VRegister Register
    | VConstInt Integer
    | VConstFloat Double
    | VConstBool Bool
    | VConstChar Char
    | VVoid
    | VInstruction Instruction

data Function
    = Function
      -- { functionBlocks :: [BasicBlock]
      { functionRegisters :: [Register]
      , functionRetReg :: Int
      , functionParamRegs :: [Int]
      , functionType :: Type
      }
data Register = Register Type Bool
data Instruction = Instruction

-- DeclSymbol stuff {{{1
getValues :: DeclSymbol -> StrMap Value
getValues (DSType (FloatType _ _)) = Map.empty
getValues (DSType (IntType _ _ _)) = Map.empty
getValues (DSType (CharType _)) = Map.empty
getValues (DSType (BoolType _)) = Map.empty
getValues (DSType (FunctionType _ _ _)) = Map.empty
getValues (DSType (VoidType _)) = Map.empty
getValues (DSType (PointerType _ _ _)) = Map.empty
getValues (DSType (GenericIntType _)) = Map.empty
getValues (DSType (GenericFloatType _)) = Map.empty
getValues (DSModule (Module _ vmap)) = vmap

getDeclSymbols :: DeclSymbol -> StrMap DeclSymbol
getDeclSymbols (DSModule (Module dsmap _)) = dsmap
getDeclSymbols (DSType (FloatType dsmap _)) = dsmap
getDeclSymbols (DSType (IntType dsmap _ _)) = dsmap
getDeclSymbols (DSType (CharType dsmap)) = dsmap
getDeclSymbols (DSType (BoolType dsmap)) = dsmap
getDeclSymbols (DSType (FunctionType dsmap _ _)) = dsmap
getDeclSymbols (DSType (VoidType dsmap)) = dsmap
getDeclSymbols (DSType (PointerType dsmap _ _)) = dsmap
getDeclSymbols (DSType (GenericIntType dsmap)) = dsmap
getDeclSymbols (DSType (GenericFloatType dsmap)) = dsmap

getValue :: DeclSymbol -> String -> Maybe Value
getValue ds n = Map.lookup n $ getValues ds

getDeclSymbol :: DeclSymbol -> String -> Maybe DeclSymbol
getDeclSymbol ds n = Map.lookup n $ getDeclSymbols ds

-- TODO:
-- addValue =
-- addDeclSymbol =
