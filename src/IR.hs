module IR
    ( Unit(..)
    ) where

import File

import qualified AST(BinOp, UnaryOp)

import Data.Map(Map)
import qualified Data.Map as Map

type StrMap = Map String

data Mutability = Mutable | Immutable
data Signedness = Signed | Unsigned

data Unit = Unit File Module

data Module = Module (StrMap DeclSymbol) (StrMap Value)

data Type
    = FloatType (StrMap DeclSymbol) Int
    | IntType (StrMap DeclSymbol) Int Signedness
    | CharType (StrMap DeclSymbol)
    | BoolType (StrMap DeclSymbol)
    | FunctionType (StrMap DeclSymbol) Type [(Mutability, Type)]
    | VoidType (StrMap DeclSymbol)
    | PointerType (StrMap DeclSymbol) Mutability Type

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
      { functionBlocks :: [BasicBlock]
      , functionRegisters :: [Register]
      , functionRetReg :: Int
      , functionParamRegs :: [Int]
      , functionType :: Type
      }
data BasicBlock = BasicBlock [Instruction] (Maybe Br)

data Register = Register Type Mutability

data Instruction
    = Copy Register Value
    | Call Function [Value]
    | Addrof Register Mutability
    | DerefPtr Value

data Br
    = BrRet
    | BrGoto BasicBlock
    | BrCond Value BasicBlock BasicBlock

-- DeclSymbol stuff {{{1
getValues :: DeclSymbol -> StrMap Value
getValues (DSType (FloatType _ _)) = Map.empty
getValues (DSType (IntType _ _ _)) = Map.empty
getValues (DSType (CharType _)) = Map.empty
getValues (DSType (BoolType _)) = Map.empty
getValues (DSType (FunctionType _ _ _)) = Map.empty
getValues (DSType (VoidType _)) = Map.empty
getValues (DSType (PointerType _ _ _)) = Map.empty
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

getValue :: DeclSymbol -> String -> Maybe Value
getValue ds n = Map.lookup n $ getValues ds

getDeclSymbol :: DeclSymbol -> String -> Maybe DeclSymbol
getDeclSymbol ds n = Map.lookup n $ getDeclSymbols ds

-- TODO:
-- addValue =
-- addDeclSymbol =
