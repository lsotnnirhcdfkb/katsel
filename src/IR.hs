module IR
    ( Module(..)
    , Value(..)
    , Function(..)
    , DeclSymbol(..)
    , Type(..)
    , getValue
    , getDeclSymbol
    , addValue
    , addDeclSymbol
    ) where

import qualified AST(BinOp, UnaryOp)

import Data.Map(Map)
import qualified Data.Map as Map

type StrMap = Map String
type DSMap = StrMap DeclSymbol
type VMap = StrMap Value

data Mutability = Mutable | Immutable
data Signedness = Signed | Unsigned

data Module = Module DSMap VMap

data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap Type [(Mutability, Type)]
    | VoidType DSMap
    | PointerType DSMap Mutability Type

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
-- TODO: eventually types will have values (eg uint32::max)
getValues :: DeclSymbol -> StrMap Value
getValues (DSType FloatType {}) = Map.empty
getValues (DSType IntType {}) = Map.empty
getValues (DSType CharType {}) = Map.empty
getValues (DSType BoolType {}) = Map.empty
getValues (DSType FunctionType {}) = Map.empty
getValues (DSType VoidType {}) = Map.empty
getValues (DSType PointerType {}) = Map.empty
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

addValue :: DeclSymbol -> Value -> DeclSymbol
addValue ds v = undefined

addDeclSymbol :: DeclSymbol -> Value -> DeclSymbol
addDeclSymbol = undefined
