{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module IR
    ( getValue
    , getDeclSymbol
    , addValue
    , addDeclSymbol
    , buildIR
    ) where

import qualified AST

import Location

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(foldl')

data Empty
data DDeclared
data DDefined
data VDeclared
data Completed

type MakeEmpty a = a Empty
type MakeDDeclared a = a DDeclared
type MakeDDefined a = a DDefined
type MakeVDeclared a = a VDeclared
type MakeCompleted a = a Completed

type StrMap = Map String
type DSMap status = StrMap (DeclSymbol status)
type VMap status = StrMap (Value status)

data Mutability = Mutable | Immutable
data Signedness = Signed | Unsigned

data Module status = Module (DSMap status) (VMap status)

data Type status
    = FloatType (DSMap status) Int
    | IntType (DSMap status) Int Signedness
    | CharType (DSMap status)
    | BoolType (DSMap status)
    | FunctionType (DSMap status) (Type status) [(Mutability, Type status)]
    | VoidType (DSMap status)
    | PointerType (DSMap status) Mutability (Type status)

data DeclSymbol status
    = DSModule (Module status)
    | DSType (Type status)

data Value status
    = VFunction (Function status)
    | VRegister (Register status)
    | VConstInt Integer
    | VConstFloat Double
    | VConstBool Bool
    | VConstChar Char
    | VVoid
    | VInstruction (Instruction status)

data Function status
    = Function
      { functionBlocks :: [BasicBlock status]
      , functionRegisters :: [Register status]
      , functionRetReg :: Int
      , functionParamRegs :: [Int]
      , functionType :: Type status
      }
data BasicBlock status = BasicBlock [Instruction status] (Maybe (Br status))

data Register status = Register (Type status) Mutability

data Instruction status
    = Copy (Register status) (Value status)
    | Call (Function status) [Value status]
    | Addrof (Register status) Mutability
    | DerefPtr (Value status)

data Br status
    = BrRet
    | BrGoto (BasicBlock status)
    | BrCond (Value status) (BasicBlock status) (BasicBlock status)

-- DeclSymbol stuff {{{1
-- TODO: eventually types will have values (eg uint32::max)

getValues :: DeclSymbol dss -> VMap dss
getValues (DSType FloatType {}) = Map.empty
getValues (DSType IntType {}) = Map.empty
getValues (DSType CharType {}) = Map.empty
getValues (DSType BoolType {}) = Map.empty
getValues (DSType FunctionType {}) = Map.empty
getValues (DSType VoidType {}) = Map.empty
getValues (DSType PointerType {}) = Map.empty
getValues (DSModule (Module _ vmap)) = vmap

getDeclSymbols :: DeclSymbol dss -> DSMap dss
getDeclSymbols (DSModule (Module dsmap _)) = dsmap
getDeclSymbols (DSType (FloatType dsmap _)) = dsmap
getDeclSymbols (DSType (IntType dsmap _ _)) = dsmap
getDeclSymbols (DSType (CharType dsmap)) = dsmap
getDeclSymbols (DSType (BoolType dsmap)) = dsmap
getDeclSymbols (DSType (FunctionType dsmap _ _)) = dsmap
getDeclSymbols (DSType (VoidType dsmap)) = dsmap
getDeclSymbols (DSType (PointerType dsmap _ _)) = dsmap

getValue :: DeclSymbol dss -> String -> Maybe (Value dss)
getValue ds n = Map.lookup n $ getValues ds

getDeclSymbol :: DeclSymbol dss -> String -> Maybe (DeclSymbol dss)
getDeclSymbol ds n = Map.lookup n $ getDeclSymbols ds

addValue :: DeclSymbol dss -> Value dss -> DeclSymbol dss
addValue ds v = undefined

addDeclSymbol :: DeclSymbol dss -> Value dss -> DeclSymbol dss
addDeclSymbol = undefined

buildIR :: AST.LDModule -> IR.Module
buildIR lmod =
    case loweredMod of
        Just ir -> ir
        Nothing -> error "lowering ast to ir returned Nothing"
    where
        ddeclared = ddeclare Nothing lmod
        ddefined = ddefine ddeclared lmod
        vdeclared = vdeclare ddefined lmod
        vdefined = vdefine vdeclared lmod
        loweredMod = vdefined

class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: p -> l -> p
class Parent p c i | p c -> i where
    add :: p -> i -> c -> p
    get :: p -> i -> Maybe c

type ModParent = Maybe IR.Module
instance Parent ModParent IR.Module () where
    add _ _ m = Just m
    get m _ = m

instance Parent IR.DeclSymbol IR.DeclSymbol String where
    add = undefined
    get = undefined
instance Parent IR.DeclSymbol IR.Value String where
    add = undefined
    get = undefined
instance Parent IR.Module IR.Value String where
    add = undefined
    get = undefined
instance Parent IR.Module IR.DeclSymbol String where
    add = undefined
    get = undefined

instance Parent p IR.Module () => Lowerable AST.LDModule p where
    ddeclare parent (Located _ (AST.DModule' decls)) = add parent () finalModule
        where
            startModule = IR.Module Map.empty Map.empty
            finalModule = foldl' ddeclare startModule decls

    ddefine parent (Located _ (AST.DModule' decls)) = add parent () defined
        where
            (Just parentmod) = get parent () :: Maybe IR.Module -- not sure why this type annotation is needed to compile
            defined = foldl' ddefine parentmod decls

    vdeclare = undefined
    vdefine = undefined

instance Parent p IR.Value String => Lowerable AST.LDDecl p where
    ddeclare _ (Located _ (AST.DDecl'Fun _)) = undefined
    ddeclare _ (Located _ (AST.DDecl'Impl _ _)) = undefined

    ddefine = undefined
    vdeclare = undefined
    vdefine = undefined
