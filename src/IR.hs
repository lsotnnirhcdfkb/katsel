{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module IR
    ( buildIR
    , Module
    ) where

import qualified AST

import Location

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(foldl')

import Data.Typeable(Typeable, cast)

-- utility types and aliases {{{1
type StrMap = Map String
type DSMap = StrMap DeclSymbol
type VMap = StrMap Value

data Mutability = Mutable | Immutable
data Signedness = Signed | Unsigned
-- IRId types and functions {{{1
newtype DSIRId resolve = DSIRId [String]
data VIRId resolve = VIRId (DSIRId DeclSymbol) String

newDSIRId :: [String] -> resolve -> DSIRId resolve
newDSIRId segments _ = DSIRId segments
newVIRId :: [String] -> resolve -> VIRId resolve
newVIRId segments _ = VIRId (DSIRId $ init segments) (last segments)

dsresolve :: Typeable r => Module -> DSIRId r -> Maybe r
dsresolve parentmod (DSIRId path) =
    foldl' next (Just $ DeclSymbol parentmod) path >>= cast
    where
        next (Just ds) name = getDeclSymbol ds name
        next Nothing _ = Nothing

vresolve :: Typeable r => Module -> VIRId r -> Maybe r
vresolve parentmod (VIRId parent childname) =
    child >>= cast
    where
        parentResolved = dsresolve parentmod parent
        child = parentResolved >>= flip getValue childname
-- IR datatypes {{{1
-- DeclSymbols {{{2
data DeclSymbol where
    DeclSymbol :: (Typeable d, DSChildren d, VChildren d) => d -> DeclSymbol

data Module = Module DSMap VMap
data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap (DSIRId Type) [(Mutability, DSIRId Type)]
    | VoidType DSMap
    | PointerType DSMap Mutability (DSIRId Type)
-- classes {{{3
class DSChildren d where
    getDSMap :: d -> DSMap
    getDeclSymbol :: d -> String -> Maybe DeclSymbol
    addDeclSymbol :: d -> String -> DeclSymbol -> d

    getDeclSymbol d n = Map.lookup n $ getDSMap d

class VChildren v where
    getVMap :: v -> VMap
    getValue :: v -> String -> Maybe Value
    addValue :: v -> String -> Value -> v

    getValue v n = Map.lookup n $ getVMap v
-- instances {{{3
-- DeclSymbol {{{3
instance DSChildren DeclSymbol where
    getDSMap (DeclSymbol d) = getDSMap d
    addDeclSymbol (DeclSymbol ds) name child = DeclSymbol $ addDeclSymbol ds name child
instance VChildren DeclSymbol where
    getVMap (DeclSymbol d) = getVMap d
    addValue (DeclSymbol ds) name child = DeclSymbol $ addValue ds name child
-- Type {{{4
instance DSChildren Type where
    getDSMap (FloatType dsmap _) = dsmap
    getDSMap (IntType dsmap _ _) = dsmap
    getDSMap (CharType dsmap) = dsmap
    getDSMap (BoolType dsmap) = dsmap
    getDSMap (FunctionType dsmap _ _) = dsmap
    getDSMap (VoidType dsmap) = dsmap
    getDSMap (PointerType dsmap _ _) = dsmap
-- Module {{{4
instance DSChildren Module where
    getDSMap (Module dsmap _) = dsmap
instance VChildren Module where
    getVMap (Module _ vmap) = vmap
-- Values {{{2
data Value where
    Value :: (Typeable v) => v -> Value
-- Function {{{
data Function
    = Function
      { functionBlocks :: [BasicBlock]
      , functionRegisters :: [Register]
      , functionRetReg :: Int
      , functionParamRegs :: [Int]
      , functionType :: DSIRId Type
      }
data BasicBlock = BasicBlock [Instruction] (Maybe Br)
data Register = Register (DSIRId Type) Mutability
data Instruction
    = Copy Register FValue
    | Call Function [FValue]
    | Addrof Register Mutability
    | DerefPtr FValue

data FValue
    = FVGlobalValue Value
    | FVRegister Register
    | FVConstInt Integer
    | FVConstFloat Double
    | FVConstBool Bool
    | FVConstChar Char
    | FVVoid
    | FVInstruction Instruction

data Br
    = BrRet
    | BrGoto BasicBlock
    | BrCond FValue BasicBlock BasicBlock
-- }}}
-- building the IR {{{1
buildIR :: AST.LDModule -> Module
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
    ddeclare :: p -> l -> p
    ddefine :: p -> l -> p
    vdeclare :: p -> l -> p
    vdefine :: p -> l -> p

class Parent p c i | p c -> i where
    add :: p -> i -> c -> p
    get :: p -> i -> Maybe c

type ModParent = Maybe Module

instance Parent ModParent Module () where
    add _ _ m = Just m
    get m _ = m

instance Parent DeclSymbol DeclSymbol String where
    add = addDeclSymbol
    get = getDeclSymbol
instance Parent DeclSymbol Value String where
    add = addValue
    get = getValue
instance Parent Module DeclSymbol String where
    add = addDeclSymbol
    get = getDeclSymbol
instance Parent Module Value String where
    add = addValue
    get = getValue

lowerAllInList :: Lowerable l p => [l] -> p -> (p -> l -> p) -> p
lowerAllInList things parent fn = foldl' fn parent things

instance Parent p Module () => Lowerable AST.LDModule p where
    ddeclare parent (Located _ (AST.DModule' decls)) = add parent () $ lowerAllInList decls startModule ddeclare
        where
            startModule = Module Map.empty Map.empty

    ddefine parent (Located _ (AST.DModule' decls)) = add parent () $ lowerAllInList decls parentmod ddefine
        where
            (Just parentmod) = get parent () :: Maybe Module -- not sure why this type annotation is needed to compile

    vdeclare parent (Located _ (AST.DModule' decls)) = add parent () $ lowerAllInList decls parentmod vdeclare
        where
            (Just parentmod) = get parent () :: Maybe Module

    vdefine parent (Located _ (AST.DModule' decls)) = add parent () $ lowerAllInList decls parentmod vdefine
        where
            (Just parentmod) = get parent () :: Maybe Module

instance Parent p Value String => Lowerable AST.LDDecl p where
    ddeclare _ (Located _ (AST.DDecl'Fun _)) = undefined
    ddeclare _ (Located _ (AST.DDecl'Impl _ _)) = undefined

    ddefine = undefined
    vdeclare = undefined
    vdefine = undefined
