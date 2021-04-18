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

newtype TyCtx = TyCtx [Type]
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

data Module = Module DSMap VMap TyCtx

newtype TyIdx = TyIdx Int
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
    getDSMap (Module dsmap _ _) = dsmap
instance VChildren Module where
    getVMap (Module _ vmap _) = vmap
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
        loweredMod = vdefine lmod . vdeclare lmod .  ddefine lmod .  ddeclare lmod $ Nothing
-- helper functions {{{2
lowerAllInList :: Lowerable l p => [l] -> p -> (l -> p -> p) -> p
lowerAllInList things parent fn = foldl' (flip fn) parent things
-- Lowerable class {{{2
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> p -> p
-- Parent class {{{2
class Parent p c i | p c -> i where
    add :: p -> i -> c -> p
    get :: p -> i -> Maybe c
-- lowering modules {{{2
type ModParent = Maybe Module
-- parent instances {{{
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
-- }}}
instance Parent p Module () => Lowerable AST.LDModule p where
    ddeclare (Located _ (AST.DModule' decls)) parent = add parent () $ lowerAllInList decls startModule ddeclare
        where
            startModule = Module Map.empty Map.empty $ TyCtx []

    ddefine (Located _ (AST.DModule' decls)) parent = add parent () $ lowerAllInList decls parentmod ddefine
        where
            (Just parentmod) = get parent () :: Maybe Module -- not sure why this type annotation is needed to compile

    vdeclare (Located _ (AST.DModule' decls)) parent = add parent () $ lowerAllInList decls parentmod vdeclare
        where
            (Just parentmod) = get parent () :: Maybe Module

    vdefine (Located _ (AST.DModule' decls)) parent = add parent () $ lowerAllInList decls parentmod vdefine
        where
            (Just parentmod) = get parent () :: Maybe Module
-- lowering functions {{{2
instance Parent p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ parent = parent
    ddefine _ parent = parent

    vdeclare (Located _ (AST.SFunDecl' retty (Located _ name) params expr)) parent = add parent name fun
        where
            retty = resolveTy retty
            newty = FunctionType Map.empty retty (map makeParam params)
            makeParam (AST.DParam'Normal mutability ty _) = (mutability, ty)

            fun = error "not implemented yet" :: Value
    vdefine (Located _ (AST.SFunDecl' retty (Located _ name) params expr)) parent = error "not implemented yet"
-- lowering declarations {{{2
instance Parent p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) parent = ddeclare sf parent
    ddeclare (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"

    ddefine (Located _ (AST.DDecl'Fun sf)) parent = ddefine sf parent
    ddefine (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"

    vdeclare (Located _ (AST.DDecl'Fun sf)) parent = vdeclare sf parent
    vdeclare (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"

    vdefine (Located _ (AST.DDecl'Fun sf)) parent = vdefine sf parent
    vdefine (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"
