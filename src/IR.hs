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

import ErrorAcc

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(foldl')

import Data.Typeable(Typeable, cast)

import Control.Monad.State.Lazy(State, state, execState)

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
        next (Just ds) name = getDeclSymbol name ds
        next Nothing _ = Nothing

vresolve :: Typeable r => Module -> VIRId r -> Maybe r
vresolve parentmod (VIRId parent childname) =
    child >>= cast
    where
        parentResolved = dsresolve parentmod parent
        child = parentResolved >>= getValue childname
-- IR datatypes {{{1
-- DeclSymbols {{{2
data DeclSymbol where
    DeclSymbol :: (Typeable d, DSChildren d, VChildren d) => d -> DeclSymbol

data Module = Module DSMap VMap

newtype TyIdx = TyIdx Int
data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap TyIdx [(Mutability, TyIdx)]
    | VoidType DSMap
    | PointerType DSMap Mutability TyIdx
-- classes {{{3
class DSChildren d where
    getDSMap :: d -> DSMap
    getDeclSymbol :: String -> d -> Maybe DeclSymbol
    addDeclSymbol :: String -> DeclSymbol -> d -> d

    getDeclSymbol n d = Map.lookup n $ getDSMap d

class VChildren v where
    getVMap :: v -> VMap
    getValue :: String -> v -> Maybe Value
    addValue :: String -> Value -> v -> v

    getValue n v = Map.lookup n $ getVMap v
-- instances {{{3
-- DeclSymbol {{{3
instance DSChildren DeclSymbol where
    getDSMap (DeclSymbol d) = getDSMap d
    addDeclSymbol name child (DeclSymbol ds) = DeclSymbol $ addDeclSymbol name child ds
instance VChildren DeclSymbol where
    getVMap (DeclSymbol d) = getVMap d
    addValue name child (DeclSymbol ds) = DeclSymbol $ addValue name child ds
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
      , functionType :: TyIdx
      }
data BasicBlock = BasicBlock [Instruction] (Maybe Br)
data Register = Register TyIdx Mutability
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
        (loweredMod, tyctx) = vdefine lmod . vdeclare lmod . ddefine lmod . ddeclare lmod $ (Nothing, TyCtx [])
-- helper functions {{{2
lowerAllInList :: Lowerable l p => [l] -> (l -> (p, TyCtx) -> (p, TyCtx)) -> (p, TyCtx) -> (p, TyCtx)
lowerAllInList things fun parent = foldl' (flip fun) parent things

addFstToParent :: Parent p c i => i -> (c, TyCtx) -> p -> (p, TyCtx)
addFstToParent ind (child, tyctx) parent = (add ind child parent, tyctx)

notStateToUnitRes :: (a -> a) -> (a -> ((), a))
notStateToUnitRes fun thing = ((), thing)

tyCtxStateFunToCGTupleFun :: (TyCtx -> (r, TyCtx)) -> ((p, TyCtx) -> (r, (p, TyCtx)))
tyCtxStateFunToCGTupleFun fun (p, tyctx) =
    let (res, resTyCtx) = fun tyctx
    in (res, (p, resTyCtx))

parentStateFunToCGTupleFun :: (p -> (r, p)) -> ((p, TyCtx) -> (r, (p, TyCtx)))
parentStateFunToCGTupleFun fun (p, tyctx) =
    let (res, resP) = fun p
    in (res, (resP, tyctx))
-- type resolution & type interning {{{1
resolveTy :: AST.LDType -> TyCtx -> (TyIdx, TyCtx)
resolveTy = error "not implemented yet"

addTy :: Type -> TyCtx -> (TyIdx, TyCtx)
addTy ty (TyCtx tys) = (TyIdx $ length tys, TyCtx $ tys ++ [ty])

getVoidType :: TyCtx -> (TyIdx, TyCtx)
getVoidType = error "not implemented yet"
-- Lowerable class {{{2
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> (p, TyCtx) -> (p, TyCtx)
-- Parent class {{{2
class Parent p c i | p c -> i where
    add :: i -> c -> p -> p
    get :: i -> p -> Maybe c
-- lowering modules {{{2
type ModParent = Maybe Module
-- parent instances {{{
instance Parent ModParent Module () where
    add _ mod _ = Just mod
    get _ m = m
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
    ddeclare (Located _ (AST.DModule' decls)) (parent, tyCtx) =
        let startModule = Module Map.empty Map.empty
        in addFstToParent () (lowerAllInList decls ddeclare (startModule, tyCtx)) parent

    ddefine (Located _ (AST.DModule' decls)) (parent, tyCtx) =
        let (Just parentmod) = get () parent :: Maybe Module
        in addFstToParent () (lowerAllInList decls ddefine (parentmod, tyCtx)) parent

    vdeclare (Located _ (AST.DModule' decls)) (parent, tyCtx) =
        let (Just parentmod) = get () parent :: Maybe Module
        in addFstToParent () (lowerAllInList decls vdeclare (parentmod, tyCtx)) parent

    vdefine (Located _ (AST.DModule' decls)) (parent, tyCtx) =
        let (Just parentmod) = get () parent :: Maybe Module
        in addFstToParent () (lowerAllInList decls vdefine (parentmod, tyCtx)) parent
-- lowering functions {{{2
instance Parent p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ = id
    ddefine _ = id

    vdeclare (Located _ (AST.SFunDecl' mretty (Located _ name) params expr)) = execState $
        (state . tyCtxStateFunToCGTupleFun $ case mretty of
            Just retty -> resolveTy retty
            Nothing -> getVoidType
        ) >>= \ retty' ->
        let makeParam :: AST.LDParam -> State (p, TyCtx) (Mutability, TyIdx)
            makeParam = undefined
        in sequence (map makeParam params) >>= \ paramTys ->
        let newty = FunctionType Map.empty retty' paramTys
        in (state . tyCtxStateFunToCGTupleFun) (addTy newty) >>= \ funtyIdx ->
        let fun = Function
                  { functionBlocks = []
                  , functionRegisters = []
                  , functionRetReg = 0
                  , functionParamRegs = []
                  , functionType = funtyIdx
                  }
        in state . parentStateFunToCGTupleFun . notStateToUnitRes $ add name (Value fun)

    -- vdefine (Located _ (AST.SFunDecl' retty (Located _ name) params expr)) parent = error "not implemented yet"
    vdefine = error "not implemented yet"
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
