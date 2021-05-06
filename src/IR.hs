{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module IR
    ( build_ir
    , Module
    , TyCtx
    ) where

import qualified AST

import qualified Message

import Location

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(foldl')

import Data.Typeable(Typeable, cast)

import Control.Monad.State.Lazy(state, execState)

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

new_dsirid :: [String] -> resolve -> DSIRId resolve
new_dsirid segments _ = DSIRId segments
new_virid :: [String] -> resolve -> VIRId resolve
new_virid segments _ = VIRId (DSIRId $ init segments) (last segments)

dsresolve :: Typeable r => Module -> DSIRId r -> Maybe r
dsresolve parentmod (DSIRId path) =
    foldl' next (Just $ DeclSymbol parentmod) path >>= cast
    where
        next (Just ds) name = get name ds :: Maybe DeclSymbol
        next Nothing _ = Nothing

vresolve :: Typeable r => Module -> VIRId r -> Maybe r
vresolve parentmod (VIRId parent childname) =
    child >>= cast
    where
        parent_resolved = dsresolve parentmod parent
        child = parent_resolved >>= get childname :: Maybe Value
-- IR datatypes {{{1
-- Parent things {{{2
class Ord i => ParentR p c i | p c -> i where
    get_child_map :: p -> Map i c
    get :: i -> p -> Maybe c

    get ind parent = Map.lookup ind (get_child_map parent)

class Ord i => ParentW p c i | p c -> i where
    add :: i -> c -> p -> p

newtype IRRO a = IRRO a
newtype IRWO a = IRWO a

instance (Ord i, ParentR a c i) => ParentR (IRRO a) (IRRO c) i where
    get_child_map (IRRO ro) = Map.map IRRO $ get_child_map ro

instance (Ord i, ParentW a c i) => ParentW (IRWO a) (IRWO c) i where
    add ind (IRWO child) (IRWO wo) = IRWO $ add ind child wo

type ParentRW p c i = (ParentR p c i, ParentW p c i)
-- DeclSymbols {{{2
data DeclSymbol where
    DeclSymbol :: (Typeable d,
            ParentR d DeclSymbol String, ParentW d DeclSymbol String,
            ParentR d Value String,      ParentW d Value String) => d -> DeclSymbol

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
-- instances {{{3
-- DeclSymbol {{{4
instance ParentR DeclSymbol DeclSymbol String where
    get_child_map (DeclSymbol d) = get_child_map d
instance ParentW DeclSymbol DeclSymbol String where
    add name child (DeclSymbol ds) = DeclSymbol $ add name child ds

instance ParentR DeclSymbol Value String where
    get_child_map (DeclSymbol d) = get_child_map d
instance ParentW DeclSymbol Value String where
    add name child (DeclSymbol ds) = DeclSymbol $ add name child ds
-- Type {{{4
instance ParentR Type DeclSymbol String where
    get_child_map (FloatType dsmap _) = dsmap
    get_child_map (IntType dsmap _ _) = dsmap
    get_child_map (CharType dsmap) = dsmap
    get_child_map (BoolType dsmap) = dsmap
    get_child_map (FunctionType dsmap _ _) = dsmap
    get_child_map (VoidType dsmap) = dsmap
    get_child_map (PointerType dsmap _ _) = dsmap
-- Module {{{4
instance ParentR Module DeclSymbol String where
    get_child_map (Module dsmap _) = dsmap
instance ParentR Module Value String where
    get_child_map (Module _ vmap) = vmap

instance ParentW Module DeclSymbol String where
instance ParentW Module Value String where
-- Values {{{2
data Value where
    Value :: (Typeable v) => v -> Value
-- Function {{{
data Function
    = Function
      { get_function_blocks :: [BasicBlock]
      , get_function_registers :: [Register]
      , get_function_ret_reg :: Int
      , get_function_param_regs :: [Int]
      , get_function_type :: TyIdx
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
-- IRBuildError {{{
data IRBuildError
instance Message.ToDiagnostic IRBuildError where
    to_diagnostic = error "not implemented yet"
-- }}}
-- IRBuilder {{{
data IRBuilder = IRBuilder TyCtx [IRBuildError]

add_error :: IRBuildError -> IRBuilder -> IRBuilder
add_error err (IRBuilder tyctx errs) = IRBuilder tyctx (errs ++ [err])
-- }}}
build_ir :: AST.LDModule -> (Module, TyCtx, [IRBuildError])
build_ir mod_ast = (lowered_mod, tyctx, errors)
    where
        apply_stage :: (AST.LDModule -> IRRO ModParent -> IRRO Module -> IRDiff (IRWO ModParent)) -> (ModParent, IRBuilder) -> (ModParent, IRBuilder)
        apply_stage fun (mod_parent@(ModParent module_), ir_builder) =
            let (IRWO module_', ir_builder') = fun mod_ast (IRRO mod_parent) (IRRO module_) (IRWO mod_parent, ir_builder)
            in (module_', ir_builder')

        initial_parent_builder_tup = (ModParent $ Module Map.empty Map.empty, IRBuilder (TyCtx []) [])
        (ModParent lowered_mod, IRBuilder tyctx errors) =
            apply_stage vdefine .
            apply_stage vdeclare .
            apply_stage ddefine .
            apply_stage ddeclare $
            initial_parent_builder_tup

-- helper functions {{{2
lower_all_in_list :: Lowerable l p => [l] -> (l -> IRRO p -> IRRO Module -> IRDiff (IRWO p)) -> IRRO p -> IRRO Module -> IRDiff (IRWO p)
lower_all_in_list things fun parent root = foldl' (.) id diffs
    where
        apply_fun thing = fun thing parent root
        diffs = map apply_fun things

ast_muty_to_ir_muty :: AST.Mutability -> Mutability
ast_muty_to_ir_muty AST.Immutable = Immutable
ast_muty_to_ir_muty AST.Mutable = Mutable

add_unit_res :: (s -> s) -> s -> ((), s)
add_unit_res fun thing = ((), fun thing)

parent_fun_to_cgtup_fun :: (p -> (r, p)) -> (p, IRBuilder) -> (r, (p, IRBuilder))
parent_fun_to_cgtup_fun fun (parent, ir_builder) =
    let (res, next_parent) = fun parent
    in (res, (next_parent, ir_builder))

irbuilder_fun_to_cgtup_fun :: (IRBuilder -> (r, IRBuilder)) -> (p, IRBuilder) -> (r, (p, IRBuilder))
irbuilder_fun_to_cgtup_fun fun (parent, ir_builder) =
    let (res, next_ir_builder) = fun ir_builder
    in (res, (parent, next_ir_builder))

ro_to_wo :: IRRO a -> IRWO a
ro_to_wo (IRRO a) = IRWO a
-- type resolution & type interning {{{2
resolve_ty :: AST.LDType -> IRRO Module -> (p, IRBuilder) -> (TyIdx, (p, IRBuilder))
resolve_ty = error "not implemented yet"

add_ty :: Type -> IRBuilder -> (TyIdx, IRBuilder)
add_ty ty (IRBuilder (TyCtx tys) errs) = (TyIdx $ length tys, IRBuilder (TyCtx $ tys ++ [ty]) errs)

get_void_type :: IRBuilder -> (TyIdx, IRBuilder)
get_void_type = error "not implemented yet"
-- Lowerable class {{{2
type IRDiff p = (p, IRBuilder) -> (p, IRBuilder)
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> IRRO p -> IRRO Module -> IRDiff (IRWO p)
-- lowering modules {{{2
newtype ModParent = ModParent Module
instance ParentR ModParent Module () where
    get_child_map (ModParent mp) = Map.fromList [((), mp)]
instance ParentW ModParent Module () where
    add _ m _ = ModParent m

instance ParentRW p Module () => Lowerable AST.LDModule p where
    ddeclare (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            module_diff = lower_all_in_list decls ddeclare module_ root
            (module_', ir_builder') = module_diff (ro_to_wo module_, ir_builder)
        in (add () module_' wo_parent, ir_builder')

    ddefine (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            module_diff = lower_all_in_list decls ddefine module_ root
            (module_', ir_builder') = module_diff (ro_to_wo module_, ir_builder)
        in (add () module_' wo_parent, ir_builder')

    vdeclare (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            module_diff = lower_all_in_list decls vdeclare module_ root
            (module_', ir_builder') = module_diff (ro_to_wo module_, ir_builder)
        in (add () module_' wo_parent, ir_builder')

    vdefine (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            module_diff = lower_all_in_list decls vdefine module_ root
            (module_', ir_builder') = module_diff (ro_to_wo module_, ir_builder)
        in (add () module_' wo_parent, ir_builder')
-- lowering functions {{{2
instance ParentRW p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ _ = id
    ddefine _ _ _ = id

    vdeclare (Located _ (AST.SFunDecl' mretty (Located _ name) params _)) roparent root = execState $
        (state $ case mretty of
            Just retty -> resolve_ty retty root
            Nothing -> irbuilder_fun_to_cgtup_fun get_void_type
        ) >>= \ retty' ->
        let make_param (Located _ (AST.DParam'Normal mutability ty_ast _)) =
                state (resolve_ty ty_ast root) >>= \ ty ->
                return (ast_muty_to_ir_muty mutability, ty)
        in sequence (map make_param params) >>= \ param_tys ->
        let newty = FunctionType Map.empty retty' param_tys
        in state (irbuilder_fun_to_cgtup_fun $ add_ty newty) >>= \ fun_ty_idx ->
        let fun = Function
                  { get_function_blocks = []
                  , get_function_registers = map (uncurry $ flip Register) param_tys
                  , get_function_ret_reg = 0
                  , get_function_param_regs = take (length params) [1..]
                  , get_function_type = fun_ty_idx
                  }
        in state $ parent_fun_to_cgtup_fun . add_unit_res $ add name $ IRWO $ Value fun

    vdefine (Located _ (AST.SFunDecl' retty (Located _ name) params expr)) roparent root (woparent, ir_builder) = undefined
-- lowering declarations {{{2
instance ParentRW p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) parent = ddeclare sf parent
    ddeclare (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"

    ddefine (Located _ (AST.DDecl'Fun sf)) parent = ddefine sf parent
    ddefine (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"

    vdeclare (Located _ (AST.DDecl'Fun sf)) parent = vdeclare sf parent
    vdeclare (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"

    vdefine (Located _ (AST.DDecl'Fun sf)) parent = vdefine sf parent
    vdefine (Located _ (AST.DDecl'Impl _ _)) _ = error "not implemented yet"
