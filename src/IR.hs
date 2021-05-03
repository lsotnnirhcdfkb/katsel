{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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

{-
TODO: more typeclasses!
    split DSChildren and VChildren into DSChildGet, DSChildAdd, VChildGet, and VChildAdd
    and also split Parent into ParentWriteOnly and ParentReadOnly
    in order to make sure that lowering functions only read from the first arguments and only modify in the IRDiff
-}

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
        next (Just ds) name = get_decl_symbol name ds
        next Nothing _ = Nothing

vresolve :: Typeable r => Module -> VIRId r -> Maybe r
vresolve parentmod (VIRId parent childname) =
    child >>= cast
    where
        parent_resolved = dsresolve parentmod parent
        child = parent_resolved >>= get_value childname
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
    get_dsmap :: d -> DSMap
    get_decl_symbol :: String -> d -> Maybe DeclSymbol
    add_decl_symbol :: String -> DeclSymbol -> d -> d

    get_decl_symbol n d = Map.lookup n $ get_dsmap d

class VChildren v where
    get_vmap :: v -> VMap
    get_value :: String -> v -> Maybe Value
    add_value :: String -> Value -> v -> v

    get_value n v = Map.lookup n $ get_vmap v
-- instances {{{3
-- DeclSymbol {{{3
instance DSChildren DeclSymbol where
    get_dsmap (DeclSymbol d) = get_dsmap d
    add_decl_symbol name child (DeclSymbol ds) = DeclSymbol $ add_decl_symbol name child ds
instance VChildren DeclSymbol where
    get_vmap (DeclSymbol d) = get_vmap d
    add_value name child (DeclSymbol ds) = DeclSymbol $ add_value name child ds
-- Type {{{4
instance DSChildren Type where
    get_dsmap (FloatType dsmap _) = dsmap
    get_dsmap (IntType dsmap _ _) = dsmap
    get_dsmap (CharType dsmap) = dsmap
    get_dsmap (BoolType dsmap) = dsmap
    get_dsmap (FunctionType dsmap _ _) = dsmap
    get_dsmap (VoidType dsmap) = dsmap
    get_dsmap (PointerType dsmap _ _) = dsmap
-- Module {{{4
instance DSChildren Module where
    get_dsmap (Module dsmap _) = dsmap
instance VChildren Module where
    get_vmap (Module _ vmap) = vmap
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
        apply_stage :: (AST.LDModule -> ModParent -> Module -> IRDiff ModParent) -> (ModParent, IRBuilder) -> (ModParent, IRBuilder)
        apply_stage fun parent_builder_tup@(mod_parent@(ModParent module_), _) =
            let diff = fun mod_ast mod_parent module_
            in apply_diff diff parent_builder_tup

        initial_parent_builder_tup = (ModParent $ Module Map.empty Map.empty, IRBuilder (TyCtx []) [])
        (ModParent lowered_mod, IRBuilder tyctx errors) =
            apply_stage vdefine .
            apply_stage vdeclare .
            apply_stage ddefine .
            apply_stage ddeclare $
            initial_parent_builder_tup

-- helper functions {{{2
lower_all_in_list :: Lowerable l p => [l] -> (l -> p -> Module -> IRDiff p) -> p -> Module -> IRDiff p
lower_all_in_list things fun parent root = IRDiff $ foldl' (.) id diffs
    where
        apply_fun thing = fun thing parent root
        diffs = map (apply_diff . apply_fun) things

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
-- type resolution & type interning {{{2
resolve_ty :: AST.LDType -> IRBuilder -> (TyIdx, IRBuilder)
resolve_ty = error "not implemented yet"

add_ty :: Type -> IRBuilder -> (TyIdx, IRBuilder)
add_ty ty (IRBuilder (TyCtx tys) errs) = (TyIdx $ length tys, IRBuilder (TyCtx $ tys ++ [ty]) errs)

get_void_type :: IRBuilder -> (TyIdx, IRBuilder)
get_void_type = error "not implemented yet"
-- Lowerable class {{{2
newtype IRDiff p = IRDiff { apply_diff :: (p, IRBuilder) -> (p, IRBuilder) }
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> p -> Module -> IRDiff p
-- Parent class {{{2
class Parent p c i | p c -> i where
    add :: i -> c -> p -> p
    get :: i -> p -> Maybe c
-- lowering modules {{{2
newtype ModParent = ModParent Module
-- parent instances {{{
instance Parent ModParent Module () where
    add _ m _ = ModParent m
    get _ (ModParent m) = Just m
instance Parent DeclSymbol DeclSymbol String where
    add = add_decl_symbol
    get = get_decl_symbol
instance Parent DeclSymbol Value String where
    add = add_value
    get = get_value
instance Parent Module DeclSymbol String where
    add = add_decl_symbol
    get = get_decl_symbol
instance Parent Module Value String where
    add = add_value
    get = get_value
-- }}}
instance Parent p Module () => Lowerable AST.LDModule p where
    ddeclare (Located _ (AST.DModule' decls)) parent root =
        let (Just module_) = get () parent :: Maybe Module
            module_diff = lower_all_in_list decls ddeclare module_ root
        in IRDiff $ \ (wo_parent, ir_builder) ->
                let (module_', ir_builder') = apply_diff module_diff (module_, ir_builder)
                in (add () module_' wo_parent, ir_builder')

    ddefine (Located _ (AST.DModule' decls)) parent root =
        let (Just module_) = get () parent :: Maybe Module
            module_diff = lower_all_in_list decls ddefine module_ root
        in IRDiff $ \ (wo_parent, ir_builder) ->
                let (module_', ir_builder') = apply_diff module_diff (module_, ir_builder)
                in (add () module_' wo_parent, ir_builder')

    vdeclare (Located _ (AST.DModule' decls)) parent root =
        let (Just module_) = get () parent :: Maybe Module
            module_diff = lower_all_in_list decls vdeclare module_ root
        in IRDiff $ \ (wo_parent, ir_builder) ->
                let (module_', ir_builder') = apply_diff module_diff (module_, ir_builder)
                in (add () module_' wo_parent, ir_builder')

    vdefine (Located _ (AST.DModule' decls)) parent root =
        let (Just module_) = get () parent :: Maybe Module
            module_diff = lower_all_in_list decls vdefine module_ root
        in IRDiff $ \ (wo_parent, ir_builder) ->
                let (module_', ir_builder') = apply_diff module_diff (module_, ir_builder)
                in (add () module_' wo_parent, ir_builder')
-- lowering functions {{{2
instance Parent p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ _ = IRDiff id
    ddefine _ _ _ = IRDiff id

    vdeclare (Located _ (AST.SFunDecl' mretty (Located _ name) params _)) roparent root = IRDiff $ execState $
        (state . irbuilder_fun_to_cgtup_fun $ case mretty of
            Just retty -> resolve_ty retty
            Nothing -> get_void_type
        ) >>= \ retty' ->
        let make_param (Located _ (AST.DParam'Normal mutability ty_ast _)) =
                state (irbuilder_fun_to_cgtup_fun $ resolve_ty ty_ast) >>= \ ty ->
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
        in state $ parent_fun_to_cgtup_fun . add_unit_res $ add name (Value fun)

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
