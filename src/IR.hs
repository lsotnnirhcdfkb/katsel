{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module IR
    ( build_ir
    , Module
    , IRCtx
    ) where

import qualified AST

import qualified Message
import qualified Message.Underlines as MsgUnds

import Location

import IR.ID
import IR.Parent
import IR.Describe
import IR.DeclSpan

import IR.IRCtx

import IR.DeclSymbol
import IR.Module
import IR.Type

import IR.Value
import IR.Function

import qualified Data.Map as Map

import Data.List(foldl')
import Data.Maybe(catMaybes)

import qualified Control.Monad.State.Lazy as State(State, state, runState, execState, get, put)

-- build_ir {{{1
build_ir :: AST.LDModule -> (Module, IRCtx, [IRBuildError])
build_ir mod_ast@(Located mod_sp _) = (lowered_mod, lowered_irctx, errors)
    where
        apply_stage :: (AST.LDModule -> Module -> IRDiff ModParent) -> (ModParent, IRBuilder) -> (ModParent, IRBuilder)
        apply_stage fun (mod_parent@(ModParent module_), ir_builder) =
            let (module_', ir_builder') = fun mod_ast module_ (mod_parent, ir_builder)
            in (module_', ir_builder')

        initial_cgtup = (ModParent module_, IRBuilder irctx [])
            where
                (module_, irctx) = new_module mod_sp new_irctx
        (ModParent lowered_mod, IRBuilder lowered_irctx errors) =
            apply_stage vdefine .
            apply_stage vdeclare .
            apply_stage ddefine .
            apply_stage ddeclare $
            initial_cgtup
-- IRBuilder {{{1
data IRBuilder = IRBuilder IRCtx [IRBuildError]
-- IRBuildError {{{1
data IRBuildError
    = DuplicateValue String Value Value
    | Unsupported String Span
    | NotAType Span DeclSymbol
    | PathDoesntExist Span -- TODO: change to 'no entity called x in y'

instance Message.ToDiagnostic (IRBuildError, IRCtx) where
    to_diagnostic (DuplicateValue name old new, irctx) =
        let m_oldsp = decl_span irctx old
            m_newsp = decl_span irctx new
            old_desc = describe irctx old
            new_desc = describe irctx new

            if_span m_sp ty imp msg =
                case m_sp of
                    Just sp -> Right $ MsgUnds.Message sp ty imp msg
                    Nothing -> Left $ Message.Note msg
            oldmsg = if_span m_oldsp MsgUnds.Note MsgUnds.Secondary $ "'" ++ name ++ "' already declared as " ++ old_desc
            newmsg = if_span m_newsp MsgUnds.Error MsgUnds.Primary $ "'" ++ name ++ "' redeclared as " ++ new_desc

            totalmsgs = [oldmsg, newmsg]

            underlines_section =
                case [x | Right x <- totalmsgs] of
                    [] -> Nothing
                    msgs -> Just $ Message.Underlines $ MsgUnds.UnderlinesSection msgs
            notes = [Just x | Left x <- totalmsgs]
            sections = catMaybes $ underlines_section : notes
        in Message.SimpleDiag Message.Error m_oldsp Nothing (Just "redecl-val") sections

    to_diagnostic (Unsupported name sp, _) =
        Message.SimpleDiag Message.Warning (Just sp) Nothing Nothing
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Message sp MsgUnds.Warning MsgUnds.Primary $ name ++ " are currently unsupported"
                ]
            ]

    to_diagnostic (NotAType path_sp ds, irctx) =
        Message.SimpleDiag Message.Error (Just path_sp) Nothing (Just "not-type")
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Message path_sp MsgUnds.Error MsgUnds.Primary "not a type"
                , MsgUnds.Message path_sp MsgUnds.Note MsgUnds.Secondary $ "this path resolved to " ++ describe irctx ds
                ]
            ]

    to_diagnostic (PathDoesntExist path_sp, _) =
        Message.SimpleDiag Message.Error (Just path_sp) Nothing (Just "path-doesnt-exist")
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Message path_sp MsgUnds.Error MsgUnds.Primary "entity referred to by path doesn't exist"
                ]
            ]
-- helper functions {{{1
lower_all_in_list :: Lowerable l p => [l] -> (l -> Module -> IRDiff p) -> Module -> IRDiff p
lower_all_in_list things fun root = foldl' (flip (.)) id funs
    where
        apply_fun thing = fun thing root
        funs = map apply_fun things

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

unwrap_maybe :: Maybe a -> a
unwrap_maybe (Just x) = x
unwrap_maybe Nothing = error "unwrap maybe that is Nothing"

(>>=?) :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
(>>=?) m r c =
    m >>= \ m_res ->
    case m_res of
        Just res -> c res
        Nothing -> r
infixl 1 >>=?

add_error :: IRBuildError -> IRBuilder -> IRBuilder
add_error err (IRBuilder irctx errs) = IRBuilder irctx (errs ++ [err])
-- path resultion, type resolution, type interning {{{1
resolve_path_d :: AST.LDPath -> Module -> (p, IRBuilder) -> (Maybe (DeclSymbol, DSIRId DeclSymbol), (p, IRBuilder))
resolve_path_d (Located path_sp (AST.DPath' located_segments)) root cgtup@(parent, ir_builder@(IRBuilder irctx _)) =
    case m_dsid of
        Just dsid -> (Just (resolve_dsid irctx root dsid, dsid), cgtup)
        Nothing -> (Nothing, (parent, add_error (PathDoesntExist path_sp) ir_builder))
    where
        unlocate (Located _ x) = x
        segments = map unlocate located_segments
        m_dsid = new_dsid irctx root segments :: Maybe (DSIRId DeclSymbol)

resolve_path_v :: AST.LDPath -> Module -> (p, IRBuilder) -> (Maybe (Value, VIRId Value), (p, IRBuilder))
resolve_path_v (Located path_sp (AST.DPath' located_segments)) root cgtup@(parent, ir_builder@(IRBuilder irctx _)) =
    case m_vid of
        Just vid -> (Just (resolve_vid irctx root vid, vid), cgtup)
        Nothing -> (Nothing, (parent, add_error (PathDoesntExist path_sp) ir_builder))
    where
        unlocate (Located _ x) = x
        segments = map unlocate located_segments
        m_vid = new_vid irctx root segments :: Maybe (VIRId Value)

resolve_ty :: AST.LDType -> Module -> (p, IRBuilder) -> (Maybe TyIdx, (p, IRBuilder))

resolve_ty (Located path_sp (AST.DType'Path path)) root cgtup = flip State.runState cgtup $
    (State.state $ resolve_path_d path root) >>=? (return Nothing) $ \ (ds, _) ->
    case ds_cast ds :: Maybe TyIdx of
        j@(Just _) -> return j
        Nothing ->
            State.state (irbuilder_fun_to_cgtup_fun $ add_unit_res $ add_error $ NotAType path_sp ds) >>
            return Nothing

resolve_ty (Located sp (AST.DType'Pointer _ _)) _ (p, ir_builder) = (Nothing, (p, add_error (Unsupported "pointer types" sp) ir_builder)) ---TODO
resolve_ty (Located sp AST.DType'This) _ (p, ir_builder) = (Nothing, (p, add_error (Unsupported "'this' types" sp) ir_builder)) -- TODO

get_ty_irbuilder :: Type -> IRBuilder -> (TyIdx, IRBuilder)
get_ty_irbuilder ty (IRBuilder ctx errs) =
    let (idx, ctx') = get_ty_irctx ty ctx
    in (idx, IRBuilder ctx' errs)
-- Lowerable class {{{1
type IRDiff p = (p, IRBuilder) -> (p, IRBuilder)
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> Module -> IRDiff p
-- lowering modules {{{1
newtype ModParent = ModParent Module
instance ParentR ModParent Module () where
    get_child_map _ (ModParent mp) = Map.singleton () mp
instance ParentW ModParent Module () where
    add _ _ child (ModParent prev) = (Just prev, ModParent child)

instance ParentRW p Module () => Lowerable AST.LDModule p where
    ddeclare (Located _ (AST.DModule' decls)) root (parent, ir_builder@(IRBuilder irctx _)) =
        let (Just module_) = get irctx () parent :: Maybe Module
            (module_', ir_builder') = lower_all_in_list decls ddeclare root (module_, ir_builder)
        in (add_replace irctx () module_' parent, ir_builder')

    ddefine (Located _ (AST.DModule' decls)) root (parent, ir_builder@(IRBuilder irctx _)) =
        let (Just module_) = get irctx () parent :: Maybe Module
            (module_', ir_builder') = lower_all_in_list decls ddefine root (module_, ir_builder)
        in (add_replace irctx () module_' parent, ir_builder')

    vdeclare (Located _ (AST.DModule' decls)) root (parent, ir_builder@(IRBuilder irctx _)) =
        let (Just module_) = get irctx () parent :: Maybe Module
            (module_', ir_builder') = lower_all_in_list decls vdeclare root (module_, ir_builder)
        in (add_replace irctx () module_' parent, ir_builder')

    vdefine (Located _ (AST.DModule' decls)) root (parent, ir_builder@(IRBuilder irctx _)) =
        let (Just module_) = get irctx () parent :: Maybe Module
            (module_', ir_builder') = lower_all_in_list decls vdefine root (module_, ir_builder)
        in (add_replace irctx () module_' parent, ir_builder')
-- lowering functions {{{1
instance ParentRW p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ = id
    ddefine _ _ = id

    -- TODO: refactor this a bit because it is very messy
    vdeclare (Located fun_sp (AST.SFunDecl' mretty (Located _ name) params _)) root = State.execState $
        (State.state $ case mretty of
            Just retty -> resolve_ty retty root
            Nothing -> \ (p, irb) ->
                let (idx, irb') = get_ty_irbuilder (VoidType Map.empty) irb
                in (Just idx, (p, irb'))
        ) >>=? (return ()) $ \ retty' ->
        let make_param :: AST.LDParam -> State.State (p, IRBuilder) (Maybe (Mutability, TyIdx))
            make_param (Located _ (AST.DParam'Normal mutability ty_ast _)) =
                State.state (resolve_ty ty_ast root) >>= \ m_ty ->
                case m_ty of
                    Just ty -> return $ Just (ast_muty_to_ir_muty mutability, ty)
                    Nothing -> return Nothing
        in sequence <$> (sequence $ map make_param params) >>=? (return ()) $ \ param_tys ->
        let newty = FunctionType Map.empty retty' param_tys
        in State.state (irbuilder_fun_to_cgtup_fun $ get_ty_irbuilder newty) >>= \ fun_ty_idx ->
        let fun = Function
                  { get_function_blocks = []
                  , get_function_registers = map (uncurry $ flip Register) param_tys
                  , get_function_ret_reg = 0
                  , get_function_param_regs = take (length params) [1..]
                  , get_function_type = fun_ty_idx
                  , get_function_span = fun_sp
                  , get_function_name = name
                  }
            fun_val = Value fun
        in State.get >>= \ before@(before_parent, before_ir_builder@(IRBuilder irctx _)) ->
            case add_noreplace irctx name fun_val before_parent of
                Left other_value ->
                    State.put before >>
                    State.state (irbuilder_fun_to_cgtup_fun $ add_unit_res $ add_error $ DuplicateValue name other_value fun_val)
                Right added -> State.put (added, before_ir_builder)

    vdefine (Located _ (AST.SFunDecl' _ (Located _ name) _ _)) _ cgtup@(parent, IRBuilder irctx _) =
        let m_val = get irctx name parent :: Maybe Value
            m_fun = m_val >>= value_cast :: Maybe Function
        in case m_fun of
            -- silently ignore becuase the only way this can happen is if there is another global declaration
            -- that made a value of the same name that is not a function, which should already be reported as a duplicate value error
            Nothing -> cgtup

            Just old_fun -> lower_fun_body old_fun cgtup
-- lowering function bodies {{{2
lower_fun_body :: ParentW p Value String => Function -> (p, IRBuilder) -> (p, IRBuilder)
lower_fun_body = error "not implemented yet"
-- lowering declarations {{{1
instance ParentRW p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) root cgtup = ddeclare sf root cgtup
    ddeclare (Located sp (AST.DDecl'Impl _ _)) _ (parent, ir_builder) =
        let warn = Unsupported "'impl' blocks" sp -- TODO
        in (parent, add_error warn ir_builder)

    ddefine (Located _ (AST.DDecl'Fun sf)) root cgtup = ddefine sf root cgtup
    ddefine (Located _ (AST.DDecl'Impl _ _)) _ cgtup = cgtup

    vdeclare (Located _ (AST.DDecl'Fun sf)) root cgtup = vdeclare sf root cgtup
    vdeclare (Located _ (AST.DDecl'Impl _ _)) _ cgtup = cgtup

    vdefine (Located _ (AST.DDecl'Fun sf)) root cgtup = vdefine sf root cgtup
    vdefine (Located _ (AST.DDecl'Impl _ _)) _ cgtup = cgtup
