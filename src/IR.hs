{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Control.Monad.State.Lazy as State(State, state, runState)

-- build_ir {{{1
build_ir :: AST.LDModule -> (Module, IRCtx, [IRBuildError])
build_ir mod_ast@(Located mod_sp _) = (lowered_mod, lowered_irctx, errors)
    where
        apply_stage :: (AST.LDModule -> Module -> ModParent -> IRBuilder -> (ModParent, IRBuilder)) -> (ModParent, IRBuilder) -> (ModParent, IRBuilder)
        apply_stage fun (mod_parent@(ModParent module_), ir_builder) =
            let (module_', ir_builder') = fun mod_ast module_ mod_parent ir_builder
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
lower_all_in_list :: Lowerable l p => [l] -> (l -> Module -> p -> IRBuilder -> (p, IRBuilder)) -> Module -> p -> IRBuilder -> (p, IRBuilder)
lower_all_in_list things fun root start irb = (foldl' (flip (.)) id funs) (start, irb)
    where
        apply_fun thing (p, irb') = fun thing root p irb'
        funs = map apply_fun things

lower_all_in_list_s :: Lowerable l p => [l] -> (l -> Module -> p -> IRBuilder -> (p, IRBuilder)) -> Module -> p -> State.State IRBuilder p
lower_all_in_list_s things fun root start = State.state $ lower_all_in_list things fun root start

ast_muty_to_ir_muty :: AST.Mutability -> Mutability
ast_muty_to_ir_muty AST.Immutable = Immutable
ast_muty_to_ir_muty AST.Mutable = Mutable

(>>=?) :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
(>>=?) m r c =
    m >>= \ m_res ->
    case m_res of
        Just res -> c res
        Nothing -> r
infixl 1 >>=?

add_error :: IRBuildError -> IRBuilder -> IRBuilder
add_error err (IRBuilder irctx errs) = IRBuilder irctx (errs ++ [err])

add_error_s :: IRBuildError -> State.State IRBuilder ()
add_error_s err = State.state $ \ irb -> ((), add_error err irb)

get_s :: Parent p c i => i -> p -> State.State IRBuilder (Maybe c)
get_s ind parent = State.state $ \ irb ->
    let (IRBuilder irctx _) = irb
    in (get ind (parent, irctx), irb)

add_replace_s :: Parent p c i => i -> c -> p -> State.State IRBuilder p
add_replace_s ind child parent = State.state $ \ irb ->
    let (IRBuilder irctx errors) = irb
        (parent', irctx') = add_replace ind child (parent, irctx)
        irb' = IRBuilder irctx' errors
    in (parent', irb')

add_noreplace_s :: Parent p c i => i -> c -> p -> State.State IRBuilder (Either c p)
add_noreplace_s ind child parent = State.state $ \ irb ->
    let (IRBuilder irctx errors) = irb
    in case add_noreplace ind child (parent, irctx) of
        Left old -> (Left old, irb)
        Right (parent', irctx') -> (Right parent', IRBuilder irctx' errors)
-- path resultion, type resolution, type interning {{{1
resolve_path_d :: AST.LDPath -> Module -> IRBuilder -> (Maybe (DeclSymbol, DSIRId DeclSymbol), IRBuilder)
resolve_path_d (Located path_sp (AST.DPath' located_segments)) root ir_builder@(IRBuilder irctx _) =
    case m_dsid of
        Just dsid -> (Just (resolve_dsid irctx root dsid, dsid), ir_builder)
        Nothing -> (Nothing, add_error (PathDoesntExist path_sp) ir_builder)
    where
        unlocate (Located _ x) = x
        segments = map unlocate located_segments
        m_dsid = new_dsid irctx root segments :: Maybe (DSIRId DeclSymbol)

resolve_path_v :: AST.LDPath -> Module -> IRBuilder -> (Maybe (Value, VIRId Value), IRBuilder)
resolve_path_v (Located path_sp (AST.DPath' located_segments)) root ir_builder@(IRBuilder irctx _) =
    case m_vid of
        Just vid -> (Just (resolve_vid irctx root vid, vid), ir_builder)
        Nothing -> (Nothing, add_error (PathDoesntExist path_sp) ir_builder)
    where
        unlocate (Located _ x) = x
        segments = map unlocate located_segments
        m_vid = new_vid irctx root segments :: Maybe (VIRId Value)

resolve_ty_s :: AST.LDType -> Module -> State.State IRBuilder (Maybe TyIdx)

resolve_ty_s (Located path_sp (AST.DType'Path path)) root =
    (State.state $ resolve_path_d path root) >>=? (return Nothing) $ \ (ds, _) ->
    case ds_cast ds :: Maybe TyIdx of
        j@(Just _) -> return j
        Nothing ->
            add_error_s (NotAType path_sp ds) >>
            return Nothing

resolve_ty_s (Located sp (AST.DType'Pointer _ _)) _ =
    add_error_s (Unsupported "pointer types" sp) >> -- TODO
    return Nothing
resolve_ty_s (Located sp AST.DType'This) _ =
    add_error_s (Unsupported "'this' types" sp) >> -- TODO
    return Nothing

get_ty_s :: Type -> State.State IRBuilder TyIdx
get_ty_s ty = State.state $ \ (IRBuilder ctx errs) ->
    let (idx, ctx') = get_ty_irctx ty ctx
    in (idx, IRBuilder ctx' errs)
-- Lowerable class {{{1
class Lowerable l p where
    ddeclare, ddefine, vdeclare, vdefine :: l -> Module -> p -> IRBuilder -> (p, IRBuilder)
-- lowering modules {{{1
newtype ModParent = ModParent Module
instance Parent ModParent Module () where
    get_child_map (ModParent mp, _) = Map.singleton () mp
    add _ child (ModParent prev, irctx) = (Just prev, (ModParent child, irctx))

lower_module :: Parent a Module () => (AST.LDDecl -> Module -> Module -> IRBuilder -> (Module, IRBuilder)) -> Located AST.DModule -> Module -> a -> IRBuilder -> (a, IRBuilder)
lower_module fun (Located _ (AST.DModule' decls)) root parent = State.runState $
    get_s () parent >>= \ (Just (module_ :: Module)) ->
    lower_all_in_list_s decls fun root module_ >>= \ added_module ->
    add_replace_s () added_module parent >>= \ added_parent ->
    return added_parent

instance Parent p Module () => Lowerable AST.LDModule p where
    ddeclare = lower_module ddeclare
    ddefine = lower_module ddefine
    vdeclare = lower_module vdeclare
    vdefine = lower_module vdefine
-- lowering functions {{{1
instance Parent p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ = (,)
    ddefine _ _ = (,)

    vdeclare (Located fun_sp (AST.SFunDecl' mretty (Located _ name) params _)) root parent = State.runState $
        (case mretty of
            Just retty -> resolve_ty_s retty root
            Nothing -> Just <$> get_ty_s (VoidType Map.empty)
        ) >>=? (return parent) $ \ retty' ->
        let make_param :: AST.LDParam -> State.State IRBuilder (Maybe (Mutability, TyIdx))
            make_param (Located _ (AST.DParam'Normal mutability ty_ast _)) =
                resolve_ty_s ty_ast root >>=? (return Nothing) $ \ ty ->
                return $ Just (ast_muty_to_ir_muty mutability, ty)
        in sequence <$> (sequence $ map make_param params) >>=? (return parent) $ \ param_tys ->
        let newty = FunctionType Map.empty retty' param_tys
        in get_ty_s newty >>= \ fun_ty_idx ->
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
        in add_noreplace_s name fun_val parent >>= \ parent' ->
        case parent' of
            Left other_value ->
                add_error_s (DuplicateValue name other_value fun_val) >>
                return parent
            Right added -> return added

    vdefine (Located _ (AST.SFunDecl' _ (Located _ name) _ _)) _ parent = State.runState $
        get_s name parent >>= \ m_val ->
        let m_fun = m_val >>= value_cast :: Maybe Function
        in case m_fun of
            -- silently ignore becuase the only way this can happen is if there is another global declaration
            -- that made a value of the same name that is not a function, which should already be reported as a duplicate value error
            Nothing -> return parent

            Just old_fun -> lower_fun_body old_fun parent
-- lowering function bodies {{{2
lower_fun_body :: Parent p Value String => Function -> p -> State.State IRBuilder p
lower_fun_body = error "not implemented yet"
-- lowering declarations {{{1
instance Parent p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) root parent ir_builder = ddeclare sf root parent ir_builder
    ddeclare (Located sp (AST.DDecl'Impl _ _)) _ parent ir_builder =
        let warn = Unsupported "'impl' blocks" sp -- TODO
        in (parent, add_error warn ir_builder)

    ddefine (Located _ (AST.DDecl'Fun sf)) root = ddefine sf root
    ddefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdeclare (Located _ (AST.DDecl'Fun sf)) root = vdeclare sf root
    vdeclare (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdefine (Located _ (AST.DDecl'Fun sf)) root = vdefine sf root
    vdefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)
