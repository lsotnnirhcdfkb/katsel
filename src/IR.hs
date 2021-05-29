{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
import IR.Typed

import IR.IRCtx

import IR.DeclSymbol
import IR.Module
import IR.Type

import IR.Value
import IR.Function

import qualified Data.Map as Map

import Data.List(foldl', find)
import Data.Maybe(catMaybes)

import qualified Control.Monad.State.Lazy as State(State, state, runState, get, put)

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

get_irctx :: IRBuilder -> IRCtx
get_irctx (IRBuilder c _) = c
-- IRBuildError {{{1
data IRBuildError
    = DuplicateValue String Value Value
    | DuplicateLocal Function Local LValue
    | Unimplemented String Span
    | NotAType Span DeclSymbol
    | PathDoesntExist Span -- TODO: change to 'no entity called x in y'
    | InvalidAssign Span Span

duplicate_msg :: String -> String -> String -> (Maybe Span, String) -> (Maybe Span, String) -> Message.SimpleDiag
duplicate_msg entity_kind diag_name name (old_sp, old_desc) (new_sp, new_desc) =
    let if_span m_sp ty imp msg =
            case m_sp of
                Just sp -> Right $ MsgUnds.Message sp ty imp msg
                Nothing -> Left $ Message.Note msg

        oldmsg = if_span old_sp MsgUnds.Note MsgUnds.Secondary $ entity_kind ++ " '" ++ name ++ "' already declared as " ++ old_desc
        newmsg = if_span new_sp MsgUnds.Error MsgUnds.Primary $ entity_kind ++ " '" ++ name ++ "' redeclared as " ++ new_desc
        totalmsgs = [oldmsg, newmsg]

        underlines_section =
            case [x | Right x <- totalmsgs] of
                [] -> Nothing
                msgs -> Just $ Message.Underlines $ MsgUnds.UnderlinesSection msgs
        notes = [Just x | Left x <- totalmsgs]
        sections = catMaybes $ underlines_section : notes
    in Message.SimpleDiag Message.Error new_sp Nothing (Just diag_name) sections

instance Message.ToDiagnostic (IRBuildError, IRCtx) where
    to_diagnostic (DuplicateValue name old new, irctx) =
        duplicate_msg "value" "redecl-val" name (decl_span irctx old, describe irctx old) (decl_span irctx new, describe irctx new)

    to_diagnostic (DuplicateLocal fun (Local name old_lvalue _) new_lvalue, irctx) =
        duplicate_msg "local" "redecl-local" name (decl_span irctx (fun, old_lvalue), describe irctx (fun, old_lvalue)) (decl_span irctx (fun, new_lvalue), describe irctx (fun, new_lvalue))

    to_diagnostic (Unimplemented name sp, _) =
        Message.SimpleDiag Message.Warning (Just sp) Nothing Nothing
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Message sp MsgUnds.Warning MsgUnds.Primary $ name ++ " are currently unimplemented"
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

    to_diagnostic (InvalidAssign target_sp op_sp, _) =
        Message.SimpleDiag Message.Error (Just op_sp) Nothing (Just "invalid-assign")
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Message target_sp MsgUnds.Error MsgUnds.Primary "cannot assign to non-lvalue"
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

(|>>=?) :: Monad m => Maybe a -> m b -> (a -> m b) -> m b
(|>>=?) m = (return m >>=?)
infixl 1 |>>=?

add_error :: IRBuildError -> IRBuilder -> IRBuilder
add_error err (IRBuilder irctx errs) = IRBuilder irctx (errs ++ [err])

add_error_s :: IRBuildError -> State.State IRBuilder ()
add_error_s err = State.state $ \ irb -> ((), add_error err irb)

apply_irctx_to_irbuilder :: (IRCtx -> (r, IRCtx)) -> IRBuilder -> (r, IRBuilder)
apply_irctx_to_irbuilder fun (IRBuilder irctx errs) =
    let (r, irctx') = fun irctx
    in (r, IRBuilder irctx' errs)

apply_irctx_to_irbuilder_s :: (IRCtx -> (r, IRCtx)) -> State.State IRBuilder r
apply_irctx_to_irbuilder_s fun = State.state $ apply_irctx_to_irbuilder fun

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

resolve_ty_s (Located _ (AST.DType'Pointer muty pointee)) root =
    resolve_ty_s pointee root >>=? (return Nothing) $ \ pointee_idx ->
    let pointer_ty = PointerType Map.empty (ast_muty_to_ir_muty muty) pointee_idx
    in get_ty_s pointer_ty >>=
    return . Just

{-
resolve_ty_s (Located sp AST.DType'This) _ =
    add_error_s (Unimplemented "'this' types" sp) >> -- TODO
    return Nothing
-}

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
        let make_param :: AST.LDParam -> State.State IRBuilder (Maybe (Mutability, TyIdx, Span))
            make_param (Located sp (AST.DParam'Normal mutability ty_ast _)) =
                resolve_ty_s ty_ast root >>=? (return Nothing) $ \ ty ->
                return $ Just (ast_muty_to_ir_muty mutability, ty, sp)
        in sequence <$> (sequence $ map make_param params) >>=? (return parent) $ \ param_tys ->
        apply_irctx_to_irbuilder_s (new_function retty' param_tys fun_sp name) >>= \ fun ->
        let fun_val = Value fun
        in add_noreplace_s name fun_val parent >>= \ parent' ->
        case parent' of
            Left other_value ->
                add_error_s (DuplicateValue name other_value fun_val) >>
                return parent
            Right added -> return added

    vdefine (Located _ sf@(AST.SFunDecl' _ (Located _ name) _ _)) root parent = State.runState $
        get_s name parent >>= \ m_val ->
        let m_fun = m_val >>= value_cast :: Maybe Function
        in case m_fun of
            -- silently ignore becuase the only way this can happen is if there is another global declaration
            -- that made a value of the same name that is not a function, which should already be reported as a duplicate value error
            Nothing -> return parent

            Just old_fun -> lower_fun_body sf root old_fun parent
-- lowering function bodies {{{2
data Local = Local String LValue Integer
data FunctionCG = FunctionCG Integer [Local]
-- FunctionCG functions {{{3
add_local :: String -> LValue -> FunctionCG -> Either Local FunctionCG
add_local name lvalue fcg@(FunctionCG scope_idx locals) =
    case get_local name fcg of
        Just old -> Left old
        Nothing -> Right $ FunctionCG scope_idx (Local name lvalue scope_idx : locals)

add_local_s :: String -> LValue -> State.State FunctionCG (Either Local ())
add_local_s name lvalue = State.state $ \ fcg ->
    let elfcg = add_local name lvalue fcg
    in case elfcg of
        Right fcg' -> (Right (), fcg')
        Left old -> (Left old, fcg)

get_local :: String -> FunctionCG -> Maybe Local
get_local name (FunctionCG _ locals) = find (\ (Local n _ _) -> n == name) locals
-- triplecgtup applications helpers {{{3
apply_irb_to_funcgtup_s :: State.State IRBuilder r -> State.State (IRBuilder, FunctionCG, Function) r
apply_irb_to_funcgtup_s st = State.state $ \ (irb, fcg, fun) ->
    let (r, irb') = State.runState st irb
    in (r, (irb', fcg, fun))

apply_fcg_to_funcgtup_s :: State.State FunctionCG r -> State.State (IRBuilder, FunctionCG, Function) r
apply_fcg_to_funcgtup_s st = State.state $ \ (irb, fcg, fun) ->
    let (r, fcg') = State.runState st fcg
    in (r, (irb, fcg', fun))

apply_fun_to_funcgtup_s :: State.State Function r -> State.State (IRBuilder, FunctionCG, Function) r
apply_fun_to_funcgtup_s st = State.state $ \ (irb, fcg, fun) ->
    let (r, fun') = State.runState st fun
    in (r, (irb, fcg, fun'))
-- lower function body {{{3
lower_fun_body :: Parent p Value String => AST.SFunDecl -> Module -> Function -> p -> State.State IRBuilder p
lower_fun_body (AST.SFunDecl' _ (Located _ name) params body) root fun parent =
    let function_valid = if function_not_defined fun then Just () else Nothing
    in return function_valid >>=? (return parent) $ \ _ ->
    let param_to_local (Located _ (AST.DParam'Normal _ _ (Located _ param_name)), reg_idx) function_cg =
            let m_function_cg' = add_local param_name (LVRegister reg_idx) function_cg
            in case m_function_cg' of
                Right function_cg' -> return function_cg'
                Left old_local ->
                    add_error_s (DuplicateLocal fun old_local (LVRegister reg_idx)) >>
                    return function_cg
        add_locals_for_params = map param_to_local $ zip params $ get_param_regs fun

        start_function_cg = return $ FunctionCG 0 []

    in foldl' (>>=) start_function_cg add_locals_for_params >>= \ function_cg ->

    State.get >>= \ ir_builder ->
    let (halfway_body, (ir_builder', _, fun')) = State.runState (lower_body_expr body root) (ir_builder, function_cg, fun)
        (_, fun'') = apply_halfway halfway_body (get_entry_block fun') fun'
    in State.put ir_builder' >>
    
    add_replace_s name (Value fun'') parent >>=
    return
-- lowering things {{{3
lower_body_expr :: AST.LSBlockExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) HalfwayBlock
lower_body_expr body root =
    lower_block_expr body root >>= \ (expr_hb, m_res) -> m_res |>>=? (return $ make_halfway_block "failed_body_lowering" [] $ Just HBrRet) $ \ res ->

    State.get >>= \ (_,  _, fun) ->
    let end_block = make_halfway_block "end_block"
            [Copy (LVRegister $ get_ret_reg fun) res]
            (Just HBrRet)
        expr_hb' = set_end_br expr_hb (Just $ HBrGoto end_block)

    in return $ make_halfway_group [] expr_hb' end_block

lower_expr :: AST.LDExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) HalfwayBMFV

lower_expr (Located _ (AST.DExpr'Block block)) root = lower_block_expr block root

lower_expr (Located sp (AST.DExpr'If cond trueb m_falseb)) root =
    undefined

lower_expr (Located _ (AST.DExpr'While cond body)) root =
    undefined

lower_expr (Located _ (AST.DExpr'Assign target@(Located target_sp _) (Located op_sp AST.Equal) expr)) root =
    undefined

lower_expr (Located sp (AST.DExpr'ShortCircuit _ _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "short-circuiting binary expressions" sp) >> -- TODO
    return (make_halfway_block "unsupported_shortciruit_expr" [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Binary _ _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "binary expressions" sp) >> -- TODO
    return (make_halfway_block "unsupported_binary_expr" [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Cast _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "cast expression" sp) >> -- TODO
    return (make_halfway_block "unsupported_cast_expr" [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Unary _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "unary expressions" sp) >> -- TODO
    return (make_halfway_block "unsupported_unary_expr" [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Ref _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "reference (address-of) expressions" sp) >> -- TODO
    return (make_halfway_block "unsupported_ref_expr" [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Call _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "call expressions" sp) >> -- TODO
    return (make_halfway_block "unsupported_call_expr" [] Nothing, Nothing)

{-
lower_expr (Located sp (AST.DExpr'Field _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "field access expressions" sp) >> -- TODO
    return (make_halfway_block _ [] Nothing, Nothing)

lower_expr (Located sp (AST.DExpr'Method _ _ _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "method call expressions" sp) >> -- TODO
    return (make_halfway_block _ [] Nothing, Nothing)
-}

lower_expr (Located _ (AST.DExpr'Bool b)) _ = return (make_halfway_block "literal_bool_expr" [] Nothing, Just $ FVConstBool b)
lower_expr (Located _ (AST.DExpr'Float d)) _ = return (make_halfway_block "literal_float_expr" [] Nothing, Just $ FVConstFloat d)
lower_expr (Located _ (AST.DExpr'Int i)) _ = return (make_halfway_block "literal_int_expr" [] Nothing, Just $ FVConstInt i)
lower_expr (Located _ (AST.DExpr'Char c)) _ = return (make_halfway_block "literal_char_expr" [] Nothing, Just $ FVConstChar c)

lower_expr (Located sp (AST.DExpr'String _)) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "string literal expressions" sp) >> -- TODO
    return (make_halfway_block "unsupported_literal_string_expr" [] Nothing, Nothing)

{-
lower_expr (Located sp AST.DExpr'This) _ =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "'this' expressions" sp) >> -- TODO
    return (make_halfway_block _ [] Nothing, Nothing)
-}

lower_expr (Located _ (AST.DExpr'Path path)) root =
    (case path of
        (Located _ (AST.DPath' [Located _ iden])) ->
            State.get >>= \ (_, fcg, _) ->
            let lvalue =
                    case get_local iden fcg of
                        Just (Local _ l _) -> Just l
                        Nothing -> Nothing
            in return $ FVLValue <$> lvalue

        _ -> return Nothing
    ) >>= \ reg ->
    case reg of
        Just reg_fv -> return (make_halfway_block "resolve_path_expr_as_reg" [] Nothing, Just reg_fv)
        Nothing ->
            apply_irb_to_funcgtup_s (State.state $ resolve_path_v path root) >>=? (return (make_halfway_block "resolve_path_expr_failed" [] Nothing, Nothing)) $ \ (_, vid) ->
            return (make_halfway_block "resolve_path_expr_as_global_value" [] Nothing, Just $ FVGlobalValue vid)

lower_expr (Located _ (AST.DExpr'Ret expr)) root =
    undefined

lower_block_expr :: AST.LSBlockExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) HalfwayBMFV
lower_block_expr (Located _ (AST.SBlockExpr' stmts)) root =
    undefined

lower_stmt :: AST.LDStmt -> Module -> State.State (IRBuilder, FunctionCG, Function) HalfwayBlock
lower_stmt (Located _ (AST.DStmt'Expr ex)) root = lower_expr ex root >>= return . fst

lower_stmt (Located _ (AST.DStmt'Var ty muty (Located name_sp name) m_init)) root =
    undefined
-- lowering declarations {{{1
instance Parent p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) root parent ir_builder = ddeclare sf root parent ir_builder
    {-
    ddeclare (Located sp (AST.DDecl'Impl _ _)) _ parent ir_builder =
        let warn = Unimplemented "'impl' blocks" sp -- TODO
        in (parent, add_error warn ir_builder)
    -}

    ddefine (Located _ (AST.DDecl'Fun sf)) root = ddefine sf root
    -- ddefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdeclare (Located _ (AST.DDecl'Fun sf)) root = vdeclare sf root
    -- vdeclare (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdefine (Located _ (AST.DDecl'Fun sf)) root = vdefine sf root
    -- vdefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)
