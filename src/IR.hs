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

import qualified Control.Monad.State.Lazy as State(State, state, execState, runState, get, put)

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
    | DuplicateLocal Function Local RegisterIdx String
    | Unimplemented String Span
    | NotAType Span DeclSymbol
    | PathDoesntExist Span -- TODO: change to 'no entity called x in y'

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

    to_diagnostic (DuplicateLocal fun (Local _ old_reg_idx _) new_reg_idx name, irctx) =
        let old = get_register fun old_reg_idx
            new = get_register fun new_reg_idx
        in duplicate_msg "local" "redecl-local" name (decl_span irctx old, describe irctx old) (decl_span irctx new, describe irctx new)

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

resolve_ty_s (Located sp AST.DType'This) _ =
    add_error_s (Unimplemented "'this' types" sp) >> -- TODO
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
data Local = Local String RegisterIdx Integer
data FunctionCG = FunctionCG Integer [Local]

-- FunctionCG functions {{{3
add_local :: String -> RegisterIdx -> FunctionCG -> Either Local FunctionCG
add_local name reg_idx fcg@(FunctionCG scope_idx locals) =
    case get_local name fcg of
        Just old -> Left old
        Nothing -> Right $ FunctionCG scope_idx (Local name reg_idx scope_idx : locals)

add_local_s :: String -> RegisterIdx -> State.State FunctionCG (Either Local ())
add_local_s name reg_idx = State.state $ \ fcg ->
    let elfcg = add_local name reg_idx fcg
    in case elfcg of
        Right fcg' -> (Right (), fcg')
        Left old -> (Left old, fcg)

get_local :: String -> FunctionCG -> Maybe Local
get_local name (FunctionCG _ locals) = find (\ (Local n _ _) -> n == name) locals
-- helpers {{{3
add_instruction_s :: Instruction -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) InstructionIdx
add_instruction_s instr block_idx = apply_fun_to_funcgtup_s (State.state $ add_instruction instr block_idx)

add_br_s :: Br -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) ()
add_br_s br block_idx = apply_fun_to_funcgtup_s (State.state $ (,) () . add_br br block_idx)

add_basic_block_s :: String -> State.State (IRBuilder, FunctionCG, Function) BlockIdx
add_basic_block_s name = apply_fun_to_funcgtup_s (State.state $ add_basic_block name)
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
            let m_function_cg' = add_local param_name reg_idx function_cg
            in case m_function_cg' of
                Right function_cg' -> return function_cg'
                Left old_local ->
                    add_error_s (DuplicateLocal fun old_local reg_idx param_name) >>
                    return function_cg
        add_locals_for_params = map param_to_local $ zip params $ get_param_regs fun

        start_function_cg = return $ FunctionCG 0 []

    in foldl' (>>=) start_function_cg add_locals_for_params >>= \ function_cg ->

    State.get >>= \ ir_builder ->
    let (ir_builder', _, fun') = State.execState (lower_body_expr body root) (ir_builder, function_cg, fun)
    in State.put ir_builder' >>
    add_replace_s name (Value fun') parent >>=
    return
-- lowering things {{{3
lower_body_expr :: AST.LSBlockExpr -> Module -> State.State (IRBuilder, FunctionCG, Function) ()
lower_body_expr body root =
    State.get >>= \ (_, _, fun) ->

    lower_block_expr body root (get_entry_block fun) >>= \ (end_block, m_res) -> m_res |>>=? (return ()) $ \ res ->

    add_instruction_s (Copy (get_ret_reg fun) res) end_block >>
    add_br_s (BrGoto $ get_exit_block fun) end_block >>
    add_br_s BrRet (get_exit_block fun) >>

    return ()

lower_expr :: AST.LDExpr -> Module -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) (BlockIdx, Maybe FValue)

lower_expr (Located _ (AST.DExpr'Block block)) root cur_block = lower_block_expr block root cur_block

lower_expr (Located sp (AST.DExpr'If _ cond trueb m_falseb)) root start_block =
    lower_expr cond root start_block >>= \ (after_cond, m_cond_val) -> m_cond_val |>>=? (return (after_cond, Nothing)) $ \ cond_val ->

    add_basic_block_s "if_after_branch" >>= \ if_after_block ->

    add_basic_block_s "if_true_branch" >>= \ if_true_start_block ->
    lower_expr trueb root if_true_start_block >>= \ (if_true_end_block, m_truev) -> m_truev |>>=? (return (if_after_block, Nothing)) $ \ truev ->

    State.get >>= \ (irb, _, fun) ->
    let irctx = get_irctx irb
    in apply_fun_to_funcgtup_s (State.state $ add_register (type_of irctx (root, fun, truev)) Immutable sp) >>= \ ret_reg ->

    add_instruction_s (Copy ret_reg truev) if_true_end_block >>
    add_br_s (BrGoto if_after_block) if_true_end_block >>

    (case m_falseb of
        Nothing ->
            add_br_s (BrCond cond_val if_true_start_block if_after_block) after_cond >>
            return (Just ())

        Just (_, falseb) ->
            add_basic_block_s "if_false_branch" >>= \ if_false_start_block ->
            lower_expr falseb root if_false_start_block >>= \ (if_false_end_block, m_falsev) -> m_falsev |>>=? (return Nothing) $ \ falsev ->

            add_instruction_s (Copy ret_reg falsev) if_false_end_block >>
            add_br_s (BrGoto if_after_block) if_false_end_block >>

            add_br_s (BrCond cond_val if_true_start_block if_false_start_block) after_cond >>
            return (Just ())
    ) >>=? (return (if_after_block, Nothing)) $ \ _ ->

    return (if_after_block, Just $ FVRegister ret_reg)

lower_expr (Located sp (AST.DExpr'While _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "'while' expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Assign _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "assignment expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'ShortCircuit _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "short-circuiting binary expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Binary _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "binary expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Cast _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "cast expression" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Unary _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "unary expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Ref _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "reference (address-of) expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Call _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "call expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Field _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "field access expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp (AST.DExpr'Method _ _ _ _ _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "method call expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located _ (AST.DExpr'Bool b)) _ cur_block = return (cur_block, Just $ FVConstBool b)
lower_expr (Located _ (AST.DExpr'Float d)) _ cur_block = return (cur_block, Just $ FVConstFloat d)
lower_expr (Located _ (AST.DExpr'Int i)) _ cur_block = return (cur_block, Just $ FVConstInt i)
lower_expr (Located _ (AST.DExpr'Char c)) _ cur_block = return (cur_block, Just $ FVConstChar c)

lower_expr (Located sp (AST.DExpr'String _)) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "string literal expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located sp AST.DExpr'This) _ cur_block =
    apply_irb_to_funcgtup_s (add_error_s $ Unimplemented "'this' expressions" sp) >> -- TODO
    return (cur_block, Nothing)

lower_expr (Located _ (AST.DExpr'Path path)) root cur_block =
    (case path of
        (Located _ (AST.DPath' [Located _ iden])) ->
            State.get >>= \ (_, fcg, _) ->
            let reg_idx =
                    case get_local iden fcg of
                        Just (Local _ r _) -> Just r
                        Nothing -> Nothing
            in return $ FVRegister <$> reg_idx

        _ -> return Nothing
    ) >>= \ reg ->
    case reg of
        Just reg_fv -> return (cur_block, Just reg_fv)
        Nothing ->
            apply_irb_to_funcgtup_s (State.state $ resolve_path_v path root) >>=? (return (cur_block, Nothing)) $ \ (_, vid) ->
            return (cur_block, Just $ FVGlobalValue vid)

lower_block_expr :: AST.LSBlockExpr -> Module -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) (BlockIdx, Maybe FValue)
lower_block_expr (Located _ (AST.SBlockExpr' stmts)) root cur_block =
    let safe_last [] = Nothing
        safe_last x = Just $ last x

        safe_init [] = []
        safe_init l = init l

        (stmts', m_ret_expr) = case safe_last stmts of
            Just (Located _ (AST.DStmt'Expr ret)) -> (safe_init stmts, Just ret)
            _ -> (stmts, Nothing)

    in foldl' (>>=) (return cur_block) (map (flip lower_stmt root) stmts') >>= \ end_block ->

    (case m_ret_expr of
        Just ret_expr -> lower_expr ret_expr root end_block
        Nothing -> return (end_block, Just FVVoid)
    ) >>=
    return

lower_stmt :: AST.LDStmt -> Module -> BlockIdx -> State.State (IRBuilder, FunctionCG, Function) BlockIdx
lower_stmt (Located _ (AST.DStmt'Expr ex)) root cur_block = lower_expr ex root cur_block >>= return . fst

lower_stmt (Located _ (AST.DStmt'Var ty muty (Located name_sp name) m_init)) root cur_block =
    apply_irb_to_funcgtup_s (resolve_ty_s ty root) >>=? (return cur_block) $ \ var_ty_idx ->
    apply_fun_to_funcgtup_s (State.state $ add_register var_ty_idx (ast_muty_to_ir_muty muty) name_sp) >>= \ reg_idx ->
    apply_fcg_to_funcgtup_s (add_local_s name reg_idx) >>= \ m_old ->
    (case m_old of
        Left old ->
            State.get >>= \ (_, _, fun) ->
            apply_irb_to_funcgtup_s $ add_error_s $ DuplicateLocal fun old reg_idx name
        Right () -> return ()
    ) >>
    (case m_init of
        Nothing -> return cur_block
        Just expr ->
            lower_expr expr root cur_block >>= \ (after_expr, m_expr_val) -> m_expr_val |>>=? (return after_expr) $ \ expr_val ->
            add_instruction_s (Copy reg_idx expr_val) after_expr >>
            return after_expr
    ) >>= return

lower_stmt (Located _ (AST.DStmt'Ret expr)) root cur_block =
    add_basic_block_s "after_return" >>= \ after_return_idx ->

    lower_expr expr root cur_block >>= \ (after_expr_idx, m_ret_val) -> m_ret_val |>>=? (return after_return_idx) $ \ ret_val ->

    State.get >>= \ (_, _, fun) ->

    add_instruction_s (Copy (get_ret_reg fun) ret_val) after_expr_idx >>
    add_br_s (BrGoto $ get_exit_block fun) after_expr_idx >>

    return after_return_idx
-- lowering declarations {{{1
instance Parent p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) root parent ir_builder = ddeclare sf root parent ir_builder
    ddeclare (Located sp (AST.DDecl'Impl _ _)) _ parent ir_builder =
        let warn = Unimplemented "'impl' blocks" sp -- TODO
        in (parent, add_error warn ir_builder)

    ddefine (Located _ (AST.DDecl'Fun sf)) root = ddefine sf root
    ddefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdeclare (Located _ (AST.DDecl'Fun sf)) root = vdeclare sf root
    vdeclare (Located _ (AST.DDecl'Impl _ _)) _ = (,)

    vdefine (Located _ (AST.DDecl'Fun sf)) root = vdefine sf root
    vdefine (Located _ (AST.DDecl'Impl _ _)) _ = (,)
