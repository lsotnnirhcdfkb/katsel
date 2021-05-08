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
import qualified Message.Underlines as MsgUnds

import Location

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List(foldl')
import Data.Maybe(catMaybes)

import Data.Typeable(Typeable, cast)

import qualified Control.Monad.State.Lazy as State(State, state, execState, get, put)

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

new_dsid :: [String] -> resolve -> DSIRId resolve
new_dsid segments _ = DSIRId segments
new_vid :: [String] -> resolve -> VIRId resolve
new_vid segments _ = VIRId (DSIRId $ init segments) (last segments)

resolve_dsid :: Typeable r => IRRO Module -> DSIRId r -> Maybe r
resolve_dsid (IRRO root) (DSIRId path) =
    foldl' next (Just $ DeclSymbol root) path >>= cast
    where
        next (Just ds) name = get name ds :: Maybe DeclSymbol
        next Nothing _ = Nothing

resolve_vid :: Typeable r => IRRO Module -> VIRId r -> Maybe r
resolve_vid root (VIRId parent childname) =
    child >>= cast
    where
        parent_resolved = resolve_dsid root parent
        child = parent_resolved >>= get childname :: Maybe Value
-- IR datatypes {{{1
-- DeclSpan class {{{2
class DeclSpan h where
    decl_span :: h -> Maybe Span
-- Describe class {{{2
class Describe d where
    describe :: d -> String
-- Parent things {{{2
class Ord i => ParentR p c i | p c -> i where
    get_child_map :: p -> Map i c
    get :: i -> p -> Maybe c

    get ind parent = Map.lookup ind (get_child_map parent)

class Ord i => ParentW p c i | p c -> i where
    add :: i -> c -> p -> (Maybe c, p)

add_replace :: ParentW p c i => i -> c -> p -> p
add_replace i c p = snd $ add i c p

add_noreplace :: ParentW p c i => i -> c -> p -> Either c p
add_noreplace i c p =
    case add i c p of
        (Just old, _) -> Left old
        (Nothing, parent) -> Right parent

newtype IRRO a = IRRO a
newtype IRWO a = IRWO a

instance (Ord i, ParentR a c i) => ParentR (IRRO a) (IRRO c) i where
    get_child_map (IRRO ro) = Map.map IRRO $ get_child_map ro

instance (Ord i, ParentW a c i) => ParentW (IRWO a) (IRWO c) i where
    add ind (IRWO child) (IRWO wo) =
        let (replaced, added) = add ind child wo
        in (IRWO <$> replaced, IRWO added)

ro_cast :: (Typeable a, Typeable b) => IRRO a -> Maybe (IRRO b)
ro_cast (IRRO a) = IRRO <$> cast a
wo_cast :: (Typeable a, Typeable b) => IRWO a -> Maybe (IRWO b)
wo_cast (IRWO a) = IRWO <$> cast a

type ParentRW p c i = (ParentR p c i, ParentW p c i)
-- DeclSymbols {{{2
data DeclSymbol where
    DeclSymbol :: (Typeable d, DeclSpan d, Describe d,
            ParentR d DeclSymbol String, ParentW d DeclSymbol String,
            ParentR d Value String,      ParentW d Value String) => d -> DeclSymbol

instance ParentR DeclSymbol DeclSymbol String where
    get_child_map (DeclSymbol d) = get_child_map d
instance ParentW DeclSymbol DeclSymbol String where
    add name child (DeclSymbol ds) =
        let (replaced, added) = add name child ds
        in (replaced, DeclSymbol added)
instance ParentR DeclSymbol Value String where
    get_child_map (DeclSymbol d) = get_child_map d
instance ParentW DeclSymbol Value String where
    add name child (DeclSymbol ds) =
        let (replaced, added) = add name child ds
        in (replaced, DeclSymbol added)

instance DeclSpan DeclSymbol where
    decl_span (DeclSymbol ds) = decl_span ds
instance Describe DeclSymbol where
    describe (DeclSymbol ds) = describe ds

ds_cast :: Typeable r => DeclSymbol -> Maybe r
ds_cast (DeclSymbol v) = cast v
-- Type {{{3
newtype TyIdx = TyIdx Int
data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap TyIdx [(Mutability, TyIdx)]
    | VoidType DSMap
    | PointerType DSMap Mutability TyIdx

instance ParentR Type DeclSymbol String where
    get_child_map (FloatType dsmap _) = dsmap
    get_child_map (IntType dsmap _ _) = dsmap
    get_child_map (CharType dsmap) = dsmap
    get_child_map (BoolType dsmap) = dsmap
    get_child_map (FunctionType dsmap _ _) = dsmap
    get_child_map (VoidType dsmap) = dsmap
    get_child_map (PointerType dsmap _ _) = dsmap
-- Module {{{3
data Module = Module DSMap VMap Span

instance ParentR Module DeclSymbol String where
    get_child_map (Module dsmap _ _) = dsmap
instance ParentR Module Value String where
    get_child_map (Module _ vmap _) = vmap

instance ParentW Module DeclSymbol String where
instance ParentW Module Value String where

instance DeclSpan Module where
    decl_span (Module _ _ sp) = Just sp
instance Describe Module where
    describe _ = "module being compiled"
-- Values {{{2
data Value where
    Value :: (Typeable v, DeclSpan v, Describe v) => v -> Value

instance DeclSpan Value where
    decl_span (Value v) = decl_span v
instance Describe Value where
    describe (Value v) = describe v

value_cast :: Typeable r => Value -> Maybe r
value_cast (Value v) = cast v
-- Function {{{3
data Function
    = Function
      { get_function_blocks :: [BasicBlock]
      , get_function_registers :: [Register]
      , get_function_ret_reg :: Int
      , get_function_param_regs :: [Int]
      , get_function_type :: TyIdx
      , get_function_span :: Span
      , get_function_name :: String
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

instance DeclSpan Function where
    decl_span f = Just $ get_function_span f
instance Describe Function where
    describe f = "function named " ++ get_function_name f
-- building the IR {{{1
-- IRBuildError {{{
data IRBuildError
    = DuplicateValue String (IRWO Value) Value
    | Unsupported String Span

instance Message.ToDiagnostic IRBuildError where
    to_diagnostic (DuplicateValue name (IRWO old) new) =
        let m_oldsp = decl_span old
            m_newsp = decl_span new
            old_desc = describe old
            new_desc = describe new

            if_span m_sp ty imp msg =
                case m_sp of
                    Just sp -> Right $ MsgUnds.Message sp ty imp msg
                    Nothing -> Left $ Message.Note msg
            oldmsg = if_span m_oldsp MsgUnds.Note MsgUnds.Secondary $ "'" ++ name ++ "' already declared as " ++ old_desc
            newmsg = if_span m_newsp MsgUnds.Error MsgUnds.Primary $ "'" ++ name ++ "' redeclared as " ++ new_desc

            totalmsgs = [oldmsg, newmsg]

            underlines_section =
                case [x | Right x <- [oldmsg, newmsg]] of
                    [] -> Nothing
                    msgs -> Just $ Message.Underlines $ MsgUnds.UnderlinesSection msgs
            notes = [Just x | Left x <- [oldmsg, newmsg]]
            sections = catMaybes $ underlines_section : notes
        in Message.SimpleDiag Message.Error m_oldsp Nothing (Just "redecl-val") sections
    to_diagnostic (Unsupported name sp) =
        Message.SimpleDiag Message.Warning (Just sp) Nothing Nothing
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Message sp MsgUnds.Warning MsgUnds.Primary $ name ++ " are currently unsupported"
                ]
            ]
-- }}}
-- IRBuilder {{{
data IRBuilder = IRBuilder TyCtx [IRBuildError]

add_error :: IRBuildError -> IRBuilder -> IRBuilder
add_error err (IRBuilder tyctx errs) = IRBuilder tyctx (errs ++ [err])
-- }}}
build_ir :: AST.LDModule -> (Module, TyCtx, [IRBuildError])
build_ir mod_ast@(Located mod_sp _) = (lowered_mod, tyctx, errors)
    where
        apply_stage :: (AST.LDModule -> IRRO ModParent -> IRRO Module -> IRDiff (IRWO ModParent)) -> (ModParent, IRBuilder) -> (ModParent, IRBuilder)
        apply_stage fun (mod_parent@(ModParent module_), ir_builder) =
            let (IRWO module_', ir_builder') = fun mod_ast (IRRO mod_parent) (IRRO module_) (IRWO mod_parent, ir_builder)
            in (module_', ir_builder')

        initial_parent_builder_tup = (ModParent $ Module Map.empty Map.empty mod_sp, IRBuilder (TyCtx []) [])
        (ModParent lowered_mod, IRBuilder tyctx errors) =
            -- apply_stage vdefine .
            apply_stage vdeclare .
            apply_stage ddefine .
            apply_stage ddeclare $
            initial_parent_builder_tup

-- helper functions {{{2
lower_all_in_list :: Lowerable l p => [l] -> (l -> IRRO p -> IRRO Module -> IRDiff (IRWO p)) -> IRRO p -> IRRO Module -> IRDiff (IRWO p)
lower_all_in_list things fun parent root = foldl' (.) id funs
    where
        apply_fun thing = fun thing parent root
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

ro_to_wo :: IRRO a -> IRWO a
ro_to_wo (IRRO a) = IRWO a

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
-- type resolution & type interning {{{2
resolve_path_v :: AST.LDPath -> IRRO Module -> Maybe (VIRId Value)
resolve_path_v = error "not implemented yet"
resolve_path_d :: AST.LDPath -> IRRO Module -> Maybe (DSIRId DeclSymbol)
resolve_path_d = error "not implemented yet"

resolve_ty :: AST.LDType -> IRRO Module -> (p, IRBuilder) -> (Maybe TyIdx, (p, IRBuilder))

resolve_ty (Located _ (AST.DType'Path path)) root cgtup =
    let tyidx =
            resolve_path_d path root >>= \ dsid ->
            resolve_dsid root dsid >>= \ resvoled ->
            ds_cast resvoled :: Maybe TyIdx
    in (tyidx, cgtup)

resolve_ty (Located sp (AST.DType'Pointer _ _)) root (p, ir_builder) = (Nothing, (p, add_error (Unsupported "pointer types" sp) ir_builder)) ---TODO
resolve_ty (Located sp AST.DType'This) root (p, ir_builder) = (Nothing, (p, add_error (Unsupported "'this' types" sp) ir_builder)) -- TODO

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
    add _ child (ModParent prev) = (Just prev, ModParent child)

instance ParentRW p Module () => Lowerable AST.LDModule p where
    ddeclare (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            (module_', ir_builder') = lower_all_in_list decls ddeclare module_ root (ro_to_wo module_, ir_builder)
        in (add_replace () module_' wo_parent, ir_builder')

    ddefine (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            (module_', ir_builder') = lower_all_in_list decls ddefine module_ root (ro_to_wo module_, ir_builder)
        in (add_replace () module_' wo_parent, ir_builder')

    vdeclare (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            (module_', ir_builder') = lower_all_in_list decls vdeclare module_ root (ro_to_wo module_, ir_builder)
        in (add_replace () module_' wo_parent, ir_builder')

    vdefine (Located _ (AST.DModule' decls)) parent root (wo_parent, ir_builder) =
        let (Just module_) = get () parent :: Maybe (IRRO Module)
            (module_', ir_builder') = lower_all_in_list decls vdefine module_ root (ro_to_wo module_, ir_builder)
        in (add_replace () module_' wo_parent, ir_builder')
-- lowering functions {{{2
instance ParentRW p Value String => Lowerable AST.LSFunDecl p where
    -- functions do not lower to anything during the declaration phases
    ddeclare _ _ _ = id
    ddefine _ _ _ = id

    vdeclare (Located fun_sp (AST.SFunDecl' mretty (Located _ name) params _)) roparent root = State.execState $
        (State.state $ case mretty of
            Just retty -> resolve_ty retty root
            Nothing -> \ (p, irb) ->
                let (idx, irb') = get_void_type irb
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
        in State.state (irbuilder_fun_to_cgtup_fun $ add_ty newty) >>= \ fun_ty_idx ->
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
        in State.get >>= \ before@(woparent, ir_builder) ->
            case add_noreplace name (IRWO fun_val) woparent of
                Left other_value ->
                    State.state (irbuilder_fun_to_cgtup_fun $ add_unit_res $ add_error $ DuplicateValue name other_value fun_val) >>
                    State.put before
                Right added -> State.put (added, ir_builder)

    vdefine (Located _ (AST.SFunDecl' retty (Located _ name) params expr)) roparent root cgtup =
        let m_val = get name roparent :: Maybe (IRRO Value)
            m_fun = m_val >>= ro_cast :: Maybe (IRRO Function)
        in case m_fun of
            -- silently ignore becuase the only way this can happen is if there is another global declaration
            -- that made a value of the same name that is not a function, which should already be reported as a duplicate value error
            Nothing -> cgtup

            Just old_fun -> lower_fun_body (ro_to_wo old_fun) cgtup
-- lowering function bodies {{{3
lower_fun_body :: ParentW p (IRWO Value) String => IRWO Function -> (p, IRBuilder) -> (p, IRBuilder)
lower_fun_body = error "not implemented yet"
-- lowering declarations {{{2
instance ParentRW p Value String => Lowerable AST.LDDecl p where
    ddeclare (Located _ (AST.DDecl'Fun sf)) roparent root cgtup = ddeclare sf roparent root cgtup
    ddeclare (Located sp (AST.DDecl'Impl _ _)) roparent root (woparent, ir_builder) =
        let warn = Unsupported "'impl' blocks" sp -- TODO
        in (woparent, add_error warn ir_builder)

    ddefine (Located _ (AST.DDecl'Fun sf)) roparent root cgtup = ddefine sf roparent root cgtup
    ddefine (Located _ (AST.DDecl'Impl _ _)) roparent root cgtup = cgtup

    vdeclare (Located _ (AST.DDecl'Fun sf)) roparent root cgtup = vdeclare sf roparent root cgtup
    vdeclare (Located _ (AST.DDecl'Impl _ _)) roparent root cgtup = cgtup

    vdefine (Located _ (AST.DDecl'Fun sf)) roparent root cgtup = vdefine sf roparent root cgtup
    vdefine (Located _ (AST.DDecl'Impl _ _)) roparent root cgtup = cgtup
