{-# LANGUAGE FlexibleInstances #-}

module IR.Function
    ( Function
    , BasicBlock
    , Register
    , FValue(..)
    , LValue(..)
    , Instruction
    , Br

    , BlockIdx
    , InstructionIdx
    , RegisterIdx

    , TypeError
    , make_copy
    , make_call
    , make_addrof
    , make_derefptr
    , make_br_ret
    , make_br_goto
    , make_br_cond

    , print_fun

    , new_function

    , get_entry_block
    , get_exit_block
    , get_ret_reg
    , get_param_regs
    , get_ret_type
    , get_param_types
    , get_register
    , get_span
    , get_name

    , function_not_defined

    , add_register
    , add_basic_block
    , add_instruction
    , add_br

    , simplify_cfg
    ) where

import IR.TyIdx
import IR.Type
import IR.Value

import IR.Module

import IR.ID

import IR.IRCtx

import IR.DeclSpan
import IR.Describe
import IR.Typed

import Location

import qualified Message
import qualified Message.Underlines as MsgUnds

import qualified Data.Map as Map(empty)
import Data.List(intercalate, findIndex)
import Data.Maybe(mapMaybe)

data Function
    = Function
      { get_blocks :: [BasicBlock]
      , get_entry_block :: BlockIdx
      , get_exit_block :: BlockIdx

      , get_registers :: [Register]
      , get_ret_reg :: RegisterIdx
      , get_param_regs :: [RegisterIdx]

      , get_ret_type :: TyIdx
      , get_param_types :: [TyIdx]

      , get_instruction_pool :: [Instruction]

      , get_span :: Span
      , get_name :: String
      }
      deriving Eq
newtype BlockIdx = BlockIdx Int deriving Eq
newtype InstructionIdx = InstructionIdx Int deriving Eq
newtype RegisterIdx = RegisterIdx Int deriving Eq

data BasicBlock = BasicBlock String [InstructionIdx] (Maybe Br) deriving Eq
data Register = Register TyIdx Mutability Span deriving Eq
data Instruction
    = Copy LValue FValue
    | Call FValue [FValue]
    | Addrof LValue Mutability TyIdx
    | DerefPtr FValue
    deriving Eq

data LValue
    = LVRegister RegisterIdx
    deriving Eq
data FValue
    = FVGlobalValue (VIRId Value)
    | FVNLVRegister RegisterIdx
    | FVLValue LValue
    | FVConstInt Integer TyIdx
    | FVConstFloat Double TyIdx
    | FVConstBool Bool
    | FVConstChar Char
    | FVUnit
    | FVInstruction InstructionIdx
    deriving Eq

data Br
    = BrRet
    | BrGoto BlockIdx
    | BrCond FValue BlockIdx BlockIdx
    deriving Eq

instance DeclSpan Register where
    decl_span _ (Register _ _ sp) = Just sp
instance Describe Register where
    describe irctx (Register ty_idx muty _) =
        let muty_str = case muty of
                Immutable -> "immutable"
                Mutable -> "mutable"

            ty_str = stringify_tyidx irctx ty_idx

        in muty_str ++ " register of type '" ++ ty_str ++ "'"
instance Typed Register where
    type_of _ (Register ty _ _) = ty

instance DeclSpan Function where
    decl_span _ f = Just $ get_span f
instance Describe Function where
    describe _ f = "function named '" ++ get_name f ++ "'"

instance DeclSpan (Function, LValue) where
    decl_span irctx (f, LVRegister reg) = decl_span irctx $ get_register f reg
instance Describe (Function, LValue) where
    describe irctx (f, LVRegister reg) = describe irctx $ get_register f reg
instance Typed (Function, LValue) where
    type_of irctx (f, LVRegister reg) = type_of irctx $ get_register f reg

instance Typed (Module, Function, FValue) where
    type_of irctx (root, _, FVGlobalValue vid) = type_of irctx $ resolve_vid irctx root vid
    type_of irctx (_, fun, FVNLVRegister regidx) = type_of irctx $ get_register fun regidx
    type_of irctx (_, fun, FVLValue lv) = type_of irctx (fun, lv)
    type_of _ (_, _, FVConstInt _ ty) = ty
    type_of _ (_, _, FVConstFloat _ ty) = ty
    type_of irctx (_, _, FVConstBool _) = resolve_bool irctx
    type_of irctx (_, _, FVConstChar _) = resolve_char irctx
    type_of irctx (_, _, FVUnit) = resolve_unit irctx
    type_of irctx (_, fun, FVInstruction idx) = type_of irctx $ get_instruction fun idx

instance Typed Instruction where
    type_of irctx (Copy _ _) = resolve_unit irctx
    type_of _ (Addrof _ _ ty) = ty

new_function :: TyIdx -> [(Mutability, TyIdx, Span)] -> Span -> String -> Function
new_function ret_type param_tys sp name =
    let param_regs = map (\ (muty, tyidx, param_sp) -> Register tyidx muty param_sp) param_tys
        param_reg_idxs = map RegisterIdx $ take (length param_tys) [1..]

        registers = Register ret_type Mutable sp : param_regs

        blocks =
            [ BasicBlock "entry" [] Nothing
            , BasicBlock "exit" [] (Just BrRet)
            ]

        param_tys' = map (\ (_, a, _) -> a) param_tys

    in Function blocks (BlockIdx 0) (BlockIdx 1) registers (RegisterIdx 0) param_reg_idxs ret_type param_tys' [] sp name

add_register :: TyIdx -> Mutability -> Span -> Function -> (RegisterIdx, Function)
add_register tyidx muty sp fun = (reg_idx, fun')
    where
        reg = Register tyidx muty sp

        registers = get_registers fun
        fun' = fun
               { get_registers = registers ++ [reg]
               }
        reg_idx = RegisterIdx $ length registers

get_register :: Function -> RegisterIdx -> Register
get_register fun (RegisterIdx idx) = get_registers fun !! idx

get_instruction :: Function -> InstructionIdx -> Instruction
get_instruction (Function _ _ _ _ _ _ _ _ instr_pool _ _) (InstructionIdx iidx) = instr_pool !! iidx

function_not_defined :: Function -> Bool
function_not_defined = (2==) . length . get_blocks -- a function starts out with 2 blocks, and it only goes up from there; blocks cannot be removed

-- making instructions and branches for typechecking {{{1
-- TypeError datatype {{{2
data TypeError = TypeError [TypeErrorClause]
data TypeErrorClause
    = ThingIs String Span TyIdx Reason
    | ThingShouldBe String Span TyIdx Reason
data Reason = Because String | NoReason

instance Message.ToDiagnostic (TypeError, IRCtx) where
    to_diagnostic (TypeError clauses, irctx) =
        -- TODO: spans
        Message.SimpleDiag Message.Error Nothing Nothing Nothing
            [ Message.Underlines $ MsgUnds.UnderlinesSection $ map msg_clause clauses
            ]
        where
            msg_clause (ThingIs thing thingsp ty reason) =
                MsgUnds.Message thingsp MsgUnds.Note MsgUnds.Secondary $ "the " ++ thing ++ " is " ++ stringify_tyidx irctx ty ++ str_reason reason
            msg_clause (ThingShouldBe thing thingsp ty reason) =
                MsgUnds.Message thingsp MsgUnds.Note MsgUnds.Secondary $ "the " ++ thing ++ " should be " ++ stringify_tyidx irctx ty ++ str_reason reason

            str_reason (Because reason) = " because " ++ reason
            str_reason NoReason = ""
-- instructions {{{2
-- TODO: allow caller to supply their own type error
make_copy :: IRCtx -> Function -> Module -> Located LValue -> String -> Located FValue -> String -> (Either TypeError Instruction, IRCtx)
make_copy irctx fun root (Located lvsp lv) lv_name (Located fvsp fv) fv_name =
    let lvty = type_of irctx (fun, lv)
        fvty = type_of irctx (root, fun, fv)
    in ( if ty_match' irctx lvty fvty
             then Right $ Copy lv fv
             else Left $ TypeError
                         [ ThingIs lv_name lvsp lvty NoReason
                         , ThingIs fv_name fvsp fvty NoReason
                         ]
       , irctx
       )
make_call :: IRCtx -> Function -> Module -> FValue -> [FValue] -> (Either TypeError Instruction, IRCtx)
make_call = error "not implemented yet"
make_addrof :: IRCtx -> Function -> Module -> LValue -> Mutability -> (Either TypeError Instruction, IRCtx)
make_addrof irctx fun _ lv muty =
    let lvty = type_of irctx (fun, lv)
        (ty, irctx') = get_ty_irctx (PointerType Map.empty muty lvty) irctx
    in (Right $ Addrof lv muty ty, irctx')
make_derefptr :: IRCtx -> Function -> Module -> FValue -> (Either TypeError Instruction, IRCtx)
make_derefptr = error "not implemented yet"
-- branches {{{2
make_br_ret :: Br
make_br_ret = BrRet

make_br_goto :: BlockIdx -> Br
make_br_goto = BrGoto

make_br_cond :: IRCtx -> Function -> Module -> Located FValue -> BlockIdx -> BlockIdx -> (Either TypeError Br, IRCtx)
make_br_cond irctx fun root (Located condsp cond) t f =
    let cond_ty = type_of irctx (root, fun, cond)
        bool_ty = resolve_bool irctx
    in ( if ty_match' irctx cond_ty bool_ty
          then Right $ BrCond cond t f
          else Left $ TypeError
                  [ ThingIs "branch condition's type" condsp cond_ty NoReason
                  , ThingShouldBe "branch condition's type" condsp bool_ty NoReason
                  ]
       , irctx
       )
-- replace_block {{{1
replace_block :: [BasicBlock] -> Int -> BasicBlock -> [BasicBlock]
replace_block blocks idx block =
    let (keep, _:keep2) = splitAt idx blocks
    in keep ++ block : keep2
-- function cfg modification {{{1
add_basic_block :: String -> Function -> (BlockIdx, Function)
add_basic_block name fun =
    ( new_block_idx
    , fun { get_blocks = blocks ++ [new_block] }
    )
    where
        blocks = get_blocks fun
        new_block_idx = BlockIdx $ length blocks
        new_block = BasicBlock name [] Nothing
add_instruction :: Instruction -> BlockIdx -> Function -> (InstructionIdx, Function)
add_instruction instr (BlockIdx block_idx) fun = (instr_idx, fun { get_instruction_pool = new_pool, get_blocks = new_blocks })
    where
        new_pool = get_instruction_pool fun ++ [instr]
        instr_idx = InstructionIdx $ length new_pool - 1

        blocks = get_blocks fun

        (BasicBlock block_name block_instrs block_br) = blocks !! block_idx
        new_block = BasicBlock block_name (block_instrs ++ [instr_idx]) block_br
        new_blocks = replace_block blocks block_idx new_block

add_br :: Br -> BlockIdx -> Function -> Function
add_br br (BlockIdx block_idx) fun = fun { get_blocks = new_blocks }
    where
        blocks = get_blocks fun
        (BasicBlock block_name block_instrs _) = blocks !! block_idx
        new_block = BasicBlock block_name block_instrs (Just br)
        new_blocks = replace_block blocks block_idx new_block
-- cfg analysis {{{1
find_preds :: [BasicBlock] -> [(BlockIdx, [BlockIdx])]
find_preds blocks = preds
    where
        numbered_blocks :: [(BlockIdx, BasicBlock)]
        numbered_blocks = zip (map BlockIdx [0..]) blocks

        preds :: [(BlockIdx, [BlockIdx])]
        preds = map (\ (i, _) -> (i, preds_of i)) numbered_blocks

        preds_of :: BlockIdx -> [BlockIdx]
        preds_of b = map fst $ filter ((`is_pred_of` b) . snd) numbered_blocks

        is_pred_of :: BasicBlock -> BlockIdx -> Bool
        is_pred_of (BasicBlock _ _ (Just br)) dest = br `leads_to` dest
        is_pred_of (BasicBlock _ _ Nothing) _ = False

        leads_to :: Br -> BlockIdx -> Bool
        leads_to BrRet _ = False
        leads_to (BrGoto b) dest = b == dest
        leads_to (BrCond _ t f) dest = t == dest || f == dest
-- optimizations {{{1
-- helpers {{{2
keep_blocks :: [BasicBlock] -> [BlockIdx] -> [BasicBlock]
keep_blocks blocks indexes =
    let new_indexes = zip indexes (map BlockIdx [0..])

        unwrap_maybe (Just x) = x
        unwrap_maybe Nothing = error "unwrap Nothing"

        convert_block_idx = unwrap_maybe . (`lookup` new_indexes)

        convert_br BrRet = BrRet
        convert_br (BrGoto b) = BrGoto (convert_block_idx b)
        convert_br (BrCond v t f) = BrCond v (convert_block_idx t) (convert_block_idx f)

        keep_block (BlockIdx block_idx) =
            let (BasicBlock name instrs br) = blocks !! block_idx
                new_br = convert_br <$> br
            in BasicBlock name instrs new_br

    in map keep_block indexes

repeat_opt :: (Function -> Function) -> Function -> Function
repeat_opt opt f =
    let f' = opt f
    in if f == f'
        then f
        else repeat_opt opt f'
-- simplify cfg {{{2
simplify_cfg :: Function -> Function
simplify_cfg = repeat_opt $ remove . merge
    where
        remove fun =
            let blocks = get_blocks fun
                preds = find_preds blocks
                keep = map fst $ filter (\ (idx, p) -> not (null p) || idx == get_entry_block fun || idx == get_exit_block fun) preds
            in fun { get_blocks = keep_blocks blocks keep }

        merge fun =
            let blocks = get_blocks fun
                preds = find_preds blocks
                mergeable =
                    mapMaybe (
                        \ (cur, ps) ->
                        if cur == get_entry_block fun || cur == get_exit_block fun
                            then Nothing
                            else case ps of
                                    [pidx@(BlockIdx pidx')] ->
                                        let (BasicBlock _ _ p_br) = blocks !! pidx'
                                        in case p_br of
                                            Just (BrGoto _) -> Just (cur, pidx)
                                            _ -> Nothing
                                    _ -> Nothing
                    ) preds

            in case mergeable of
                (BlockIdx block, BlockIdx merge_into):_ ->
                    let (BasicBlock name instrs1 _) = blocks !! merge_into
                        (BasicBlock _ instrs2 br) = blocks !! block
                        new_block = BasicBlock name (instrs1 ++ instrs2) br

                        new_blocks = replace_block blocks merge_into new_block
                    in fun { get_blocks = new_blocks }
                _ -> fun
-- printing functions {{{1
print_fun :: IRCtx -> Function -> String
print_fun irctx fun@(Function blocks _ _ regs (RegisterIdx ret_reg_idx) param_regs _ _ _ _ _) =
    let make_comment tags = if null tags then "" else " // " ++ intercalate ", " tags

        shown_registers = concatMap show_reg $ zip ([0..] :: [Int]) regs
            where
                show_reg (reg_idx, Register reg_ty muty _) = "    " ++ muty_str ++ "#" ++ show reg_idx ++ ": " ++ stringify_tyidx irctx reg_ty ++ ";" ++ make_comment tags ++ "\n"
                    where
                        muty_str = case muty of
                            Mutable -> "mut "
                            Immutable -> ""

                        tags = (
                                if ret_reg_idx == reg_idx
                                    then ["return value register"]
                                    else []
                            ) ++ (
                                case findIndex (\ (RegisterIdx i) -> i == reg_idx) param_regs of
                                    Just i -> ["register for param " ++ show i]
                                    Nothing -> []
                            )

        shown_blocks = concatMap show_block $ zip (map BlockIdx [0..]) blocks
            where
                preds = find_preds blocks

                show_block_from_idx (BlockIdx idx) =
                    let (BasicBlock n _ _) = blocks !! idx
                    in block_name_num n idx
                block_name_num name num = name ++ "(" ++ show num ++ ")"

                show_block (block_n@(BlockIdx block_n'), BasicBlock block_name instructions m_br) =
                    "    "  ++ block_name_num block_name block_n' ++ " {" ++ make_comment tags ++ "\n" ++

                    concatMap (\ idx@(InstructionIdx idx') ->
                            let instr = get_instruction fun idx
                            in "        (%" ++ show idx' ++ ": " ++ stringify_tyidx irctx (type_of irctx instr) ++ ") = " ++ show_instruction instr ++ ";\n"
                        ) instructions ++

                    "        =>: " ++ maybe "<no br>" show_br m_br ++ ";\n" ++

                    "    }\n"
                    where
                        tags =
                            case lookup block_n preds of
                                Just block_preds
                                    | not $ null block_preds -> ["has predecessors: " ++ intercalate ", " (map show_block_from_idx block_preds)]
                                _ -> ["no predecessors"]

                show_reg (RegisterIdx i) = '#' : show i

                show_fv (FVGlobalValue vid) = intercalate "::" $ vid_segments vid
                show_fv (FVNLVRegister i) = show_reg i
                show_fv (FVLValue lv) = show_lv lv
                show_fv (FVConstInt i ty) = "(" ++ show i ++ " of " ++ stringify_tyidx irctx ty ++ ")"
                show_fv (FVConstFloat d ty) = "(" ++ show d ++ " of " ++ stringify_tyidx irctx ty ++ ")"
                show_fv (FVConstBool b) = if b then "true" else "false"
                show_fv (FVConstChar c) = ['\'', c, '\'']
                show_fv FVUnit = "unit"
                show_fv (FVInstruction (InstructionIdx iidx)) = "%" ++ show iidx

                show_lv (LVRegister i) = show_reg i

                show_instruction (Copy lv fv) = "copy " ++ show_fv fv ++ " -> " ++ show_lv lv
                show_instruction (Call fv args) = "call " ++ show_fv fv ++ " [" ++ intercalate ", " (map show_fv args) ++ "]"
                show_instruction (Addrof lv muty _) = "addrof " ++ case muty of { Mutable -> "mut "; Immutable -> ""} ++ show_lv lv
                show_instruction (DerefPtr fv) = "derefptr " ++ show_fv fv

                show_br BrRet = "ret"
                show_br (BrGoto b) = "goto " ++ show_block_from_idx b
                show_br (BrCond fv t f) = "cond " ++ show_fv fv ++ " " ++ show_block_from_idx t ++ " " ++ show_block_from_idx f

    in concat
        [ "fun {\n"
        , shown_registers
        , "\n"
        , shown_blocks
        , "}\n"
        ]
