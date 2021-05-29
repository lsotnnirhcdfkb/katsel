{-# LANGUAGE FlexibleInstances #-}

module IR.Function
    ( Function
    , BasicBlock
    , Register
    , FValue(..)
    , LValue(..)
    , Instruction

    , BlockIdx
    , RegisterIdx
    , InstructionIdx

    , TypeError
    , make_copy
    , make_call
    , make_addrof
    , make_derefptr
    , make_br_ret
    , make_br_goto
    , make_br_cond

    , new_function

    , get_entry_block
    , get_exit_block
    , get_ret_reg
    , get_param_regs
    , get_register

    , function_not_defined

    , add_register

    , HalfwayBr
    , HalfwayBlock
    , HalfwayBFV

    , make_halfway_group
    , make_halfway_block
    , make_halfway_exit

    , apply_halfway
    ) where

import IR.Type
import IR.Value

import IR.Module

import IR.ID

import IR.IRCtx

import IR.DeclSpan
import IR.Describe
import IR.Typed
import IR.PrintClasses

import Location

import qualified Data.Map as Map(empty)
import Data.List(foldl', intercalate, findIndex, nub)

data Function
    = Function
      { get_blocks :: [BasicBlock]
      , get_entry_block :: BlockIdx
      , get_exit_block :: BlockIdx

      , get_registers :: [Register]
      , get_ret_reg :: RegisterIdx
      , get_param_regs :: [RegisterIdx]

      , get_type :: TyIdx

      , get_span :: Span
      , get_name :: String
      }
newtype BlockIdx = BlockIdx Int deriving Eq
newtype RegisterIdx = RegisterIdx Int deriving Eq
data InstructionIdx = InstructionIdx BlockIdx Int deriving Eq

data BasicBlock = BasicBlock String [Instruction] (Maybe Br)
data Register = Register TyIdx Mutability Span
data Instruction
    = Copy LValue FValue
    | Call FValue [FValue]
    | Addrof LValue Mutability
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
    | FVVoid
    | FVInstruction InstructionIdx
    deriving Eq

-- these branches don't need type checking because they are only created by applying halfway branches, which are already type-checked
data Br
    = BrRet
    | BrGoto BlockIdx
    | BrCond FValue BlockIdx BlockIdx

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
instance Typed Function where
    type_of _ = get_type
instance VPrint Function where
    v_print irctx (Function blocks (BlockIdx entry_block_idx) (BlockIdx exit_block_idx) regs (RegisterIdx ret_reg_idx) param_regs _ _ _) =
        let
            make_comment tags = if null tags then "" else " // " ++ intercalate ", " tags

            shown_registers = concatMap show_reg $ zip ([0..] :: [Int]) regs
                where
                    show_reg (reg_idx, Register reg_ty muty _) = "    " ++ muty_str ++ "#" ++ show reg_idx ++ ": " ++ stringify_tyidx irctx reg_ty ++ make_comment tags ++ "\n"
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

            shown_blocks = concatMap show_block $ zip ([0..] :: [Int]) blocks
                where
                    show_block_from_idx (BlockIdx idx) =
                        let (BasicBlock n _ _) = blocks !! idx
                        in block_name_num n idx
                    block_name_num name num = name ++ "(" ++ show num ++ ")"

                    show_block (block_n, BasicBlock block_name instructions m_br) =
                        "    "  ++ block_name_num block_name block_n ++ ": {" ++ make_comment tags ++ "\n" ++

                        concatMap (\ (idx, instr) -> "        (%" ++ show idx ++ ": " ++ stringify_tyidx irctx (type_of irctx instr) ++ ") = " ++ show_instruction instr ++ ";\n") (zip ([0..] :: [Int]) instructions) ++

                        "        =>: " ++ (
                            case m_br of
                                Just br -> show_br br
                                Nothing -> "<no br>"
                        ) ++ ";\n" ++

                        "    }\n"
                        where
                            tags = (
                                    if block_n == entry_block_idx
                                        then ["entry block"]
                                        else []
                                ) ++ (
                                    if block_n == exit_block_idx
                                        then ["exit block"]
                                        else []
                                )

                    show_fv (FVGlobalValue vid) = intercalate "::" $ vid_segments vid
                    show_fv (FVNLVRegister (RegisterIdx i)) = "#" ++ show i
                    show_fv (FVLValue lv) = show_lv lv
                    show_fv (FVConstInt i ty) = "(" ++ show i ++ " of " ++ stringify_tyidx irctx ty ++ ")"
                    show_fv (FVConstFloat d ty) = "(" ++ show d ++ " of " ++ stringify_tyidx irctx ty ++ ")"
                    show_fv (FVConstBool b) = if b then "true" else "false"
                    show_fv (FVConstChar c) = ['\'', c, '\'']
                    show_fv FVVoid = "void"
                    show_fv (FVInstruction (InstructionIdx bidx iidx)) = "%" ++ show_block_from_idx bidx ++ "." ++ show iidx

                    show_lv (LVRegister (RegisterIdx i)) = "#" ++ show i

                    show_instruction (Copy lv fv) = "copy " ++ show_fv fv ++ " -> " ++ show_lv lv
                    show_instruction (Call fv args) = "call " ++ show_fv fv ++ " [" ++ intercalate ", " (map show_fv args) ++ "]"
                    show_instruction (Addrof lv muty) = "addrof " ++ case muty of { Mutable -> "mut"; Immutable -> ""} ++ " " ++ show_lv lv
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

instance DeclSpan (Function, LValue) where
    decl_span irctx (f, (LVRegister reg)) = decl_span irctx $ get_register f reg
instance Describe (Function, LValue) where
    describe irctx (f, (LVRegister reg)) = describe irctx $ get_register f reg
instance Typed (Function, LValue) where
    type_of irctx (f, (LVRegister reg)) = type_of irctx $ get_register f reg

instance Typed (Module, Function, FValue) where
    type_of irctx (root, _, FVGlobalValue vid) = type_of irctx $ resolve_vid irctx root vid
    type_of irctx (_, fun, FVNLVRegister regidx) = type_of irctx $ get_register fun regidx
    type_of irctx (_, fun, FVLValue lv) = type_of irctx (fun, lv)
    type_of _ (_, _, FVConstInt _ ty) = ty
    type_of _ (_, _, FVConstFloat _ ty) = ty
    type_of irctx (_, _, FVConstBool _) = resolve_bool irctx
    type_of irctx (_, _, FVConstChar _) = resolve_char irctx
    type_of irctx (_, _, FVVoid) = resolve_void irctx
    type_of irctx (_, fun, FVInstruction idx) = type_of irctx $ get_instruction fun idx

instance Typed Instruction where
    type_of irctx (Copy _ _) = resolve_void irctx

new_function :: TyIdx -> [(Mutability, TyIdx, Span)] -> Span -> String -> IRCtx -> (Function, IRCtx)
new_function ret_type param_tys sp name irctx =
    let param_regs = map (\ (muty, tyidx, param_sp) -> Register tyidx muty param_sp) param_tys
        param_reg_idxs = map RegisterIdx $ take (length param_tys) [1..]

        registers = (Register ret_type Mutable sp) : param_regs

        function_type = FunctionType Map.empty ret_type $ map (\ (a, b, _) -> (a, b)) param_tys
        (function_type_idx, irctx') = get_ty_irctx function_type irctx

        blocks =
            [ BasicBlock "entry" [] Nothing
            , BasicBlock "exit" [] (Just BrRet)
            ]

    in (Function blocks (BlockIdx 0) (BlockIdx 1) registers (RegisterIdx 0) param_reg_idxs function_type_idx sp name, irctx')

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
get_instruction (Function blocks _ _ _ _ _ _ _ _) (InstructionIdx (BlockIdx bidx) iidx) =
    let (BasicBlock _ instrs _) = blocks !! bidx
    in instrs !! iidx

function_not_defined :: Function -> Bool
function_not_defined = (2==) . length . get_blocks -- a function starts out with 2 blocks, and it only goes up from there; blocks cannot be removed

-- making instructions and branches for typechecking {{{1
-- TypeError datatype {{{2
data TypeError = TypeError [TypeErrorClause]
data TypeErrorClause
    = ThingsTypeIs String TyIdx Reason
    | ThingsTypeShouldBe String TyIdx Reason
data Reason = Because String | NoReason
-- instructions {{{2
make_copy :: LValue -> FValue -> Either TypeError Instruction
make_copy lv fv = error "not implemented yet"
make_call :: FValue -> [FValue] -> Either TypeError Instruction
make_call fun args = error "not implemented yet"
make_addrof :: LValue -> Mutability -> Either TypeError Instruction
make_addrof lv muty = error "not implemented yet"
make_derefptr :: FValue -> Either TypeError Instruction
make_derefptr fv = error "not implemented yet"
-- branches {{{2
make_br_ret :: HalfwayBr
make_br_ret = HBrRet
make_br_goto :: HalfwayBlock -> HalfwayBr
make_br_goto = HBrGoto
make_br_cond :: FValue -> HalfwayBlock -> HalfwayBlock -> Either TypeError HalfwayBr
make_br_cond = error "not implemented yet"
-- replace_block {{{1
replace_block :: [b] -> Int -> b -> [b]
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
add_instruction instr block_idx@(BlockIdx block_idx') fun =
    ( instr_idx
    , fun { get_blocks = new_blocks }
    )
    where
        blocks = get_blocks fun

        (BasicBlock block_name block_instrs block_br) = blocks !! block_idx'

        new_block = BasicBlock block_name (block_instrs ++ [instr]) block_br

        new_blocks = replace_block blocks block_idx' new_block
        instr_idx = InstructionIdx block_idx $ length block_instrs

add_br :: Br -> BlockIdx -> Function -> Function
add_br br (BlockIdx block_idx) fun = fun { get_blocks = new_blocks }
    where
        blocks = get_blocks fun
        (BasicBlock block_name block_instrs _) = blocks !! block_idx
        new_block = BasicBlock block_name block_instrs (Just br)
        new_blocks = replace_block blocks block_idx new_block
-- halfway stuff {{{1
data HalfwayBr
    = HBrRet
    | HBrGoto HalfwayBlock
    | HBrCond FValue HalfwayBlock HalfwayBlock
    deriving Eq

data HalfwayBlock
    = HBlockGroup [HalfwayBlock] Int Int
    | HBlock String [Instruction] HalfwayBr
    | HExitBlock
    deriving Eq
type HalfwayBFV = (HalfwayBlock, FValue)
-- making blocks {{{2
make_halfway_group :: [HalfwayBlock] -> HalfwayBlock -> HalfwayBlock -> HalfwayBlock
make_halfway_group roots start end = HBlockGroup (start:end:discovered) 0 1
    where
        discovered = filter (\ b -> b /= start && b /= end) $ nub $ roots ++ concatMap discover (start:roots)

        discover (HBlock _ _ (HBrGoto b)) = b : discover b
        discover (HBlock _ _ (HBrCond _ t f)) = [t, f] ++ discover t ++ discover f
        discover (HBlock _ _ _) = []
        discover (HBlockGroup b _ _) = concatMap discover b
        discover HExitBlock = []

make_halfway_block :: String -> [Instruction] -> HalfwayBr -> HalfwayBlock
make_halfway_block = HBlock

make_halfway_exit :: HalfwayBlock
make_halfway_exit = HExitBlock
-- applying halfway blocks {{{2
add_all_hb_blocks :: HalfwayBlock -> Function -> (Function, [((String, [Instruction], HalfwayBr), BlockIdx)], BlockIdx, BlockIdx)
add_all_hb_blocks hb fun =
    let make_block_list (HBlock name instrs br) = [(name, instrs, br)]
        make_block_list (HBlockGroup blocks _ _) = concatMap make_block_list blocks
        make_block_list HExitBlock = [] -- exit blocks do not have a tuple form, and also should not be created

        block_list = make_block_list hb

        (fun_with_blocks, block_map) =
            foldl' add_block (fun, []) block_list
            where
                add_block (f, m) btup@(name, _, _) =
                    let (idx, f') = add_basic_block name f
                    in (f', (btup, idx):m)

        start_block_idx = get_start_block hb
        end_block_idx = get_end_block hb

        get_start_block (HBlock name instrs br) = unwrap_maybe $ lookup (name, instrs, br) block_map
        get_start_block (HBlockGroup blocks i _) = get_start_block $ blocks !! i
        get_start_block HExitBlock = get_exit_block fun

        get_end_block (HBlock name instrs br) = unwrap_maybe $ lookup (name, instrs, br) block_map
        get_end_block (HBlockGroup blocks _ i) = get_end_block $ blocks !! i
        get_end_block HExitBlock = get_exit_block fun

    in (fun_with_blocks, block_map, start_block_idx, end_block_idx)

unwrap_maybe :: Maybe a -> a
unwrap_maybe (Just x) = x
unwrap_maybe Nothing = error "unwrap_maybe got Nothing"

apply_halfway :: HalfwayBlock -> BlockIdx -> Function -> (BlockIdx, Function)
apply_halfway hb start_block fun =
    let (f_with_blocks, block_map, start_idx, end_idx) = add_all_hb_blocks hb fun

        f_with_instrs = foldl' fill_instrs f_with_blocks block_map
            where
                fill_instrs f ((_, instrs, _), f_bidx) = foldl' (\ f' instr -> snd $ add_instruction instr f_bidx f') f instrs

        f_with_brs = foldl' fill_brs f_with_instrs block_map
            where
                convert_hb :: HalfwayBlock -> BlockIdx
                convert_hb (HBlock name instrs br) = unwrap_maybe $ lookup (name, instrs, br) block_map
                convert_hb (HBlockGroup blocks start _) = convert_hb $ blocks !! start
                convert_hb HExitBlock = get_exit_block fun

                convert_hbr HBrRet = BrRet
                convert_hbr (HBrGoto dest) = BrGoto $ convert_hb dest
                convert_hbr (HBrCond c t f) = BrCond c (convert_hb t) (convert_hb f)

                fill_brs f ((_, _, br), f_bidx) = add_br (convert_hbr br) f_bidx f

        f_with_first_br = add_br (BrGoto start_idx) start_block f_with_brs
    in (end_idx, f_with_first_br)
