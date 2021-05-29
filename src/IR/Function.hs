{-# LANGUAGE FlexibleInstances #-}

module IR.Function
    ( Function
    , BasicBlock
    , Register
    , FValue(..)
    , LValue(..)
    , Instruction(..)
    , Br(..)

    , BlockIdx
    , RegisterIdx
    , InstructionIdx

    , new_function

    , get_entry_block
    , get_exit_block
    , get_ret_reg
    , get_param_regs
    , get_register

    , function_not_defined

    , add_register

    , HalfwayBr(..)
    , HalfwayBlock
    , HalfwayBFV

    , make_halfway_group
    , make_halfway_block
    , make_halfway_exit

    , set_end_br
    , set_end_br'
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
import Data.List(foldl')

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
    | Addrof RegisterIdx Mutability
    | DerefPtr FValue
    deriving Eq

data LValue
    = LVRegister RegisterIdx
    deriving Eq
data FValue
    = FVGlobalValue (VIRId Value)
    | FVNLVRegister RegisterIdx
    | FVLValue LValue
    | FVConstInt Integer
    | FVConstFloat Double
    | FVConstBool Bool
    | FVConstChar Char
    | FVVoid
    | FVInstruction InstructionIdx
    deriving Eq

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
instance Typed Function  where
    type_of _ = get_type
instance VPrint Function  where

instance DeclSpan (Function, LValue) where
    decl_span irctx (f, (LVRegister reg)) = decl_span irctx $ get_register f reg
instance Describe (Function, LValue) where
    describe irctx (f, (LVRegister reg)) = describe irctx $ get_register f reg
instance Typed (Function, LValue) where
    type_of irctx (f, (LVRegister reg)) = type_of irctx $ get_register f reg

instance Typed (Module, Function, FValue) where
    type_of irctx (root, _, FVGlobalValue vid) = type_of irctx $ resolve_vid irctx root vid

new_function :: TyIdx -> [(Mutability, TyIdx, Span)] -> Span -> String -> IRCtx -> (Function, IRCtx)
new_function ret_type param_tys sp name irctx =
    let param_regs = map (\ (muty, tyidx, param_sp) -> Register tyidx muty param_sp) param_tys
        param_reg_idxs = map RegisterIdx $ take (length param_tys) [1..]

        registers = (Register ret_type Mutable sp) : param_regs

        function_type = FunctionType Map.empty ret_type $ map (\ (a, b, _) -> (a, b)) param_tys
        (function_type_idx, irctx') = get_ty_irctx function_type irctx

        blocks =
            [ BasicBlock "entry" [] Nothing
            , BasicBlock "exit" [] Nothing
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

function_not_defined :: Function -> Bool
function_not_defined = (2==) . length . get_blocks -- a function starts out with 2 blocks, and it only goes up from there; blocks cannot be removed

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
-- Block stuff {{{1
data HalfwayBr
    = HBrRet
    | HBrGoto HalfwayBlock
    | HBrCond FValue HalfwayBlock HalfwayBlock
    deriving Eq
data HalfwayBlock
    = HBlockGroup [HalfwayBlock] Int Int
    | HBlock String [Instruction] (Maybe HalfwayBr)
    | HExitBlock
    deriving Eq
type HalfwayBFV = (HalfwayBlock, FValue)

make_halfway_group :: [HalfwayBlock] -> HalfwayBlock -> HalfwayBlock -> HalfwayBlock
make_halfway_group blocks start end = HBlockGroup (start:end:blocks) 0 1
make_halfway_block :: String -> [Instruction] -> Maybe HalfwayBr -> HalfwayBlock
make_halfway_block = HBlock
make_halfway_exit :: HalfwayBlock
make_halfway_exit = HExitBlock

set_end_br :: HalfwayBlock -> Maybe HalfwayBr -> HalfwayBlock
set_end_br (HBlockGroup blocks start end) br = HBlockGroup blocks' start end
    where
        end_block = blocks !! end
        end_block' = set_end_br end_block br
        blocks' = replace_block blocks end end_block'
set_end_br (HBlock name instrs _) br = HBlock name instrs br
set_end_br HExitBlock _ = HExitBlock -- exit blocks always have a return br, so this is a no-op

set_end_br' :: HalfwayBFV -> Maybe HalfwayBr -> HalfwayBFV
set_end_br' (hb, fv) br = (set_end_br hb br, fv)

add_all_hb_blocks :: HalfwayBlock -> Function -> (Function, [((String, [Instruction], Maybe HalfwayBr), BlockIdx)], BlockIdx, BlockIdx)
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

                fill_brs f ((_, _, m_br), f_bidx) =
                    case m_br of
                        Nothing -> f
                        Just br -> add_br (convert_hbr br) f_bidx f

        f_with_first_br = add_br (BrGoto start_idx) start_block f_with_brs
    in (end_idx, f_with_first_br)
