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
    , add_basic_block
    , add_instruction
    , add_br
    ) where

import IR.Type
import IR.Value

import IR.Module

import IR.ID

import IR.IRCtx

import IR.DeclSpan
import IR.Describe
import IR.Typed

import Location

import qualified Data.Map as Map(empty)

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
newtype BlockIdx = BlockIdx Int
newtype RegisterIdx = RegisterIdx Int
data InstructionIdx = InstructionIdx BlockIdx Int

data BasicBlock = BasicBlock String [Instruction] (Maybe Br)
data Register = Register TyIdx Mutability Span
data Instruction
    = Copy LValue FValue
    | Call (VIRId Function) [FValue]
    | Addrof RegisterIdx Mutability
    | DerefPtr FValue

data LValue
    = LVRegister RegisterIdx
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

        new_blocks = replace_block blocks block_idx new_block
        instr_idx = InstructionIdx block_idx $ length block_instrs

add_br :: Br -> BlockIdx -> Function -> Function
add_br br block_idx@(BlockIdx block_idx') fun = fun { get_blocks = new_blocks }
    where
        blocks = get_blocks fun
        (BasicBlock block_name block_instrs _) = blocks !! block_idx'
        new_block = BasicBlock block_name block_instrs (Just br)
        new_blocks = replace_block blocks block_idx new_block

replace_block :: [BasicBlock] -> BlockIdx -> BasicBlock -> [BasicBlock]
replace_block blocks (BlockIdx idx) block =
    let (keep, _:keep2) = splitAt idx blocks
    in keep ++ block : keep2
