module IR.Function
    ( Function
    , BasicBlock
    , Register
    , FValue
    , Instruction
    , Br

    , BlockIdx
    , RegisterIdx

    , new_function
    ) where

import IR.Type
import IR.Value

import IR.IRCtx

import IR.DeclSpan
import IR.Describe

import Location

import qualified Data.Map as Map(empty)

data Function
    = Function
      { get_function_blocks :: [BasicBlock]
      , get_entry_block :: BlockIdx
      , get_exit_block :: BlockIdx

      , get_function_registers :: [Register]
      , get_function_ret_reg :: RegisterIdx
      , get_function_param_regs :: [RegisterIdx]

      , get_function_type :: TyIdx

      , get_function_span :: Span
      , get_function_name :: String
      }
newtype BlockIdx = BlockIdx Int
newtype RegisterIdx = RegisterIdx Int

data BasicBlock = BasicBlock String [Instruction] (Maybe Br)
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
    decl_span _ f = Just $ get_function_span f
instance Describe Function where
    describe _ f = "function named '" ++ get_function_name f ++ "'"

new_function :: TyIdx -> [(Mutability, TyIdx)] -> Span -> String -> IRCtx -> (Function, IRCtx)
new_function ret_type param_tys sp name irctx =
    let param_regs = map (uncurry $ flip Register) param_tys
        param_reg_idxs = map RegisterIdx $ take (length param_tys) [1..]

        registers = (Register ret_type Mutable) : param_regs

        function_type = FunctionType Map.empty ret_type param_tys
        (function_type_idx, irctx') = get_ty_irctx function_type irctx

        blocks =
            [ BasicBlock "entry" [] Nothing
            , BasicBlock "exit" [] Nothing
            ]

    in (Function blocks (BlockIdx 0) (BlockIdx 1) registers (RegisterIdx 0) param_reg_idxs function_type_idx sp name, irctx')
