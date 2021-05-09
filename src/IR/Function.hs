module IR.Function
    ( Function(..)
    , Register(..)
    ) where

import IR.Type
import IR.Value

import IR.DeclSpan
import IR.Describe

import Location

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
    describe f = "function named '" ++ get_function_name f ++ "'"
