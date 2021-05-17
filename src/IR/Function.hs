module IR.Function
    ( Function
    , BasicBlock
    , Register
    , FValue
    , Instruction
    , Br

    , BlockIdx
    , RegisterIdx
    , InstructionIdx

    , new_function

    , get_param_regs
    , get_register

    , function_not_defined
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
    = Copy Register FValue
    | Call Function [FValue]
    | Addrof Register Mutability
    | DerefPtr FValue

data FValue
    = FVGlobalValue Value
    | FVRegister RegisterIdx
    | FVConstInt Integer
    | FVConstFloat Double
    | FVConstBool Bool
    | FVConstChar Char
    | FVVoid
    | FVInstruction InstructionIdx

data Br
    = BrRet
    | BrGoto BasicBlock
    | BrCond FValue BasicBlock BasicBlock

instance DeclSpan Register where
    decl_span _ (Register _ _ sp) = Just sp
instance Describe Register where
    describe irctx (Register ty_idx muty _) =
        let muty_str = case muty of
                Immutable -> "immutable"
                Mutable -> "mutable"

            ty_str = stringify_tyidx irctx ty_idx

        in muty_str ++ " register of type '" ++ ty_str ++ "'"

instance DeclSpan Function where
    decl_span _ f = Just $ get_span f
instance Describe Function where
    describe _ f = "function named '" ++ get_name f ++ "'"

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

get_register :: Function -> RegisterIdx -> Register
get_register fun (RegisterIdx idx) = get_registers fun !! idx

function_not_defined :: Function -> Bool
function_not_defined = (2==) . length . get_blocks
