module IR.ConstFunctionPointer
    ( ConstFunctionPointer
    , new_function_pointer
    , get_fptr_pointee
    , get_function_idx
    ) where

import IR.IRCtx
import IR.Type
import IR.Function

import IR.DeclSpan
import IR.Typed
import IR.Value

import Interner

import qualified Data.Map as Map (empty)

data ConstFunctionPointer = ConstFunctionPointer { get_ty :: InternerIdx Type, get_function_idx :: InternerIdx Function }

new_function_pointer :: InternerIdx Function -> IRCtx -> (ConstFunctionPointer, IRCtx)
new_function_pointer fun_idx irctx =
    let fun = get_function irctx fun_idx
        fptr_type = FunctionPointerType Map.empty (get_ret_type fun) (get_param_types fun)
        (fptr_tyidx, irctx') = get_ty_irctx fptr_type irctx
    in (ConstFunctionPointer fptr_tyidx fun_idx, irctx')

get_fptr_pointee :: IRCtx -> ConstFunctionPointer -> Function
get_fptr_pointee irctx (ConstFunctionPointer _ fidx) = get_function irctx fidx

instance DeclSpan ConstFunctionPointer where
    decl_span irctx (ConstFunctionPointer _ fidx) = decl_span irctx (get_function irctx fidx)
instance Typed ConstFunctionPointer where
    type_of _ = get_ty

instance ApplyToV ConstFunctionPointer where
    apply_to_v = id

instance IsValue ConstFunctionPointer
