module IR.FunctionPointer
    ( FunctionPointer
    , new_function_pointer
    , get_fptr_pointee
    , get_function_idx
    ) where

import IR.IRCtx
import IR.Type
import IR.Function

import IR.DeclSpan
import IR.Describe
import IR.Typed
import IR.Value

import Interner

import qualified Data.Map as Map(empty)

data FunctionPointer = FunctionPointer { get_ty :: InternerIdx Type, get_function_idx :: InternerIdx Function }

new_function_pointer :: InternerIdx Function -> IRCtx -> (FunctionPointer, IRCtx)
new_function_pointer fun_idx irctx =
    let fun = get_function irctx fun_idx
        fptr_type = FunctionPointerType Map.empty (get_ret_type fun) (get_param_types fun)
        (fptr_tyidx, irctx') = get_ty_irctx fptr_type irctx
    in (FunctionPointer fptr_tyidx fun_idx, irctx')

get_fptr_pointee :: IRCtx -> FunctionPointer -> Function
get_fptr_pointee irctx (FunctionPointer _ fidx) = get_function irctx fidx

instance DeclSpan FunctionPointer where
    decl_span irctx (FunctionPointer _ fidx) = decl_span irctx (get_function irctx fidx)
instance Describe FunctionPointer where
    describe _ _ = "constant function pointer"
instance Typed FunctionPointer where
    type_of _ = get_ty

instance ApplyToV FunctionPointer where
    apply_to_v = id

instance IsValue FunctionPointer
