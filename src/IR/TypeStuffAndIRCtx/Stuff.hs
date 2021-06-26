{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.TypeStuffAndIRCtx.Stuff where

import Interner

import IR.DeclSpan
import IR.DeclSymbol
import IR.MapSynonyms
import IR.Parent
import IR.Value
import {-# SOURCE #-} IR.Function

import Data.List(intercalate)
import qualified Data.Map as Map

data IRCtx = IRCtx { get_type_interner :: Interner Type, get_function_interner :: Interner Function }
data Signedness = Signed | Unsigned deriving Eq
-- data Mutability = Mutable | Immutable deriving Eq -- TODO: mutability
data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionPointerType DSMap (InternerIdx Type) [InternerIdx Type]
    | UnitType DSMap

new_irctx :: IRCtx
new_irctx = IRCtx new_type_interner new_interner

new_type_interner :: Interner Type
new_type_interner = new_interner_with
    [ FloatType Map.empty 32
    , FloatType Map.empty 64
    , IntType Map.empty  8 Unsigned
    , IntType Map.empty 16 Unsigned
    , IntType Map.empty 32 Unsigned
    , IntType Map.empty 64 Unsigned
    , IntType Map.empty  8 Signed
    , IntType Map.empty 16 Signed
    , IntType Map.empty 32 Signed
    , IntType Map.empty 64 Signed
    , CharType Map.empty
    , BoolType Map.empty
    , UnitType Map.empty
    ]

resolve_float32, resolve_float64, resolve_uint8, resolve_uint16, resolve_uint32, resolve_uint64, resolve_sint8, resolve_sint16, resolve_sint32, resolve_sint64, resolve_char, resolve_bool, resolve_unit :: IRCtx -> InternerIdx Type
resolve_float32 = fst . get_ty_irctx (FloatType Map.empty 32)
resolve_float64 = fst . get_ty_irctx (FloatType Map.empty 64)
resolve_uint8 = fst . get_ty_irctx (IntType Map.empty  8 Unsigned)
resolve_uint16 = fst . get_ty_irctx (IntType Map.empty 16 Unsigned)
resolve_uint32 = fst . get_ty_irctx (IntType Map.empty 32 Unsigned)
resolve_uint64 = fst . get_ty_irctx (IntType Map.empty 64 Unsigned)
resolve_sint8 = fst . get_ty_irctx (IntType Map.empty  8 Signed)
resolve_sint16 = fst . get_ty_irctx (IntType Map.empty 16 Signed)
resolve_sint32 = fst . get_ty_irctx (IntType Map.empty 32 Signed)
resolve_sint64 = fst . get_ty_irctx (IntType Map.empty 64 Signed)
resolve_char = fst . get_ty_irctx (CharType Map.empty)
resolve_bool = fst . get_ty_irctx (BoolType Map.empty)
resolve_unit = fst . get_ty_irctx (UnitType Map.empty)

get_ty_irctx :: Type -> IRCtx -> (InternerIdx Type, IRCtx)
get_ty_irctx ty (IRCtx ty_interner functions) =
    let (idx, ty_interner') = add_to_interner ty_eq ty ty_interner
    in (idx, IRCtx ty_interner' functions)

replace_ty :: Type -> InternerIdx Type -> IRCtx -> IRCtx
replace_ty ty idx (IRCtx ty_interner fun_interner) = IRCtx (replace_in_interner ty idx ty_interner) fun_interner

resolve_tyidx_irctx :: IRCtx -> InternerIdx Type -> Type
resolve_tyidx_irctx (IRCtx interner _) idx = get_from_interner idx interner

apply_to_tyidx :: (Type -> a) -> IRCtx -> InternerIdx Type -> a
apply_to_tyidx fun irctx idx = fun $ resolve_tyidx_irctx irctx idx

ty_eq :: Type -> Type -> Bool

ty_eq (FloatType _ size_a) (FloatType _ size_b)
    | size_a == size_b = True

ty_eq (IntType _ size_a sign_a) (IntType _ size_b sign_b)
    | size_a == size_b && sign_a == sign_b = True

ty_eq (FunctionPointerType _ ret_a params_a) (FunctionPointerType _ ret_b params_b)
    | ret_a == ret_b && params_a == params_b = True

ty_eq (CharType _) (CharType _) = True
ty_eq (BoolType _) (BoolType _) = True
ty_eq (UnitType _) (UnitType _) = True

ty_eq _ _ = False

-- TODO: type matching
ty_match :: Type -> Type -> Bool
ty_match = ty_eq

ty_match' :: IRCtx -> InternerIdx Type -> InternerIdx Type -> Bool
ty_match' irctx a b = ty_match (resolve_tyidx_irctx irctx a) (resolve_tyidx_irctx irctx b)

stringify_ty :: IRCtx -> Type -> String
stringify_ty _ (FloatType _ 32) = "float"
stringify_ty _ (FloatType _ 64) = "double"
stringify_ty _ (FloatType _ _) = error "float type that is not size 32 or size 64"
stringify_ty _ (IntType _ size signedness) =
    let signedness_str = case signedness of
            Unsigned -> "u"
            Signed -> "s"
    in signedness_str ++ "int" ++ show size
stringify_ty _ (CharType _) = "char"
stringify_ty _ (BoolType _) = "bool"
stringify_ty irctx (FunctionPointerType _ ret_idx params) =
    let ret_str = stringify_tyidx irctx ret_idx
        param_strs = map (stringify_tyidx irctx) params
    in "fun(" ++ intercalate ", " param_strs ++ "): " ++ ret_str
stringify_ty _ (UnitType _) = "unit"

stringify_tyidx :: IRCtx -> InternerIdx Type -> String
stringify_tyidx irctx idx = stringify_ty irctx $ resolve_tyidx_irctx irctx idx

get_function :: IRCtx -> InternerIdx Function -> Function
get_function (IRCtx _ functions) idx = get_from_interner idx functions

add_function :: Function -> IRCtx -> (InternerIdx Function, IRCtx)
add_function fun (IRCtx ty_interner fun_interner) =
    let (idx, fun_interner') = add_to_interner (\ _ _ -> False) fun fun_interner
    in (idx, IRCtx ty_interner fun_interner')

replace_function :: Function -> InternerIdx Function -> IRCtx -> IRCtx
replace_function fun idx (IRCtx ty_interner fun_interner) = IRCtx ty_interner (replace_in_interner fun idx fun_interner)

instance DeclSpan (InternerIdx Type) where
    decl_span irctx idx = decl_span irctx $ resolve_tyidx_irctx irctx idx
instance Parent (InternerIdx Type) DeclSymbol String where
    get_child_map (idx, irctx) = get_child_map (resolve_tyidx_irctx irctx idx, irctx)
    add i child (tyidx, irctx) =
        let resolved_ty = resolve_tyidx_irctx irctx tyidx
            (old_child, (resolved_ty', irctx')) = add i child (resolved_ty, irctx)
            irctx'' = replace_ty resolved_ty' tyidx irctx' 
        in (old_child, (tyidx, irctx''))
-- TODO: maybe this shouldn't be copy and pasted
instance Parent (InternerIdx Type) Value String where
    get_child_map (idx, irctx) = get_child_map (resolve_tyidx_irctx irctx idx, irctx)
    add i child (tyidx, irctx) =
        let resolved_ty = resolve_tyidx_irctx irctx tyidx
            (old_child, (resolved_ty', irctx')) = add i child (resolved_ty, irctx)
            irctx'' = replace_ty resolved_ty' tyidx irctx' 
        in (old_child, (tyidx, irctx''))
instance ApplyToDS (InternerIdx Type) where
    apply_to_ds _ f = f
instance IsDeclSymbol (InternerIdx Type)

instance DeclSpan Type where
    -- all the types currently implemented are primitive types or types that otherwise wouldn't have declaration spans anyway, so this is a constant Nothing
    decl_span _ _ = Nothing

instance Parent Type DeclSymbol String where
    get_child_map (FloatType dsmap _, _) = dsmap
    get_child_map (IntType dsmap _ _, _) = dsmap
    get_child_map (CharType dsmap, _) = dsmap
    get_child_map (BoolType dsmap, _) = dsmap
    get_child_map (FunctionPointerType dsmap _ _, _) = dsmap
    get_child_map (UnitType dsmap, _) = dsmap

instance Parent Type Value String where
    get_child_map _ = Map.empty
