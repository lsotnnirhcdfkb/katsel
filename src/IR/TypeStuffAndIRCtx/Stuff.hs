{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.TypeStuffAndIRCtx.Stuff where

import IR.DeclSpan
import IR.DeclSymbol
import IR.Describe
import IR.MapSynonyms
import IR.Parent
import IR.Value

import Data.List(findIndex, intercalate)
import qualified Data.Map as Map

data IRCtx = IRCtx { get_type_interner :: TypeInterner }
data Signedness = Signed | Unsigned deriving Eq
data Mutability = Mutable | Immutable deriving Eq
data TypeInterner = TypeInterner { get_types_from_type_interner :: [Type] }
data TyIdx = TyIdx { untyidx :: Int } deriving Eq
data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | GenericFloatType
    | GenericIntType
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap TyIdx [TyIdx]
    | UnitType DSMap
    | PointerType DSMap Mutability TyIdx

new_irctx :: IRCtx
new_irctx = IRCtx new_type_interner

new_type_interner :: TypeInterner
new_type_interner = TypeInterner
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
    , GenericFloatType
    , GenericIntType
    , CharType Map.empty
    , BoolType Map.empty
    , UnitType Map.empty
    ]

iter_tyidxs_from_type_interner :: TypeInterner -> [TyIdx]
iter_tyidxs_from_type_interner (TypeInterner tys) = take (length tys) (TyIdx <$> [0..])

resolve_float32, resolve_float64, resolve_uint8, resolve_uint16, resolve_uint32, resolve_uint64, resolve_sint8, resolve_sint16, resolve_sint32, resolve_sint64, resolve_generic_float, resolve_generic_int, resolve_char, resolve_bool, resolve_unit :: IRCtx -> TyIdx
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
resolve_generic_float = fst . get_ty_irctx GenericFloatType
resolve_generic_int = fst . get_ty_irctx GenericIntType
resolve_char = fst . get_ty_irctx (CharType Map.empty)
resolve_bool = fst . get_ty_irctx (BoolType Map.empty)
resolve_unit = fst . get_ty_irctx (UnitType Map.empty)

get_ty_irctx :: Type -> IRCtx -> (TyIdx, IRCtx)
get_ty_irctx ty (IRCtx interner) =
    let (idx, interner') = get_ty ty interner
    in (idx, IRCtx interner')

resolve_tyidx :: TypeInterner -> TyIdx -> Type
resolve_tyidx (TypeInterner tys) (TyIdx idx) = tys !! idx

resolve_tyidx_irctx :: IRCtx -> TyIdx -> Type
resolve_tyidx_irctx (IRCtx interner) = resolve_tyidx interner

apply_to_tyidx :: (Type -> a) -> IRCtx -> TyIdx -> a
apply_to_tyidx fun irctx idx = fun $ resolve_tyidx_irctx irctx idx

replace_ty :: IRCtx -> TyIdx -> Type -> IRCtx
replace_ty (IRCtx (TypeInterner tys)) (TyIdx tyidx) ty =
    let (keep, _:keep2) = splitAt tyidx tys
        tys' = keep ++ ty : keep2
        interner' = TypeInterner tys'
    in IRCtx interner'

get_ty :: Type -> TypeInterner -> (TyIdx, TypeInterner)
get_ty ty ctx@(TypeInterner tys) =
    case findIndex (ty_eq ty) tys of
        Just idx -> (TyIdx idx, ctx)
        Nothing -> (TyIdx $ length tys, TypeInterner $ tys ++ [ty])

ty_eq :: Type -> Type -> Bool

ty_eq (FloatType _ size_a) (FloatType _ size_b)
    | size_a == size_b = True

ty_eq (IntType _ size_a sign_a) (IntType _ size_b sign_b)
    | size_a == size_b && sign_a == sign_b = True

ty_eq (FunctionType _ ret_a params_a) (FunctionType _ ret_b params_b)
    | ret_a == ret_b && params_a == params_b = True

ty_eq (PointerType _ muty_a pointee_a) (PointerType _ muty_b pointee_b)
    | muty_a == muty_b && pointee_a == pointee_b = True

ty_eq (CharType _) (CharType _) = True
ty_eq (BoolType _) (BoolType _) = True
ty_eq (UnitType _) (UnitType _) = True
ty_eq GenericIntType GenericIntType = True
ty_eq GenericFloatType GenericFloatType = True

ty_eq _ _ = False

-- TODO: type matching
ty_match :: Type -> Type -> Bool
ty_match = ty_eq

ty_match' :: IRCtx -> TyIdx -> TyIdx -> Bool
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
stringify_ty _ GenericIntType = "<int>"
stringify_ty _ GenericFloatType = "<float>"
stringify_ty _ (CharType _) = "char"
stringify_ty _ (BoolType _) = "bool"
stringify_ty irctx (FunctionType _ ret_idx params) =
    let ret_str = stringify_tyidx irctx ret_idx
        param_strs = map (stringify_tyidx irctx) params
    in "fun(" ++ intercalate ", " param_strs ++ "): " ++ ret_str
stringify_ty _ (UnitType _) = "unit"
stringify_ty irctx (PointerType _ muty pointee) =
    let muty_str = case muty of
            Mutable -> "mut "
            Immutable -> ""
    in "*" ++ muty_str ++ stringify_tyidx irctx pointee

stringify_tyidx :: IRCtx -> TyIdx -> String
stringify_tyidx irctx idx = stringify_ty irctx $ resolve_tyidx_irctx irctx idx

instance DeclSpan TyIdx where
    decl_span irctx idx = decl_span irctx $ resolve_tyidx_irctx irctx idx
instance Describe TyIdx where
    describe irctx idx = describe irctx $ resolve_tyidx_irctx irctx idx
instance Parent TyIdx DeclSymbol String where
    get_child_map (idx, irctx) = get_child_map (resolve_tyidx_irctx irctx idx, irctx)
    add i child (tyidx, irctx) =
        let resolved_ty = resolve_tyidx_irctx irctx tyidx
            (old_child, (resolved_ty', irctx')) = add i child (resolved_ty, irctx)
            irctx'' = replace_ty irctx' tyidx resolved_ty'
        in (old_child, (tyidx, irctx''))
-- TODO: maybe this shouldn't be copy and pasted
instance Parent TyIdx Value String where
    get_child_map (idx, irctx) = get_child_map (resolve_tyidx_irctx irctx idx, irctx)
    add i child (tyidx, irctx) =
        let resolved_ty = resolve_tyidx_irctx irctx tyidx
            (old_child, (resolved_ty', irctx')) = add i child (resolved_ty, irctx)
            irctx'' = replace_ty irctx' tyidx resolved_ty'
        in (old_child, (tyidx, irctx''))
instance ApplyToDS TyIdx where
    apply_to_ds _ f = f
instance IsDeclSymbol TyIdx

instance DeclSpan Type where
    -- all the types currently implemented are primitive types or types that otherwise wouldn't have declaration spans anyway, so this is a constant Nothing
    decl_span _ _ = Nothing
instance Describe Type where
    describe _ (FloatType _ size) = "primitive " ++ show size ++ "-bit floating-point type"
    describe _ (IntType _ size signedness) =
        let signedness_str = case signedness of
                Unsigned -> "unsigned"
                Signed -> "signed"
        in "primitive " ++ show size ++ "-bit " ++ signedness_str ++ " integer type"
    describe _ GenericFloatType = "generic float type"
    describe _ GenericIntType = "generic integer type"
    describe _ (CharType _) = "primitive character type"
    describe _ (BoolType _) = "primitive bool type"
    describe _ (FunctionType _ _ _) = "function type" -- TODO: put argument types and return type here?
    describe _ (UnitType _) = "primitive unit type"
    describe _ (PointerType _ _ _) = "pointer type" -- TODO: put pointee type here?

instance Parent Type DeclSymbol String where
    get_child_map (FloatType dsmap _, _) = dsmap
    get_child_map (IntType dsmap _ _, _) = dsmap
    get_child_map (GenericFloatType, _) = Map.empty
    get_child_map (GenericIntType, _) = Map.empty
    get_child_map (CharType dsmap, _) = dsmap
    get_child_map (BoolType dsmap, _) = dsmap
    get_child_map (FunctionType dsmap _ _, _) = dsmap
    get_child_map (UnitType dsmap, _) = dsmap
    get_child_map (PointerType dsmap _ _, _) = dsmap

instance Parent Type Value String where
    get_child_map _ = Map.empty
