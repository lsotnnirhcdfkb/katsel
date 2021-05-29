{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.TypeStuffAndIRCtx.Stuff where

import IR.DeclSpan
import IR.DeclSymbol
import IR.Describe
import IR.MapSynonyms
import IR.Parent
import IR.Value
import IR.PrintClasses

import Data.List(findIndex, intercalate)

data IRCtx = IRCtx TypeInterner
data Signedness = Signed | Unsigned deriving Eq
data Mutability = Mutable | Immutable deriving Eq
newtype TypeInterner = TypeInterner [Type]
data TyIdx = TyIdx { untyidx :: Int } deriving Eq
data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap TyIdx [(Mutability, TyIdx)]
    | VoidType DSMap
    | PointerType DSMap Mutability TyIdx

new_irctx :: IRCtx
new_irctx = IRCtx new_type_interner

new_type_interner :: TypeInterner
new_type_interner = TypeInterner []

get_ty_irctx :: Type -> IRCtx -> (TyIdx, IRCtx)
get_ty_irctx ty (IRCtx interner) =
    let (idx, interner') = get_ty ty interner
    in (idx, IRCtx interner')

resolve_tyidx :: TypeInterner -> TyIdx -> Type
resolve_tyidx (TypeInterner tys) (TyIdx idx) = tys !! idx

resolve_tyidx_irctx :: IRCtx -> TyIdx -> Type
resolve_tyidx_irctx (IRCtx interner) idx = resolve_tyidx interner idx

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
ty_eq (VoidType _) (VoidType _) = True

ty_eq _ _ = False

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
stringify_ty irctx (FunctionType _ ret_idx params) =
    let ret_str = stringify_tyidx irctx ret_idx
        param_strs = map (stringify_tyidx irctx . snd) params
    in "fun (" ++ intercalate ", " param_strs ++ "): " ++ ret_str
stringify_ty _ (VoidType _) = "void"
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
instance DSPrint TyIdx where
    ds_print irctx idx = ds_print irctx $ resolve_tyidx_irctx irctx idx
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
    describe _ (CharType _) = "primitive character type"
    describe _ (BoolType _) = "primitive bool type"
    describe _ (FunctionType _ _ _) = "function type" -- TODO: put argument types and return type here?
    describe _ (VoidType _) = "primitive void type"
    describe _ (PointerType _ _ _) = "pointer type" -- TODO: put pointee type here?
instance DSPrint Type where
    ds_print _ (FloatType _ size) = "primitive float type " ++ show size
    ds_print _ (IntType _ size signedness) = "primitive " ++ signedness_str ++ " int type " ++ show size
        where
            signedness_str = case signedness of
                Unsigned -> "unsigned"
                Signed -> "signed"
    ds_print _ (CharType _) = "primitive char type"
    ds_print _ (BoolType _) = "primitive bool type"
    ds_print irctx (FunctionType _ ret_ty params) = "function type fun (" ++ intercalate ", " (map (stringify_tyidx irctx . snd) params) ++ "): " ++ stringify_tyidx irctx ret_ty
    ds_print _ (VoidType _) = "primitive void type"
    ds_print irctx (PointerType _ muty ty) = "pointer type *" ++ muty_str ++ stringify_tyidx irctx ty
        where
            muty_str = case muty of
                Mutable -> "mut "
                Immutable -> ""

instance Parent Type DeclSymbol String where
    get_child_map ((FloatType dsmap _), _) = dsmap
    get_child_map ((IntType dsmap _ _), _) = dsmap
    get_child_map ((CharType dsmap), _) = dsmap
    get_child_map ((BoolType dsmap), _) = dsmap
    get_child_map ((FunctionType dsmap _ _), _) = dsmap
    get_child_map ((VoidType dsmap), _) = dsmap
    get_child_map ((PointerType dsmap _ _), _) = dsmap

instance Parent Type Value String where
