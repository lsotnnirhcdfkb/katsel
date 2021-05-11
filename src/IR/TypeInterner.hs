module IR.TypeInterner
    ( TypeInterner
    , new_type_interner
    , get_ty
    ) where

import IR.Type
import IR.TyIdx

import Data.List(findIndex)

newtype TypeInterner = TypeInterner [Type]

new_type_interner :: TypeInterner
new_type_interner = TypeInterner []

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
