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
