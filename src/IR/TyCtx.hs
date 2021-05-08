module IR.TyCtx
    ( TyCtx
    , empty_tyctx
    , get_ty
    ) where

import IR.Type
import IR.TyIdx

import Data.List(findIndex)

newtype TyCtx = TyCtx [Type]

empty_tyctx :: TyCtx
empty_tyctx = TyCtx []

get_ty :: Type -> TyCtx -> (TyIdx, TyCtx)
get_ty ty ctx@(TyCtx tys) =
    case findIndex (ty_eq ty) tys of
        Just idx -> (TyIdx idx, ctx)
        Nothing -> (TyIdx $ length tys, TyCtx $ tys ++ [ty])
