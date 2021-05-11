module IR.IRCtx
    ( IRCtx
    , new_irctx
    , get_ty_irctx
    ) where

import IR.TypeInterner
import IR.Type

data IRCtx = IRCtx TypeInterner

new_irctx :: IRCtx
new_irctx = IRCtx new_type_interner

get_ty_irctx :: Type -> IRCtx -> (TyIdx, IRCtx)
get_ty_irctx ty (IRCtx interner) =
    let (idx, interner') = get_ty ty interner
    in (idx, IRCtx interner')
