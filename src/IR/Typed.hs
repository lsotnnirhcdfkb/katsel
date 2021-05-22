module IR.Typed
    ( Typed(..)
    ) where

import {-# SOURCE #-} IR.TyIdx
import {-# SOURCE #-} IR.IRCtx

class Typed v where
    type_of :: IRCtx -> v -> TyIdx
