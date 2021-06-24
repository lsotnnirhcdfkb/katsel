module IR.Typed
    ( Typed(..)
    ) where

import Interner
import {-# SOURCE #-} IR.IRCtx
import {-# SOURCE #-} IR.Type

class Typed v where
    type_of :: IRCtx -> v -> InternerIdx Type
