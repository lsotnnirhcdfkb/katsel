{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSymbol
    ( DeclSymbol (..)
    , IsDeclSymbol
    , ds_cast
    ) where

import IR.DeclSpan
import Data.Typeable (Typeable, cast)

class (Typeable d, DeclSpan ctx d) => IsDeclSymbol ctx d
data DeclSymbol ctx = forall d. IsDeclSymbol ctx d => DeclSymbol d

instance Eq (DeclSymbol ctx) where
instance Ord (DeclSymbol ctx) where

ds_cast :: IsDeclSymbol ctx d => DeclSymbol ctx -> Maybe d
ds_cast (DeclSymbol d) = cast d
