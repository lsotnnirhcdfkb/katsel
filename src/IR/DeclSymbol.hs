{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSymbol
    ( DeclSymbol (..)
    , IsDeclSymbol
    ) where

import IR.DeclSpan

import Data.Typeable (Typeable, cast)

class (Typeable d, DeclSpan ctx d) => IsDeclSymbol ctx d

data DeclSymbol ctx = forall d. IsDeclSymbol ctx d => DeclSymbol d
