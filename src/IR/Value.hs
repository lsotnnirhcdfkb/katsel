{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Value
    ( Value(..)
    , IsValue
    ) where

import IR.DeclSpan

import Data.Typeable (Typeable, cast)

class (Typeable v, DeclSpan ctx v {- TODO: Typed -}) => IsValue ctx v

data Value ctx = forall v. IsValue ctx v => Value v
