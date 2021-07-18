{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Value
    ( Value(..)
    , IsValue
    , v_cast
    ) where

import IR.DeclSpan

import Data.Typeable (Typeable, cast)

class (Typeable v, DeclSpan ctx v {- TODO: Typed -}) => IsValue ctx v

data Value ctx = forall v. IsValue ctx v => Value v

v_cast :: IsValue ctx v => Value ctx -> Maybe v
v_cast (Value v) = cast v
