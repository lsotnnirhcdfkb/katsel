{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Value
    ( Value(..)
    , IsValue
    , v_cast
    ) where

import IR.DeclSpan
import Data.Typeable (Typeable, cast)

class (Typeable v, Eq v, DeclSpan ctx v {- TODO: Typed -}) => IsValue ctx v
data Value ctx = forall v. IsValue ctx v => Value v

instance Eq (Value ctx) where
    Value v1 == Value v2 =
        case cast v2 of
            Just v2' -> v1 == v2'
            Nothing -> False

v_cast :: IsValue ctx v => Value ctx -> Maybe v
v_cast (Value v) = cast v
