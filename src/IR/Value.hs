{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Value
    ( Value(..)
    , IsValue
    ) where

import IR.DeclSpan

class (DeclSpan ctx v {- TODO: Typed -}) => IsValue ctx v
data Value ctx = forall v. IsValue ctx v => Value v
