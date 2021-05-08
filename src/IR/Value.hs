{-# LANGUAGE GADTs #-}

module IR.Value
    ( Value(..)
    , value_cast
    ) where

import IR.DeclSpan
import IR.Describe

import Data.Typeable(Typeable, cast)

data Value where
    Value :: (Typeable v, DeclSpan v, Describe v) => v -> Value

instance DeclSpan Value where
    decl_span (Value v) = decl_span v
instance Describe Value where
    describe (Value v) = describe v

value_cast :: Typeable r => Value -> Maybe r
value_cast (Value v) = cast v