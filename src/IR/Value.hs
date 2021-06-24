{-# LANGUAGE GADTs #-}

module IR.Value
    ( Value(..)
    , IsValue
    , value_cast
    , ApplyToV(..)
    ) where

import IR.DeclSpan
import IR.Describe
import IR.Typed

import {-# SOURCE #-} IR.FunctionPointer

import Data.Typeable(Typeable, cast)

class (Typeable v, DeclSpan v, Describe v, Typed v, ApplyToV v) => IsValue v where

class ApplyToV v where
    apply_to_v :: (FunctionPointer -> r) -> v -> r

data Value where
    Value :: IsValue v => v -> Value

instance DeclSpan Value where
    decl_span irctx (Value v) = decl_span irctx v
instance Describe Value where
    describe irctx (Value v) = describe irctx v
instance Typed Value where
    type_of irctx (Value v) = type_of irctx v

instance ApplyToV Value where
    apply_to_v f (Value v) = apply_to_v f v

value_cast :: Typeable r => Value -> Maybe r
value_cast (Value v) = cast v
