{-# LANGUAGE GADTs #-}

module IR.Value
    ( Value(..)
    , value_cast
    ) where

import IR.DeclSpan
import IR.Describe
import IR.Typed
import IR.Print

import Data.Typeable(Typeable, cast)

data Value where
    Value :: (Typeable v, VPrint v, DeclSpan v, Describe v, Typed v) => v -> Value

instance DeclSpan Value where
    decl_span irctx (Value v) = decl_span irctx v
instance Describe Value where
    describe irctx (Value v) = describe irctx v
instance Typed Value where
    type_of irctx (Value v) = type_of irctx v

value_cast :: Typeable r => Value -> Maybe r
value_cast (Value v) = cast v
