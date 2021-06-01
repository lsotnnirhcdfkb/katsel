{-# LANGUAGE GADTs #-}

module IR.Value
    ( Value(..)
    , IsValue
    , value_cast
    ) where

import IR.DeclSpan
import IR.Describe
import IR.Typed
import IR.PrintClasses

import Data.Typeable(Typeable, cast)

class (Typeable v, VPrint v, DeclSpan v, Describe v, Typed v) => IsValue v where

data Value where
    Value :: IsValue v => v -> Value

instance DeclSpan Value where
    decl_span irctx (Value v) = decl_span irctx v
instance Describe Value where
    describe irctx (Value v) = describe irctx v
instance Typed Value where
    type_of irctx (Value v) = type_of irctx v
instance VPrint Value where
    v_print irctx (Value v) = v_print irctx v

value_cast :: Typeable r => Value -> Maybe r
value_cast (Value v) = cast v
