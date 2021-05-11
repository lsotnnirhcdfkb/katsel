{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.TyIdx where

import IR.DeclSpan
import IR.Describe
import IR.Parent

import IR.DeclSymbol
import IR.Value

newtype TyIdx = TyIdx { untyidx :: Int } deriving Eq

instance DeclSpan TyIdx where
instance Describe TyIdx where
instance Parent TyIdx DeclSymbol String where
instance Parent TyIdx Value String where
