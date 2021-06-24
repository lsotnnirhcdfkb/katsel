{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSymbol
    ( DeclSymbol(..)
    , IsDeclSymbol
    , ds_cast
    , ApplyToDS(..)
    ) where

import Interner

import IR.Describe
import IR.DeclSpan
import IR.Parent
import IR.Value

import {-# SOURCE #-} IR.Module
import {-# SOURCE #-} IR.Type

import Data.Typeable(Typeable, cast)

class (Typeable d, DeclSpan d, Describe d, Parent d DeclSymbol String, Parent d Value String, ApplyToDS d) => IsDeclSymbol d where

class ApplyToDS d where
    apply_to_ds :: (Module -> r) -> (InternerIdx Type -> r) -> d -> r

data DeclSymbol where
    DeclSymbol :: IsDeclSymbol d => d -> DeclSymbol

instance Parent DeclSymbol DeclSymbol String where
    get_child_map (DeclSymbol d, irctx) = get_child_map (d, irctx)
    add name child (DeclSymbol ds, irctx) =
        let (replaced, (added, irctx')) = add name child (ds, irctx)
        in (replaced, (DeclSymbol added, irctx'))

instance Parent DeclSymbol Value String where
    get_child_map (DeclSymbol d, irctx) = get_child_map (d, irctx)
    add name child (DeclSymbol ds, irctx) =
        let (replaced, (added, irctx')) = add name child (ds, irctx)
        in (replaced, (DeclSymbol added, irctx'))

instance DeclSpan DeclSymbol where
    decl_span irctx (DeclSymbol ds) = decl_span irctx ds
instance Describe DeclSymbol where
    describe irctx (DeclSymbol ds) = describe irctx ds

instance ApplyToDS DeclSymbol where
    apply_to_ds f1 f2 (DeclSymbol ds) = apply_to_ds f1 f2 ds

ds_cast :: Typeable r => DeclSymbol -> Maybe r
ds_cast (DeclSymbol v) = cast v

