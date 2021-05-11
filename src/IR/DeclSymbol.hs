{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module IR.DeclSymbol
    ( DeclSymbol(..)
    , ds_cast
    ) where

import IR.Describe
import IR.DeclSpan
import IR.Parent
import IR.Value

import Data.Typeable(Typeable, cast)

data DeclSymbol where
    DeclSymbol :: (Typeable d, DeclSpan d, Describe d, Parent d DeclSymbol String, Parent d Value String) => d -> DeclSymbol

instance Parent DeclSymbol DeclSymbol String where
    get_child_map irctx (DeclSymbol d) = get_child_map irctx d
    add irctx name child (DeclSymbol ds) =
        let (replaced, added) = add irctx name child ds
        in (replaced, DeclSymbol added)

instance Parent DeclSymbol Value String where
    get_child_map irctx (DeclSymbol d) = get_child_map irctx d
    add irctx name child (DeclSymbol ds) =
        let (replaced, added) = add irctx name child ds
        in (replaced, DeclSymbol added)

instance DeclSpan DeclSymbol where
    decl_span irctx (DeclSymbol ds) = decl_span irctx ds
instance Describe DeclSymbol where
    describe irctx (DeclSymbol ds) = describe irctx ds

ds_cast :: Typeable r => DeclSymbol -> Maybe r
ds_cast (DeclSymbol v) = cast v

