{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    DeclSymbol :: (Typeable d, DeclSpan d, Describe d,
            ParentR d DeclSymbol String, ParentW d DeclSymbol String,
            ParentR d Value String,      ParentW d Value String) => d -> DeclSymbol

instance ParentR DeclSymbol DeclSymbol String where
    get_child_map (DeclSymbol d) = get_child_map d
instance ParentW DeclSymbol DeclSymbol String where
    add name child (DeclSymbol ds) =
        let (replaced, added) = add name child ds
        in (replaced, DeclSymbol added)
instance ParentR DeclSymbol Value String where
    get_child_map (DeclSymbol d) = get_child_map d
instance ParentW DeclSymbol Value String where
    add name child (DeclSymbol ds) =
        let (replaced, added) = add name child ds
        in (replaced, DeclSymbol added)

instance DeclSpan DeclSymbol where
    decl_span (DeclSymbol ds) = decl_span ds
instance Describe DeclSymbol where
    describe (DeclSymbol ds) = describe ds

ds_cast :: Typeable r => DeclSymbol -> Maybe r
ds_cast (DeclSymbol v) = cast v

