{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Module 
    ( Module(..)
    ) where

import IR.Parent
import IR.MapSynonyms
import IR.DeclSymbol
import IR.Value
import IR.DeclSpan
import IR.Describe

import Location

data Module = Module DSMap VMap Span

instance ParentR Module DeclSymbol String where
    get_child_map (Module dsmap _ _) = dsmap
instance ParentR Module Value String where
    get_child_map (Module _ vmap _) = vmap

instance ParentW Module DeclSymbol String where
instance ParentW Module Value String where

instance DeclSpan Module where
    decl_span (Module _ _ sp) = Just sp
instance Describe Module where
    describe _ = "module being compiled"
