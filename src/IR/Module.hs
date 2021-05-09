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

import qualified Data.Map as Map

import Location

data Module = Module DSMap VMap Span

instance ParentR Module DeclSymbol String where
    get_child_map (Module dsmap _ _) = dsmap
instance ParentR Module Value String where
    get_child_map (Module _ vmap _) = vmap

instance ParentW Module DeclSymbol String where
    add name ds (Module dsmap vmap sp) =
        let old_val = Map.lookup name dsmap
            dsmap' = Map.insert name ds dsmap
        in (old_val, Module dsmap' vmap sp)
instance ParentW Module Value String where
    add name ds (Module dsmap vmap sp) =
        let old_val = Map.lookup name vmap
            vmap' = Map.insert name ds vmap
        in (old_val, Module dsmap vmap' sp)

instance DeclSpan Module where
    decl_span (Module _ _ sp) = Just sp
instance Describe Module where
    describe _ = "module being compiled"
