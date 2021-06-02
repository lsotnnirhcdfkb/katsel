{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Module
    ( Module
    , new_module
    ) where

import IR.Parent
import IR.MapSynonyms
import IR.IRCtx
import IR.DeclSymbol
import IR.Type
import IR.Value
import IR.DeclSpan
import IR.Describe
import IR.PrintClasses

import qualified Data.Map as Map

import Location

data Module = Module DSMap VMap Span
new_module :: Span -> IRCtx -> (Module, IRCtx)
new_module sp irctx = (Module dsmap Map.empty sp, irctx)
    where
        r :: (IRCtx -> TyIdx) -> DeclSymbol
        r f = DeclSymbol $ f irctx

        dsmap = Map.fromList
            [ ("void", r resolve_void)
            , ("float", r resolve_float32)
            , ("double", r resolve_float64)
            , ("bool", r resolve_bool)
            , ("char", r resolve_char)
            , ("uint8", r resolve_uint8)
            , ("uint16", r resolve_uint16)
            , ("uint32", r resolve_uint32)
            , ("uint64", r resolve_uint64)
            , ("sint8", r resolve_sint8)
            , ("sint16", r resolve_sint16)
            , ("sint32", r resolve_sint32)
            , ("sint64", r resolve_sint64)
            ]

instance Parent Module DeclSymbol String where
    get_child_map (Module dsmap _ _, _) = dsmap
    add name ds (Module dsmap vmap sp, irctx) =
        let old_val = Map.lookup name dsmap
            dsmap' = Map.insert name ds dsmap
        in (old_val, (Module dsmap' vmap sp, irctx))

instance Parent Module Value String where
    get_child_map (Module _ vmap _, _) = vmap
    add name ds (Module dsmap vmap sp, irctx) =
        let old_val = Map.lookup name vmap
            vmap' = Map.insert name ds vmap
        in (old_val, (Module dsmap vmap' sp, irctx))

instance DeclSpan Module where
    decl_span _ (Module _ _ sp) = Just sp
instance Describe Module where
    describe _ _ = "module being compiled"
instance DSPrint Module where
    ds_print _ _ = "mod"

instance IsDeclSymbol Module
