{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Module
    ( Module
    , new_module
    ) where

import Interner

import SimpleLens

import Location

import IR.DeclSpan
import IR.DeclSymbol

import IR.IRCtx

import IR.ChildList

import Data.List (mapAccumL, foldl')

data Module = Module Span

new_module :: Span -> IRCtx -> (DSIdx Module, IRCtx)
new_module sp irctx =
    let (mod_idx, irctx') = get_ds (Module sp) irctx

        get_tyidx ctx (tyn, ty) =
            let (tyidx, ctx') = modify ds_interner (get_from_interner ty) (ctx :: IRCtx)
            in (ctx', (tyn, tyidx))

        (irctx'', tyidxs) =
            mapAccumL get_tyidx irctx'
                []

        irctx''' = foldl' (\ i (tyn, tyidx) -> over ds_child_list (add_replace (upcast_dsidx mod_idx) tyn tyidx) i) irctx'' tyidxs
            
    in (mod_idx, irctx''')

instance DeclSpan IRCtx Module where
    decl_span _ (Module sp) = Just sp

instance Eq Module where
    _ == _ = True

instance IsDeclSymbol IRCtx Module
