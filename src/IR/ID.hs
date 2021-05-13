{-# LANGUAGE FlexibleContexts #-}

module IR.ID
    ( DSIRId
    , VIRId
    , new_dsid
    , new_vid
    , resolve_dsid
    , resolve_vid
    ) where

import IR.Parent
import IR.DeclSymbol
import IR.Value
import IR.Module
import IR.IRCtx

import Data.Typeable(Typeable, cast)

import Data.List(foldl')

data DSIRId resolve = DSIRId [String]
data VIRId resolve = VIRId (DSIRId DeclSymbol) String

m_resolve_dsid :: Typeable r => IRCtx -> Module -> DSIRId r -> Maybe r
m_resolve_dsid irctx root (DSIRId segments) = foldl' next (Just $ DeclSymbol root) segments >>= cast
    where
        next (Just ds) name = get name (ds, irctx) :: Maybe DeclSymbol
        next Nothing _ = Nothing
m_resolve_vid :: Typeable r => IRCtx -> Module -> VIRId r -> Maybe r
m_resolve_vid irctx root (VIRId ds_path v_name) =
    (m_resolve_dsid irctx root ds_path :: Maybe DeclSymbol) >>= \ parent_resolved ->
    (get v_name (parent_resolved, irctx) :: Maybe Value) >>=
    cast

new_dsid :: Typeable resolve => IRCtx -> Module -> [String] -> Maybe (DSIRId resolve)
new_dsid irctx root segments = dsid <$ m_resolve_dsid irctx root dsid
    where
        dsid = DSIRId segments :: Typeable resolve => DSIRId resolve
new_vid :: Typeable resolve => IRCtx -> Module -> [String] -> Maybe (VIRId resolve)
new_vid irctx root segments = vid <$ m_resolve_vid irctx root vid
    where
        vid = VIRId (DSIRId $ init segments) (last segments) :: Typeable resolve => VIRId resolve

resolve_dsid :: Typeable r => IRCtx -> Module -> DSIRId r -> r
resolve_dsid irctx root dsid =
    case m_resolve_dsid irctx root dsid of
        Just x -> x
        Nothing -> error "DSIRId does not resolve correctly"
resolve_vid :: Typeable r => IRCtx -> Module -> VIRId r -> r
resolve_vid irctx root vid =
    case m_resolve_vid irctx root vid of
        Just x -> x
        Nothing -> error "VIRId does not resolve correctly"
