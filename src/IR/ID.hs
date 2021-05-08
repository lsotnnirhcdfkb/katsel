{-# LANGUAGE FlexibleContexts #-}

module IR.ID
    ( DSIRId
    , VIRId
    , new_dsid
    , new_vid
    , resolve_dsid
    , resolve_vid
    ) where

import IR.ROWO
import IR.Parent
import IR.DeclSymbol
import IR.Value
import IR.Module

import Data.Typeable(Typeable, cast)

import Data.List(foldl')

data DSIRId resolve = DSIRId [String]
data VIRId resolve = VIRId (DSIRId DeclSymbol) String

m_resolve_dsid :: Typeable r => IRRO Module -> DSIRId r -> Maybe r
m_resolve_dsid root (DSIRId segments) = foldl' next (Just $ DeclSymbol $ unirro root) segments >>= cast
    where
        next (Just ds) name = get name ds :: Maybe DeclSymbol
        next Nothing _ = Nothing
m_resolve_vid :: Typeable r => IRRO Module -> VIRId r -> Maybe r
m_resolve_vid root (VIRId ds_path v_name) = child >>= cast
    where
        parent_resolved = m_resolve_dsid root ds_path :: Maybe DeclSymbol
        child = parent_resolved >>= get v_name :: Maybe Value

new_dsid :: Typeable resolve => IRRO Module -> [String] -> Maybe (DSIRId resolve)
new_dsid root segments = dsid <$ m_resolve_dsid root dsid
    where
        dsid = DSIRId segments :: Typeable resolve => DSIRId resolve
new_vid :: Typeable resolve => IRRO Module -> [String] -> Maybe (VIRId resolve)
new_vid root segments = vid <$ m_resolve_vid root vid
    where
        vid = VIRId (DSIRId $ init segments) (last segments) :: Typeable resolve => VIRId resolve

resolve_dsid :: Typeable r => IRRO Module -> DSIRId r -> r
resolve_dsid root dsid =
    case m_resolve_dsid root dsid of
        Just x -> x
        Nothing -> error "DSIRId does not resolve correctly"
resolve_vid :: Typeable r => IRRO Module -> VIRId r -> r
resolve_vid root vid =
    case m_resolve_vid root vid of
        Just x -> x
        Nothing -> error "VIRId does not resolve correctly"
