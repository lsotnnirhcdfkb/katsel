{-# LANGUAGE FlexibleContexts #-}

module IR.RecFindEntities(all_entities_in_mod) where

import IR.IRCtx
import IR.Parent
import IR.DeclSymbol
import IR.Module
import IR.Value
import IR.ID

import Data.Typeable(Typeable)

import qualified Data.Map as Map

all_entities_in_mod :: IRCtx -> Module -> ([(DSIRId DeclSymbol, DeclSymbol)], [(VIRId Value, Value)])
all_entities_in_mod irctx root = (rec_find_dses irctx root root_id root, rec_find_vals irctx root root_id root)
    where
        root_id = new_id_unsafe new_dsid irctx root []

new_id_unsafe :: Typeable resolve => (IRCtx -> Module -> [String] -> Maybe (i resolve)) -> IRCtx -> Module -> [String] -> i resolve
new_id_unsafe fun irctx root segments =
    case fun irctx root segments of
        Just x -> x
        Nothing -> error "cannot create id"

change_name_to_id :: Typeable resolve => (IRCtx -> Module -> [String] -> Maybe (i resolve)) -> IRCtx -> Module -> DSIRId DeclSymbol -> (String, e) -> (i resolve, e)
change_name_to_id make_id_fun irctx root parent_id (name, e) = (new_id_unsafe make_id_fun irctx root (dsid_segments parent_id ++ [name]), e)

rec_find_dses :: Parent d DeclSymbol String => IRCtx -> Module -> DSIRId DeclSymbol -> d -> [(DSIRId DeclSymbol, DeclSymbol)]
rec_find_dses irctx root parent_id parent =
    let children = Map.toAscList $ get_child_map (parent, irctx)

        children_with_ids = map (change_name_to_id new_dsid irctx root parent_id) children

        children_childern = concatMap (uncurry $ rec_find_dses irctx root) children_with_ids
    in children_with_ids ++ children_childern

rec_find_vals :: Parent d Value String => IRCtx -> Module -> DSIRId DeclSymbol -> d -> [(VIRId Value, Value)]
rec_find_vals irctx root parent_id parent =
    let children = Map.toAscList $ get_child_map (parent, irctx)
        children_with_ids = map (change_name_to_id new_vid irctx root parent_id) children
    in children_with_ids
