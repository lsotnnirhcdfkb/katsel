{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module IR.Parent
    ( Parent(..)
    , add_replace
    , add_noreplace
    ) where

import {-# SOURCE #-} IR.IRCtx

import Data.Map(Map)
import qualified Data.Map as Map

class Ord i => Parent p c i | p c -> i where
    get_child_map :: IRCtx -> p -> Map i c
    get :: IRCtx -> i -> p -> Maybe c
    add :: IRCtx -> i -> c -> p -> (Maybe c, p)

    get irctx ind parent = Map.lookup ind (get_child_map irctx parent)

add_replace :: Parent p c i => IRCtx -> i -> c -> p -> p
add_replace irctx i c p = snd $ add irctx i c p

add_noreplace :: Parent p c i => IRCtx -> i -> c -> p -> Either c p
add_noreplace irctx i c p =
    case add irctx i c p of
        (Just old, _) -> Left old
        (Nothing, parent) -> Right parent
