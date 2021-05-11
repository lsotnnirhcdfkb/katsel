{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module IR.Parent
    ( ParentR(..)
    , ParentW(..)
    , ParentRW
    , add_replace
    , add_noreplace
    ) where

import {-# SOURCE #-} IR.IRCtx

import Data.Map(Map)
import qualified Data.Map as Map

class Ord i => ParentR p c i | p c -> i where
    get_child_map :: IRCtx -> p -> Map i c
    get :: IRCtx -> i -> p -> Maybe c

    get irctx ind parent = Map.lookup ind (get_child_map irctx parent)

class Ord i => ParentW p c i | p c -> i where
    add :: IRCtx -> i -> c -> p -> (Maybe c, p)

add_replace :: ParentW p c i => IRCtx -> i -> c -> p -> p
add_replace irctx i c p = snd $ add irctx i c p

add_noreplace :: ParentW p c i => IRCtx -> i -> c -> p -> Either c p
add_noreplace irctx i c p =
    case add irctx i c p of
        (Just old, _) -> Left old
        (Nothing, parent) -> Right parent

type ParentRW p c i = (ParentR p c i, ParentW p c i)
