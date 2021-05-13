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
    get_child_map :: (p, IRCtx) -> Map i c
    get :: i -> (p, IRCtx) -> Maybe c
    add :: i -> c -> (p, IRCtx) -> (Maybe c, (p, IRCtx))

    get ind parent_irctx_tup = Map.lookup ind (get_child_map parent_irctx_tup)

add_replace :: Parent p c i => i -> c -> (p, IRCtx) -> (p, IRCtx)
add_replace i c tup = snd $ add i c tup

add_noreplace :: Parent p c i => i -> c -> (p, IRCtx) -> Either c (p, IRCtx)
add_noreplace i c tup =
    case add i c tup of
        (Just old, _) -> Left old
        (Nothing, parent) -> Right parent
