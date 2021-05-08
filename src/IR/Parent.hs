{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module IR.Parent
    ( ParentR(..)
    , ParentW(..)
    , ParentRW
    , add_replace
    , add_noreplace
    ) where

import Data.Map(Map)
import qualified Data.Map as Map

class Ord i => ParentR p c i | p c -> i where
    get_child_map :: p -> Map i c
    get :: i -> p -> Maybe c

    get ind parent = Map.lookup ind (get_child_map parent)

class Ord i => ParentW p c i | p c -> i where
    add :: i -> c -> p -> (Maybe c, p)

add_replace :: ParentW p c i => i -> c -> p -> p
add_replace i c p = snd $ add i c p

add_noreplace :: ParentW p c i => i -> c -> p -> Either c p
add_noreplace i c p =
    case add i c p of
        (Just old, _) -> Left old
        (Nothing, parent) -> Right parent

type ParentRW p c i = (ParentR p c i, ParentW p c i)
