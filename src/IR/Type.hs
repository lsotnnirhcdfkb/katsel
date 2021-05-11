{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Type
    ( Mutability(..)
    , Signedness(..)
    , TyIdx
    , Type(..)
    ) where

import IR.MapSynonyms
import IR.Parent
import IR.DeclSymbol
import IR.TyIdx

data Mutability = Mutable | Immutable deriving Eq
data Signedness = Signed | Unsigned deriving Eq

data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap TyIdx [(Mutability, TyIdx)]
    | VoidType DSMap
    | PointerType DSMap Mutability TyIdx

instance ParentR Type DeclSymbol String where
    get_child_map _ (FloatType dsmap _) = dsmap
    get_child_map _ (IntType dsmap _ _) = dsmap
    get_child_map _ (CharType dsmap) = dsmap
    get_child_map _ (BoolType dsmap) = dsmap
    get_child_map _ (FunctionType dsmap _ _) = dsmap
    get_child_map _ (VoidType dsmap) = dsmap
    get_child_map _ (PointerType dsmap _ _) = dsmap
