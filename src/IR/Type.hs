{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module IR.Type
    ( Mutability(..)
    , Signedness(..)
    , TyIdx
    , Type(..)
    , ty_eq
    ) where

import IR.MapSynonyms
import IR.Parent
import IR.DeclSymbol
import IR.TyIdx

data Mutability = Mutable | Immutable
data Signedness = Signed | Unsigned

data Type
    = FloatType DSMap Int
    | IntType DSMap Int Signedness
    | CharType DSMap
    | BoolType DSMap
    | FunctionType DSMap TyIdx [(Mutability, TyIdx)]
    | VoidType DSMap
    | PointerType DSMap Mutability TyIdx

ty_eq :: Type -> Type -> Bool
ty_eq = error "not implemented yet"

instance ParentR Type DeclSymbol String where
    get_child_map (FloatType dsmap _) = dsmap
    get_child_map (IntType dsmap _ _) = dsmap
    get_child_map (CharType dsmap) = dsmap
    get_child_map (BoolType dsmap) = dsmap
    get_child_map (FunctionType dsmap _ _) = dsmap
    get_child_map (VoidType dsmap) = dsmap
    get_child_map (PointerType dsmap _ _) = dsmap
