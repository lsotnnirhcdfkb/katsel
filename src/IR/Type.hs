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

ty_eq :: Type -> Type -> Bool

ty_eq (FloatType _ size_a) (FloatType _ size_b)
    | size_a == size_b = True

ty_eq (IntType _ size_a sign_a) (IntType _ size_b sign_b)
    | size_a == size_b && sign_b == sign_b = True

ty_eq (FunctionType _ ret_a params_a) (FunctionType _ ret_b params_b)
    | ret_a == ret_b && params_a == params_b = True

ty_eq (PointerType _ muty_a pointee_a) (PointerType _ muty_b pointee_b)
    | muty_a == muty_b && pointee_a == pointee_b = True

ty_eq (CharType _) (CharType _) = True
ty_eq (BoolType _) (BoolType _) = True
ty_eq (VoidType _) (VoidType _) = True

ty_eq _ _ = False

instance ParentR Type DeclSymbol String where
    get_child_map (FloatType dsmap _) = dsmap
    get_child_map (IntType dsmap _ _) = dsmap
    get_child_map (CharType dsmap) = dsmap
    get_child_map (BoolType dsmap) = dsmap
    get_child_map (FunctionType dsmap _ _) = dsmap
    get_child_map (VoidType dsmap) = dsmap
    get_child_map (PointerType dsmap _ _) = dsmap
