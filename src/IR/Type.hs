{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Type
    ( Signedness(..)
    , FloatSize(..)
    , IntSize(..)
    , Type(..)

    , stringify_ty
    , match_signedness
    ) where

import IR.DeclSpan
import IR.DeclSymbol

import IR.IRCtx

import Data.List (intercalate)

data Signedness = Signed | Unsigned
data FloatSize = F32 | F64
data IntSize = I8 | I16 | I32 | I64
data Type
    = FloatType FloatSize
    | IntType IntSize Signedness
    | CharType
    | BoolType
    | UnitType
    | FunctionPointerType (DSIdx Type) [DSIdx Type]

instance Show FloatSize where
    show F32 = "32"
    show F64 = "64"

instance Show IntSize where
    show I8 = "8"
    show I16 = "16"
    show I32 = "32"
    show I64 = "64"

instance DeclSpan IRCtx Type where
    decl_span _ (FloatType _) = Nothing
    decl_span _ (IntType _ _) = Nothing
    decl_span _ CharType = Nothing
    decl_span _ BoolType = Nothing
    decl_span _ UnitType = Nothing
    decl_span _ (FunctionPointerType _ _) = Nothing

instance IsDeclSymbol IRCtx Type

match_signedness :: a -> a -> Signedness -> a
match_signedness s u sgn =
    case sgn of
        Signed -> s
        Unsigned -> u

stringify_ty :: IRCtx -> Type -> String

stringify_ty _ (FloatType F32) = "float"
stringify_ty _ (FloatType F64) = "double"

stringify_ty _ (IntType size signedness) = match_signedness "s" "u" signedness ++ "int" ++ show size

stringify_ty _ CharType = "char"
stringify_ty _ BoolType = "bool"
stringify_ty _ UnitType = "unit"

stringify_ty irctx (FunctionPointerType retty params) =
    let ret_str = stringify_ty irctx (resolve_dsidx retty irctx)
        param_strs = map (stringify_ty irctx . flip resolve_dsidx irctx) params
    in "fun(" ++ intercalate ", " param_strs ++ "): " ++ ret_str
