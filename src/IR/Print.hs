{-# LANGUAGE FlexibleContexts #-}

module IR.Print
    ( print_mod
    ) where

import IR.Parent
import IR.Typed

import IR.DeclSymbol
import IR.Value

import IR.Module
import IR.Type

import IR.Function
import IR.ConstFunctionPointer

import IR.IRCtx

import Interner

import qualified Data.Map as Map
import Data.List(intercalate)

-- TODO: print irctx

print_mod :: IRCtx -> Module -> String
print_mod irctx = (print_irctx irctx ++) . print_ds irctx . DeclSymbol

-- irctx {{{1
print_irctx :: IRCtx -> String
print_irctx irctx =
    concat
        [ "irctx {\n"
        , indent 4 (print_interner "function" (print_fun irctx) function_interner)
        , "}\n"
        ]

    where
        function_interner = get_function_interner irctx

print_interner :: String -> (a -> String) -> Interner a -> String
print_interner interner_label str_fun interner =
    interner_label ++ " interner {\n" ++
    indent 4 (unlines $ zipWith str_with_idx idxs items) ++
    "}\n"
    where
        str_with_idx idx item = "[" ++ show idx ++ "]" ++ " " ++ str_fun item

        idxs = all_interner_idxs interner
        items = all_interner_items interner
-- declsymbols {{{1
print_ds :: IRCtx -> DeclSymbol -> String
print_ds irctx ds = apply_to_ds (print_mod' irctx) (print_tyidx irctx) ds ++ " " ++ body_with_braces
    where
        body_with_braces =
            if null body
                then "{}\n"
                else "{\n" ++ body ++ "}\n"
        body = indent 4 $
            concatMap ((++"\n") . print_ds_binding irctx) (Map.toAscList $ get_child_map (ds, irctx)) ++
            concatMap ((++"\n") . print_v_binding irctx) (Map.toAscList $ get_child_map (ds, irctx))

print_ds_binding :: IRCtx -> (String, DeclSymbol) -> String
print_ds_binding irctx (name, ds) = "[decl] " ++ name ++ " = " ++ print_ds irctx ds

print_v_binding :: IRCtx -> (String, Value) -> String
print_v_binding irctx (name, v) =
    let v_str = print_v irctx v
        num_lines = length $ lines v_str
    in "[value] " ++ name ++ ": " ++ stringify_tyidx irctx (type_of irctx v) ++ " =" ++ (
            if num_lines > 1
                then "\n" ++ indent 4 v_str
                else " " ++ v_str ++ "\n"
        )

print_mod' :: IRCtx -> Module -> String
print_mod' _ _ = "mod"

print_tyidx :: IRCtx -> InternerIdx Type -> String
print_tyidx irctx = apply_to_tyidx (print_ty irctx) irctx
-- values {{{1
print_v :: IRCtx -> Value -> String
print_v irctx = apply_to_v (print_fun_ptr irctx)

print_fun_ptr :: IRCtx -> ConstFunctionPointer -> String
print_fun_ptr _ fptr = "(address of function at interner index " ++ show (get_function_idx fptr) ++ ")"
-- types {{{1
print_ty :: IRCtx -> Type -> String
print_ty _ (FloatType _ size) = "primitive float type " ++ show size
print_ty _ (IntType _ size signedness) = "primitive " ++ signedness_str ++ " int type " ++ show size
    where
        signedness_str = case signedness of
            Unsigned -> "unsigned"
            Signed -> "signed"
print_ty _ (CharType _) = "primitive char type"
print_ty _ (BoolType _) = "primitive bool type"
print_ty irctx (FunctionPointerType _ ret_ty params) = "function pointer type fun(" ++ intercalate ", " (map (stringify_tyidx irctx) params) ++ "): " ++ stringify_tyidx irctx ret_ty
print_ty _ (UnitType _) = "primitive unit type"
-- helpers {{{1
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
