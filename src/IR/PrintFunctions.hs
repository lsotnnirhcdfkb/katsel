{-# LANGUAGE FlexibleContexts #-}

module IR.PrintFunctions
    ( print_mod
    ) where

import IR.Module
import IR.PrintClasses
import IR.IRCtx
import IR.Parent
import IR.DeclSymbol
import IR.Value
import IR.Typed
import IR.Type

import qualified Data.Map as Map

print_mod :: IRCtx -> Module -> String
print_mod = print_ds

print_ds :: (Parent d DeclSymbol String, Parent d Value String, DSPrint d) => IRCtx -> d -> String
print_ds irctx d = ds_print irctx d ++ " " ++ body_with_braces
    where
        body_with_braces =
            if null body
                then "{}\n"
                else "{\n" ++ body ++ "}\n"
        body = indent 4 $
            (concatMap ((++"\n") . print_ds_binding irctx) (Map.toAscList $ get_child_map (d, irctx))) ++
            (concatMap ((++"\n") . print_v_binding irctx) (Map.toAscList $ get_child_map (d, irctx)))

print_ds_binding :: IRCtx -> (String, DeclSymbol) -> String
print_ds_binding irctx (name, ds) =
    "[decl] " ++ name ++ " = " ++ print_ds irctx ds

print_v_binding :: IRCtx -> (String, Value) -> String
print_v_binding irctx (name, v) =
    let v_str = v_print irctx v
        num_lines = length $ lines v_str
    in "[value] " ++ name ++ ": " ++ stringify_tyidx irctx (type_of irctx v) ++ " =" ++ (
            if num_lines > 1
                then "\n" ++ indent 4 v_str
                else " " ++ v_str
        ) ++ (
            if not (null v_str) && last v_str /= '\n'
                then "\n"
                else ""
        )

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines
