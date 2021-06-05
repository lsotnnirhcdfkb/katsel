module CBackend
    ( lower_mod_to_c
    ) where

import qualified IR

import qualified Data.Map as Map

lower_mod_to_c :: IR.IRCtx -> IR.Module -> String
lower_mod_to_c irctx root =
    let v_child_list = Map.toAscList $ IR.get_child_map (root, irctx)
        ds_child_list = Map.toAscList $ IR.get_child_map (root, irctx)

        desc_wrapper action thing_type fun (name, thing) = "// " ++ action ++ " of " ++ thing_type ++ " '" ++ name ++ "':\n" ++ fun name thing ++ "\n"
    in
    "// declarations of declsymbols\n\n" ++
    concatMap (desc_wrapper "declaration" "declsymbol" (decl_ds irctx)) ds_child_list ++
    "\n// definitions of declsymbols\n\n" ++
    concatMap (desc_wrapper "definition" "declsymbol" (def_ds irctx)) ds_child_list ++
    "\n// declarations of values\n\n" ++
    concatMap (desc_wrapper "declaration" "value" (decl_v irctx)) v_child_list ++
    "\n// definitions of values\n\n" ++
    concatMap (desc_wrapper "definition" "value" (def_v irctx)) v_child_list

-- decl_ds {{{1
decl_ds :: IR.IRCtx -> String -> IR.DeclSymbol -> String
decl_ds irctx name = IR.apply_to_ds (decl_mod irctx name) (decl_tyidx irctx name)

decl_mod :: IR.IRCtx -> String -> IR.Module -> String
decl_mod _ _ _ = "// declaration of module\n"

decl_tyidx :: IR.IRCtx -> String -> IR.TyIdx -> String
decl_tyidx irctx name = IR.apply_to_tyidx (decl_ty irctx name) irctx

decl_ty :: IR.IRCtx -> String -> IR.Type -> String
decl_ty _ _ _ = "#error declaration of type currently unsupported\n"
-- def_ds {{{1
def_ds :: IR.IRCtx -> String -> IR.DeclSymbol -> String
def_ds irctx name = IR.apply_to_ds (def_mod irctx name) (def_tyidx irctx name)

def_mod :: IR.IRCtx -> String -> IR.Module -> String
def_mod _ _ _ = "// definition of module\n"

def_tyidx :: IR.IRCtx -> String -> IR.TyIdx -> String
def_tyidx irctx name = IR.apply_to_tyidx (def_ty irctx name) irctx

def_ty :: IR.IRCtx -> String -> IR.Type -> String
def_ty _ _ _ = "#error definition of type currently unsupported\n"
-- decl_v {{{1
decl_v :: IR.IRCtx -> String -> IR.Value -> String
decl_v irctx name = IR.apply_to_v (decl_fun irctx name)

decl_fun :: IR.IRCtx -> String -> IR.Function -> String
decl_fun _ _ _ = "#error declaration of function currently unsupported\n"
-- def_v {{{1
def_v :: IR.IRCtx -> String -> IR.Value -> String
def_v irctx name = IR.apply_to_v (def_fun irctx name)

def_fun :: IR.IRCtx -> String -> IR.Function -> String
def_fun _ _ _ = "#error declaration of function currently unsupported\n"
