module CBackend
    ( lower_mod_to_c
    ) where

import qualified IR

import qualified Data.Map as Map

lower_mod_to_c :: IR.IRCtx -> IR.Module -> String
lower_mod_to_c irctx root =
    let v_child_list = Map.toList $ IR.get_child_map (root, irctx)
        ds_child_list = Map.toList $ IR.get_child_map (root, irctx)
    in
    "// declarations of declsymbols\n" ++
    concatMap (uncurry $ decl_ds irctx) ds_child_list ++
    "\n// definitions of declsymbols\n" ++
    concatMap (uncurry $ def_ds irctx) ds_child_list ++
    "\n// declarations of values\n" ++
    concatMap (uncurry $ decl_v irctx) v_child_list ++
    "\n// definitions of values\n" ++
    concatMap (uncurry $ def_v irctx) v_child_list

-- decl_ds {{{1
decl_ds :: IR.IRCtx -> String -> IR.DeclSymbol -> String
decl_ds irctx name = IR.apply_to_ds (decl_mod irctx name) (decl_tyidx irctx name)

decl_mod irctx name mod' = "// declaration of module\n"

decl_tyidx irctx name = IR.apply_to_tyidx (decl_ty irctx name) irctx

decl_ty irctx name ty = "// declaration of type\n#error declaration of type currently unsupported\n"
-- def_ds {{{1
def_ds :: IR.IRCtx -> String -> IR.DeclSymbol -> String
def_ds irctx name = IR.apply_to_ds (def_mod irctx name) (def_tyidx irctx name)

def_mod irctx name mod' = "// definition of module\n"

def_tyidx irctx name = IR.apply_to_tyidx (def_ty irctx name) irctx

def_ty irctx name ty = "// definition of type\n#error definition of type currently unsupported\n"
-- decl_v {{{1
decl_v :: IR.IRCtx -> String -> IR.Value -> String
decl_v irctx name = IR.apply_to_v (decl_fun irctx name)

decl_fun irctx name fun = "// declaration of function\n#error declaration of function currently unsupported\n"
-- def_v {{{1
def_v :: IR.IRCtx -> String -> IR.Value -> String
def_v irctx name = IR.apply_to_v (def_fun irctx name)

def_fun irctx name fun = "// definition of function\n#error declaration of function currently unsupported\n"
