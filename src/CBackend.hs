module CBackend
    ( lower_mod_to_c
    ) where

import qualified IR
import qualified Mangle

lower_mod_to_c :: IR.IRCtx -> IR.Module -> String
lower_mod_to_c irctx root =
    let (dses, vals) = IR.all_entities_in_mod irctx root

        desc_wrapper action thing_type fun mangle_fun (path, thing) = "// " ++ action ++ " of " ++ thing_type ++ " '" ++ show path ++ "' mangled as '" ++ Mangle.mangled_str (mangle_fun path) ++ "':\n" ++ fun irctx path thing ++ "\n"
        section action thing_type fun mangle_fun child_list =
            "// " ++ action ++ "s of " ++ thing_type ++ "s\n\n" ++
            concatMap (desc_wrapper action thing_type fun mangle_fun) child_list
    in concat
        [ section "declaration" "declsymbol" decl_ds Mangle.mangle_dsid dses
        , section "definition" "declsymbol" def_ds Mangle.mangle_dsid dses
        , section "declaration" "value" decl_v Mangle.mangle_vid vals
        , section "definition" "value" def_v Mangle.mangle_vid vals
        ]

-- decl_ds {{{1
decl_ds :: IR.IRCtx -> IR.DSIRId IR.DeclSymbol -> IR.DeclSymbol -> String
decl_ds irctx name = IR.apply_to_ds (error "cannot declare module in c backend") (decl_tyidx irctx name)

decl_tyidx :: IR.IRCtx -> IR.DSIRId IR.DeclSymbol -> IR.TyIdx -> String
decl_tyidx irctx name = IR.apply_to_tyidx (decl_ty irctx name) irctx

decl_ty :: IR.IRCtx -> IR.DSIRId IR.DeclSymbol -> IR.Type -> String
decl_ty _ _ _ = "#error declaration of type currently unsupported\n"
-- def_ds {{{1
def_ds :: IR.IRCtx -> IR.DSIRId IR.DeclSymbol -> IR.DeclSymbol -> String
def_ds irctx name = IR.apply_to_ds (error "cannot define module in c backend") (def_tyidx irctx name)

def_tyidx :: IR.IRCtx -> IR.DSIRId IR.DeclSymbol -> IR.TyIdx -> String
def_tyidx irctx name = IR.apply_to_tyidx (def_ty irctx name) irctx

def_ty :: IR.IRCtx -> IR.DSIRId IR.DeclSymbol -> IR.Type -> String
def_ty _ _ _ = "#error definition of type currently unsupported\n"
-- decl_v {{{1
decl_v :: IR.IRCtx -> IR.VIRId IR.Value -> IR.Value -> String
decl_v irctx name = IR.apply_to_v (decl_fun irctx name)

decl_fun :: IR.IRCtx -> IR.VIRId IR.Value -> IR.Function -> String
decl_fun _ _ _ = "#error declaration of function currently unsupported\n"
-- def_v {{{1
def_v :: IR.IRCtx -> IR.VIRId IR.Value -> IR.Value -> String
def_v irctx name = IR.apply_to_v (def_fun irctx name)

def_fun :: IR.IRCtx -> IR.VIRId IR.Value -> IR.Function -> String
def_fun _ _ _ = "#error declaration of function currently unsupported\n"
