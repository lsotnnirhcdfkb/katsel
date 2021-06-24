module CBackend
    ( lower_mod_to_c
    ) where

import qualified IR
import qualified Mangle

import Interner

lower_mod_to_c :: IR.IRCtx -> IR.Module -> String
lower_mod_to_c irctx root =
    let (dses, vals) = IR.all_entities_in_mod irctx root

        desc_wrapper action thing_type fun mangle_fun (path, thing) =
            let mangled = mangle_fun path
            in "// " ++ action ++ " of " ++ thing_type ++ " '" ++ show path ++ "' mangled as '" ++ Mangle.mangled_str mangled ++ "':\n" ++ fun irctx path mangled thing ++ "\n"
        section action thing_type fun mangle_fun child_list =
            "// " ++ action ++ "s of " ++ thing_type ++ "s\n\n" ++
            concatMap (desc_wrapper action thing_type fun mangle_fun) child_list
    in concat
        [ section "declaration" "declsymbol" decl_ds Mangle.mangle_dsid dses
        , section "definition" "declsymbol" def_ds Mangle.mangle_dsid dses
        , section "declaration" "value" decl_v Mangle.mangle_vid vals
        , section "definition" "value" def_v Mangle.mangle_vid vals
        ]

type LoweringFun id entity = IR.IRCtx -> id -> Mangle.MangledName -> entity -> String

print_not_impl_fun :: String -> String -> LoweringFun id entity
print_not_impl_fun action thing _ _ _ _ = "#error " ++ action ++ " of " ++ thing ++ " currently unsupported\n"

-- decl_ds {{{1
decl_ds :: LoweringFun (IR.DSIRId IR.DeclSymbol) IR.DeclSymbol
decl_ds irctx path mname = IR.apply_to_ds (error "cannot declare module in c backend") (decl_tyidx irctx path mname)

decl_tyidx :: LoweringFun (IR.DSIRId IR.DeclSymbol) (InternerIdx IR.Type)
decl_tyidx irctx path mname = IR.apply_to_tyidx (decl_ty irctx path mname) irctx

decl_ty :: LoweringFun (IR.DSIRId IR.DeclSymbol) (IR.Type)
decl_ty _ _ mname (IR.FloatType _ 32) = concat ["typedef float ", Mangle.mangled_str mname, ";\n"]
decl_ty _ _ mname (IR.FloatType _ 64) = concat ["typedef double ", Mangle.mangled_str mname, ";\n"]
decl_ty _ _ _ (IR.FloatType _ size) = error $ "cannot lower illegal float point type (must be 32 or 64 bits wide, but got " ++ show size ++ " bits"

decl_ty _ _ mname (IR.IntType _ size signedness) =
    let signedness_str = case signedness of
            IR.Signed -> "s"
            IR.Unsigned -> "u"
    in concat ["typedef ", signedness_str, "int", show size, "_t ", Mangle.mangled_str mname, ";\n"]

decl_ty _ _ _ IR.GenericFloatType = error "cannot declare generic float type"
decl_ty _ _ _ IR.GenericIntType = error "cannot declare generic int type"
decl_ty _ _ _ (IR.UnitType _) = "// cannot declare unit type\n"

decl_ty _ _ mname (IR.CharType _) = concat ["typedef char ", Mangle.mangled_str mname, ";\n"]
decl_ty _ _ mname (IR.BoolType _) = concat ["typedef bool ", Mangle.mangled_str mname, ";\n"]

-- TODO: implement a way to properly print declarators for any type
decl_ty _ _ _ (IR.FunctionPointerType _ _ _) = "// not implemented yet\n" -- TODO
decl_ty _ _ _ (IR.PointerType _ _ _) = "// not implemented yet\n" -- TODO
-- def_ds {{{1
def_ds :: LoweringFun (IR.DSIRId IR.DeclSymbol) (IR.DeclSymbol)
def_ds irctx path mname = IR.apply_to_ds (error "cannot define module in c backend") (def_tyidx irctx path mname)

def_tyidx :: LoweringFun (IR.DSIRId IR.DeclSymbol) (InternerIdx IR.Type)
def_tyidx irctx path mname = IR.apply_to_tyidx (def_ty irctx path mname) irctx

def_ty :: LoweringFun (IR.DSIRId IR.DeclSymbol) (IR.Type)
def_ty _ _ _ (IR.FloatType _ _) = "// float type does not need definition\n"
def_ty _ _ _ (IR.IntType _ _ _) = "// int type does not need definition\n"
def_ty _ _ _ IR.GenericFloatType = error "cannot define generic float type"
def_ty _ _ _ IR.GenericIntType = error "cannot define generic int type"
def_ty _ _ _ (IR.CharType _) = "// char type does not need definition\n"
def_ty _ _ _ (IR.BoolType _) = "// bool type does not need definition\n"
def_ty _ _ _ (IR.FunctionPointerType _ _ _) = "// function type does not need definition\n"
def_ty _ _ _ (IR.UnitType _) = "// unit type does not need definition\n"
def_ty _ _ _ (IR.PointerType _ _ _) = "// pointer type does not need definition\n"
-- decl_v {{{1
decl_v :: LoweringFun (IR.VIRId IR.Value) (IR.Value)
decl_v irctx path mname = IR.apply_to_v (decl_fun_ptr irctx path mname)

decl_fun_ptr :: LoweringFun (IR.VIRId IR.Value) (IR.FunctionPointer)
decl_fun_ptr = print_not_impl_fun "declaration" "const function pointer" -- TODO

decl_fun :: LoweringFun (IR.VIRId IR.Value) (IR.Function)
decl_fun = print_not_impl_fun "declaration" "function" -- TODO
-- def_v {{{1
def_v :: LoweringFun (IR.VIRId IR.Value) (IR.Value)
def_v irctx path mname = IR.apply_to_v (def_fun_ptr irctx path mname)

def_fun_ptr :: LoweringFun (IR.VIRId IR.Value) (IR.FunctionPointer)
def_fun_ptr = print_not_impl_fun "definition" "const function pointer" -- TODO

def_fun :: LoweringFun (IR.VIRId IR.Value) (IR.Function)
def_fun = print_not_impl_fun "declaration" "function" -- TODO
