module CBackend
    ( lower_mod_to_c
    ) where

import qualified IR
import qualified Mangle

import Interner

import Data.List(intercalate)

lower_mod_to_c :: IR.IRCtx -> IR.Module -> String
lower_mod_to_c irctx root =
    let (dses, vals) = IR.all_entities_in_mod irctx root

        desc_wrapper action thing_type fun mangle_fun (path, thing) =
            let mangled = mangle_fun path
            in "// " ++ action ++ " of " ++ thing_type ++ " '" ++ show path ++ "' mangled as '" ++ Mangle.mangled_str mangled ++ "':\n" ++ fun irctx mangled thing ++ "\n"
        section action thing_type fun mangle_fun child_list =
            "// " ++ action ++ "s of " ++ thing_type ++ "s\n\n" ++
            concatMap (desc_wrapper action thing_type fun mangle_fun) child_list

        include_section =
            "// includes:\n" ++
            concatMap (("#include "++) . (++"\n"))
                [ "<stdint.h>"
                ] ++
                "\n"

    in concat
        [ include_section

        , section "declaration" "declsymbol" decl_ds Mangle.mangle_dsid dses
        , "// declaration declaration of irctx\n" ++ ddecl_irctx irctx ++ "\n"

        , section "definition" "declsymbol" def_ds Mangle.mangle_dsid dses
        , "// declaration definition of irctx\n" ++ ddef_irctx irctx ++ "\n"

        , section "declaration" "value" decl_v Mangle.mangle_vid vals
        , "// value declaration of irctx:\n" ++ vdecl_irctx irctx ++ "\n"

        , section "definition" "value" def_v Mangle.mangle_vid vals
        , "// value definition of irctx:\n" ++ vdef_irctx irctx ++ "\n"
        ]

type LoweringFun entity = IR.IRCtx -> Mangle.MangledName -> entity -> String

print_not_impl_fun :: String -> String -> LoweringFun entity
print_not_impl_fun action thing _ _ _ = "#error " ++ action ++ " of " ++ thing ++ " currently unsupported\n"

print_not_necessary :: String -> String -> String
print_not_necessary action thing = "// " ++ thing ++ " does not need " ++ action ++ "\n"

-- c declarations {{{1
data CDecl = CDecl String CDeclarator
data CDeclarator
    = IdentifierDeclarator Mangle.MangledName
    | AbstractDeclarator
    | PointerDeclarator CDeclarator
    | ArrayDeclarator CDeclarator Int
    | FunctionDeclarator CDeclarator [CDecl]

str_cdecl :: CDecl -> String
str_cdecl (CDecl res declarator) =
    case str_declarator declarator of
        "" -> res
        s -> res ++ " " ++ s
    where
        str_declarator' d = "(" ++ str_declarator d ++ ")"

        str_declarator (IdentifierDeclarator mn) = Mangle.mangled_str mn
        str_declarator AbstractDeclarator = ""
        str_declarator (PointerDeclarator decl) = "*" ++ str_declarator' decl
        str_declarator (ArrayDeclarator decl size) = str_declarator' decl ++ "[" ++ show size ++ "]"
        str_declarator (FunctionDeclarator decl []) = str_declarator' decl ++ "(void)"
        str_declarator (FunctionDeclarator decl params) = str_declarator' decl ++ "(" ++ intercalate ", " (map str_cdecl params) ++ ")"
-- converting types to c declarations {{{1
type_to_cdecl' :: IR.IRCtx -> InternerIdx IR.Type -> Maybe Mangle.MangledName -> CDecl
type_to_cdecl' irctx tyidx name = IR.apply_to_tyidx (\ ty -> type_to_cdecl irctx ty name) irctx tyidx

type_to_cdecl :: IR.IRCtx -> IR.Type -> Maybe Mangle.MangledName -> CDecl
type_to_cdecl irctx ty name = CDecl basic_type declarator
    where
        name_declarator = maybe AbstractDeclarator IdentifierDeclarator name
        (basic_type, declarator) = convert False name_declarator ty

        convert' :: Bool -> CDeclarator -> InternerIdx IR.Type -> (String, CDeclarator)
        convert' uav parent idx = IR.apply_to_tyidx (convert uav parent) irctx idx

        convert :: Bool -> CDeclarator -> IR.Type -> (String, CDeclarator)
        -- the given declarator is a value that is the same time as the type passed in
        -- this function will add to the declarator the operation that can be performed on the type, and also return the basic type
        convert _ parent (IR.FloatType _ 32) = ("float", parent)
        convert _ parent (IR.FloatType _ 64) = ("double", parent)
        convert _ _ (IR.FloatType _ _) = error "cannot convert invalid float type to c declaration"
        convert _ parent (IR.IntType _ size signedness) =
            let signedness_str = case signedness of
                    IR.Signed -> ""
                    IR.Unsigned -> "u"
            in (signedness_str ++ "int" ++ show size ++ "_t", parent)

        convert unit_as_void parent (IR.UnitType _)
            | unit_as_void = ("void", parent)
            | otherwise = error "cannot lower unit type to c"

        convert _ parent (IR.CharType _) = ("uint8_t", parent) -- TODO: this maybe should not be 8 bits
        convert _ parent (IR.BoolType _) = ("int", parent)

        convert _ parent (IR.FunctionPointerType _ ret params) =
            let is_unit = IR.apply_to_tyidx
                    (\case
                        IR.UnitType _ -> True
                        _ -> False
                    ) irctx
                cparams = filter (not . is_unit) params

                parent' = FunctionDeclarator (PointerDeclarator parent) (map (IR.apply_to_tyidx (\ pty -> type_to_cdecl irctx pty Nothing) irctx) cparams)
            in convert' True parent' ret

        -- convert parent (IR.PointerType _ pointee) =
            -- let parent' = PointerDeclarator parent
            -- in convert' parent' pointee
-- decl_ds {{{1
decl_ds :: LoweringFun IR.DeclSymbol
decl_ds irctx mname = IR.apply_to_ds (error "cannot declare module in c backend") (decl_tyidx irctx mname)

decl_tyidx :: LoweringFun (InternerIdx IR.Type)
decl_tyidx irctx mname = IR.apply_to_tyidx (decl_ty irctx mname) irctx

decl_ty :: LoweringFun IR.Type
decl_ty _ _ (IR.UnitType _) = print_not_necessary "declaration" "unit type"
decl_ty irctx mname ty = "typedef " ++ str_cdecl (type_to_cdecl irctx ty (Just mname)) ++ ";\n"
-- ddecl_irctx {{{1
ddecl_irctx :: IR.IRCtx -> String
ddecl_irctx _ = ""
-- def_ds {{{1
def_ds :: LoweringFun IR.DeclSymbol
def_ds irctx mname = IR.apply_to_ds (error "cannot define module in c backend") (def_tyidx irctx mname)

def_tyidx :: LoweringFun (InternerIdx IR.Type)
def_tyidx irctx mname = IR.apply_to_tyidx (def_ty irctx mname) irctx

def_ty :: LoweringFun IR.Type
def_ty _ _ (IR.FloatType _ _) = print_not_necessary "definition" "float type"
def_ty _ _ (IR.IntType _ _ _) = print_not_necessary "definition" "int type"
def_ty _ _ (IR.CharType _) = print_not_necessary "definition" "char type"
def_ty _ _ (IR.BoolType _) = print_not_necessary "definition" "bool type"
def_ty _ _ (IR.FunctionPointerType _ _ _) = print_not_necessary "definition" "function type"
def_ty _ _ (IR.UnitType _) = print_not_necessary "definition" "unit type"
-- ddef_irctx {{{1
ddef_irctx :: IR.IRCtx -> String
ddef_irctx _ = ""
-- decl_v {{{1
decl_v :: LoweringFun IR.Value
decl_v irctx mname = IR.apply_to_v (decl_fun_ptr irctx mname)

decl_fun_ptr :: LoweringFun IR.FunctionPointer
decl_fun_ptr irctx mname fptr =
    let ty = IR.type_of irctx fptr
    in str_cdecl (type_to_cdecl' irctx ty (Just mname)) ++ ";\n"
-- vdecl_irctx {{{1
vdecl_irctx :: IR.IRCtx -> String
vdecl_irctx irctx =
    let function_interner = IR.get_function_interner irctx

        function_idxs = all_interner_idxs function_interner
        functions = all_interner_items function_interner

    in "// declaration of functions:\n" ++ concat (zipWith (\ idx -> decl_fun irctx (Mangle.mangle_fun idx)) function_idxs functions)
-- def_v {{{1
def_v :: LoweringFun IR.Value
def_v irctx mname = IR.apply_to_v (def_fun_ptr irctx mname)

def_fun_ptr :: LoweringFun IR.FunctionPointer
def_fun_ptr irctx mname fptr =
    let ty = IR.type_of irctx fptr
    in str_cdecl (type_to_cdecl' irctx ty (Just mname)) ++ " = &" ++ Mangle.mangled_str (Mangle.mangle_fun (IR.get_function_idx fptr)) ++ ";\n"
-- vdef_irctx {{{1
vdef_irctx :: IR.IRCtx -> String
vdef_irctx irctx =
    let function_interner = IR.get_function_interner irctx

        function_idxs = all_interner_idxs function_interner
        functions = all_interner_items function_interner

    in "// definition of functions:\n" ++ concat (zipWith (\ idx -> def_fun irctx (Mangle.mangle_fun idx)) function_idxs functions)
-- functions {{{1
decl_fun :: IR.IRCtx -> Mangle.MangledName -> IR.Function -> String
decl_fun = print_not_impl_fun "declaration" "function" -- TODO

def_fun :: IR.IRCtx -> Mangle.MangledName -> IR.Function -> String
def_fun = print_not_impl_fun "definition" "function" -- TODO
