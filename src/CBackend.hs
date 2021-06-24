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
str_cdecl (CDecl res declarator) = res ++ " " ++ str_declarator declarator
    where
        str_declarator' d = "(" ++ str_declarator d ++ ")"

        str_declarator (IdentifierDeclarator mn) = Mangle.mangled_str mn
        str_declarator AbstractDeclarator = ""
        str_declarator (PointerDeclarator decl) = "*" ++ str_declarator' decl
        str_declarator (ArrayDeclarator decl size) = str_declarator' decl ++ "[" ++ show size ++ "]"
        str_declarator (FunctionDeclarator decl params) = str_declarator' decl ++ "(" ++ intercalate ", " (map str_cdecl params) ++ ")"
-- converting types to c declarations {{{1
type_to_cdecl :: IR.IRCtx -> IR.Type -> Maybe Mangle.MangledName -> CDecl
type_to_cdecl irctx ty name = CDecl basic_type declarator
    where
        name_declarator = maybe AbstractDeclarator IdentifierDeclarator name
        (basic_type, declarator) = convert name_declarator ty

        convert' :: CDeclarator -> InternerIdx IR.Type -> (String, CDeclarator)
        convert' parent idx = IR.apply_to_tyidx (convert parent) irctx idx

        convert :: CDeclarator -> IR.Type -> (String, CDeclarator)
        -- the given declarator is a value that is the same time as the type passed in
        -- this function will add to the declarator the operation that can be performed on the type, and also return the basic type
        convert parent (IR.FloatType _ 32) = ("float", parent)
        convert parent (IR.FloatType _ 64) = ("double", parent)
        convert _ (IR.FloatType _ _) = error "cannot convert invalid float type to c declaration"
        convert parent (IR.IntType _ size signedness) =
            let signedness_str = case signedness of
                    IR.Signed -> "s"
                    IR.Unsigned -> "u"
            in (signedness_str ++ "int" ++ show size ++ "_t", parent)

        convert _ (IR.UnitType _) = error "cannot convert unit type into c declaration"

        convert parent (IR.CharType _) = ("uint8_t", parent) -- TODO: this maybe should not be 8 bits
        convert parent (IR.BoolType _) = ("int", parent)

        convert parent (IR.FunctionPointerType _ ret params) =
            let parent' = FunctionDeclarator (PointerDeclarator parent) (map (IR.apply_to_tyidx (\ pty -> type_to_cdecl irctx pty Nothing) irctx) params)
            in convert' parent' ret

        -- convert parent (IR.PointerType _ pointee) =
            -- let parent' = PointerDeclarator parent
            -- in convert' parent' pointee
-- decl_ds {{{1
decl_ds :: LoweringFun (IR.DSIRId IR.DeclSymbol) IR.DeclSymbol
decl_ds irctx path mname = IR.apply_to_ds (error "cannot declare module in c backend") (decl_tyidx irctx path mname)

decl_tyidx :: LoweringFun (IR.DSIRId IR.DeclSymbol) (InternerIdx IR.Type)
decl_tyidx irctx path mname = IR.apply_to_tyidx (decl_ty irctx path mname) irctx

decl_ty :: LoweringFun (IR.DSIRId IR.DeclSymbol) IR.Type
decl_ty _ _ _ (IR.UnitType _) = "// cannot declare unit type\n"
decl_ty irctx _ mname ty = "typedef " ++ str_cdecl (type_to_cdecl irctx ty (Just mname)) ++ ";\n"
-- def_ds {{{1
def_ds :: LoweringFun (IR.DSIRId IR.DeclSymbol) IR.DeclSymbol
def_ds irctx path mname = IR.apply_to_ds (error "cannot define module in c backend") (def_tyidx irctx path mname)

def_tyidx :: LoweringFun (IR.DSIRId IR.DeclSymbol) (InternerIdx IR.Type)
def_tyidx irctx path mname = IR.apply_to_tyidx (def_ty irctx path mname) irctx

def_ty :: LoweringFun (IR.DSIRId IR.DeclSymbol) IR.Type
def_ty _ _ _ (IR.FloatType _ _) = print_not_necessary "definition" "float type"
def_ty _ _ _ (IR.IntType _ _ _) = print_not_necessary "definition" "int type"
def_ty _ _ _ (IR.CharType _) = print_not_necessary "definition" "char type"
def_ty _ _ _ (IR.BoolType _) = print_not_necessary "definition" "bool type"
def_ty _ _ _ (IR.FunctionPointerType _ _ _) = print_not_necessary "definition" "function type"
def_ty _ _ _ (IR.UnitType _) = print_not_necessary "definition" "unit type"
-- decl_v {{{1
decl_v :: LoweringFun (IR.VIRId IR.Value) IR.Value
decl_v irctx path mname = IR.apply_to_v (decl_fun_ptr irctx path mname)

decl_fun_ptr :: LoweringFun (IR.VIRId IR.Value) IR.FunctionPointer
decl_fun_ptr = print_not_impl_fun "declaration" "const function pointer" -- TODO

decl_fun :: LoweringFun (IR.VIRId IR.Value) IR.Function
decl_fun = print_not_impl_fun "declaration" "function" -- TODO
-- def_v {{{1
def_v :: LoweringFun (IR.VIRId IR.Value) IR.Value
def_v irctx path mname = IR.apply_to_v (def_fun_ptr irctx path mname)

def_fun_ptr :: LoweringFun (IR.VIRId IR.Value) IR.FunctionPointer
def_fun_ptr = print_not_impl_fun "definition" "const function pointer" -- TODO

def_fun :: LoweringFun (IR.VIRId IR.Value) IR.Function
def_fun = print_not_impl_fun "definition" "function" -- TODO
