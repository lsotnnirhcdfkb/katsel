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

        include_section =
            "// includes:\n" ++
            concatMap (("#include "++) . (++"\n"))
                [ "<stdint.h>"
                ] ++
            "\n"

        (dsdecl_section, dsdef_section, vdecl_section, vdef_section) =
            ( add_comments "declaration" "declsymbol" dses Mangle.mangle_dsid ds_decls
            , add_comments "definition" "declsymbol" dses Mangle.mangle_dsid ds_defs
            , add_comments "declaration" "value" vals Mangle.mangle_vid v_decls
            , add_comments "definition" "value" vals Mangle.mangle_vid v_defs
            )
            where
                apply_lower_fun mangle_fun lowering_fun (item_path, item) = lowering_fun irctx (mangle_fun item_path) item

                (ds_decls, ds_defs) = unzip $ map (apply_lower_fun Mangle.mangle_dsid lower_ds) dses
                (v_decls, v_defs) = unzip $ map (apply_lower_fun Mangle.mangle_vid lower_v) vals

                add_comments action thing_type thing_list mangle_fun lowered =
                    group_comment action thing_type ++ concat (zipWith add_single_comment thing_list lowered)
                    where
                        add_single_comment (thing_path, _) =
                            (single_comment action thing_type thing_path (Mangle.mangled_str $ mangle_fun thing_path) ++) . (++"\n")

                single_comment action thing_type path mangled_name = "// " ++ action ++ " of " ++ thing_type ++ " '" ++ show path ++ "' mangled as '" ++ mangled_name ++ "':\n"
                group_comment action thing_type = "// " ++ action ++ "s of " ++ thing_type ++ "s\n\n"

        (ddecl_irctx_section, ddef_irctx_section, vdecl_irctx_section, vdef_irctx_section) =
            let (a, b) = dlower_irctx irctx
                (c, d) = vlower_irctx irctx
            in ( "// declaration declaration of irctx:\n" ++ a ++ "\n"
               , "// declaration definition of irctx:\n" ++ b ++ "\n"
               , "// value declaration of irctx:\n" ++ c ++ "\n"
               , "// value definition of irctx:\n" ++ d ++ "\n"
               )

    in concat
        [ include_section

        , dsdecl_section
        , ddecl_irctx_section

        , dsdef_section
        , ddef_irctx_section

        , vdecl_section
        , vdecl_irctx_section

        , vdef_section
        , vdef_irctx_section
        ]

type LoweringFun entity = IR.IRCtx -> Mangle.MangledName -> entity -> (String, String)

not_necessary :: String
not_necessary = "// not necessary\n"
not_necessary' :: (String, String)
not_necessary' = (not_necessary, not_necessary)

-- helpers {{{1
filter_units_from_params :: IR.IRCtx -> [InternerIdx IR.Type] -> [InternerIdx IR.Type]
filter_units_from_params irctx =
    let is_unit = IR.apply_to_tyidx
            (\case
                IR.UnitType _ -> True
                _ -> False
            ) irctx
    in filter (not . is_unit)
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
        (basic_type, declarator) = add_to_declarator False irctx name_declarator ty

add_to_declarator' :: Bool -> IR.IRCtx -> CDeclarator -> InternerIdx IR.Type -> (String, CDeclarator)
add_to_declarator' uav irctx parent = IR.apply_to_tyidx (add_to_declarator uav irctx parent) irctx

add_to_declarator :: Bool -> IR.IRCtx -> CDeclarator -> IR.Type -> (String, CDeclarator)
-- the given declarator is a value that is the same time as the type passed in
-- this function will add to the declarator the operation that can be performed on the type, and also return the basic type
add_to_declarator _ _ parent (IR.FloatType _ 32) = ("float", parent)
add_to_declarator _ _ parent (IR.FloatType _ 64) = ("double", parent)
add_to_declarator _ _ _ (IR.FloatType _ _) = error "cannot convert invalid float type to c declaration"
add_to_declarator _ _ parent (IR.IntType _ size signedness) =
    let signedness_str = case signedness of
            IR.Signed -> ""
            IR.Unsigned -> "u"
    in (signedness_str ++ "int" ++ show size ++ "_t", parent)

add_to_declarator unit_as_void _ parent (IR.UnitType _)
    | unit_as_void = ("void", parent)
    | otherwise = error "cannot lower unit type to c"

add_to_declarator _ _ parent (IR.CharType _) = ("uint8_t", parent) -- TODO: this maybe should not be 8 bits
add_to_declarator _ _ parent (IR.BoolType _) = ("int", parent)

add_to_declarator _ irctx parent (IR.FunctionPointerType _ ret params) =
    let params' = filter_units_from_params irctx params
        parent' = FunctionDeclarator (PointerDeclarator parent) (map (\ pty -> type_to_cdecl' irctx pty Nothing) params')
    in add_to_declarator' True irctx parent' ret

-- convert parent (IR.PointerType _ pointee) =
    -- let parent' = PointerDeclarator parent
    -- in convert' parent' pointee
-- declsymbols {{{1
lower_ds :: LoweringFun IR.DeclSymbol
lower_ds irctx mname = IR.apply_to_ds (error "cannot lower module in c backend") (lower_tyidx irctx mname)

lower_tyidx :: LoweringFun (InternerIdx IR.Type)
lower_tyidx irctx mname = IR.apply_to_tyidx (lower_ty irctx mname) irctx

lower_ty :: LoweringFun IR.Type
lower_ty _ _ (IR.UnitType _) = not_necessary'
lower_ty irctx mname ty =
    ( "typedef " ++ str_cdecl (type_to_cdecl irctx ty (Just mname)) ++ ";\n"
    , case ty of
        IR.FloatType _ _ -> not_necessary
        IR.IntType _ _ _ -> not_necessary
        IR.CharType _ -> not_necessary
        IR.BoolType _ -> not_necessary
        IR.FunctionPointerType _ _ _ -> not_necessary
        IR.UnitType _ -> not_necessary
    )
-- declsymbol phase of irctx {{{1
dlower_irctx :: IR.IRCtx -> (String, String)
dlower_irctx _ = not_necessary'
-- values {{{1
lower_v :: LoweringFun IR.Value
lower_v irctx mname = IR.apply_to_v (lower_fun_ptr irctx mname)

lower_fun_ptr :: LoweringFun IR.FunctionPointer
lower_fun_ptr irctx mname fptr =
    let ty = IR.type_of irctx fptr
        cdecl_strd = str_cdecl (type_to_cdecl' irctx ty (Just mname))
    in ( cdecl_strd ++ ";\n"
       , cdecl_strd ++ " = &" ++ Mangle.mangled_str (Mangle.mangle_fun (IR.get_function_idx fptr)) ++ ";\n"
       )
-- value phase of irctx {{{1
vlower_irctx :: IR.IRCtx -> (String, String)
vlower_irctx irctx =
    let function_interner = IR.get_function_interner irctx

        function_idxs = all_interner_idxs function_interner
        functions = all_interner_items function_interner

        (function_decls, function_defs) = unzip $ zipWith (lower_fun irctx . Mangle.mangle_fun) function_idxs functions

    in ( "// declaration of functions:\n" ++ concat function_decls
       , "// definition of functions:\n" ++ concat function_defs
       )

-- functions {{{1
lower_fun :: LoweringFun IR.Function
lower_fun irctx mname fun =
    let ret_ty = IR.get_ret_type fun
        param_tys = filter_units_from_params irctx $ IR.get_param_types fun

        (fun_basic_type, fun_declarator) = add_to_declarator' True irctx (FunctionDeclarator (IdentifierDeclarator mname) (map (\ tyidx -> type_to_cdecl' irctx tyidx Nothing) param_tys)) ret_ty

        cdecl_strd = str_cdecl (CDecl fun_basic_type fun_declarator)
    in ( cdecl_strd ++ ";\n"
       , cdecl_strd ++ " {\n    #error not implemented yet\n}\n"
       )
