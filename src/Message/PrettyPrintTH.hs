{-# LANGUAGE TemplateHaskell #-}

module Message.PrettyPrintTH where

import Language.Haskell.TH
import Control.Applicative (liftA2)

make_print_variants :: String -> Q [Dec]
make_print_variants name =
    let non_loc_state = mkName $ "pprint_" ++ name ++ "_s"
        non_loc_fun = mkName $ "pprint_" ++ name
        loc_fun = mkName $ "pprint_l" ++ name
    in
        reify non_loc_state >>= \ orig_fun_info ->
        let ast_ty = return $ case orig_fun_info of
                VarI _ (
                        AppT (AppT ArrowT input_ty) _
                    ) _ -> input_ty
                _ -> error "failure to extract type 't' out of variable of type 't -> a'"

            located_ast_ty :: Q Type
            located_ast_ty = AppT (ConT $ mkName "Located") <$> ast_ty

            sigs = sequence
                    [ SigD non_loc_fun <$> [t| $ast_ty -> String |]
                    , SigD loc_fun <$> [t| $located_ast_ty -> String |]
                    ]

            decls = [d|
                    $(varP non_loc_fun) = state_to_fun $(varE non_loc_state)
                    $(varP loc_fun) = $(varE non_loc_fun) . unlocate
                |]

        in liftA2 (++) sigs decls

