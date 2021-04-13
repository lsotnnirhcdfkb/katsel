{-# LANGUAGE TemplateHaskell #-}

module Message.PrettyPrintTH where

import Language.Haskell.TH
import Control.Applicative(liftA2)

makePrintVariants :: String -> Q [Dec]
makePrintVariants name =
    let nonlocState = mkName $ "pprint" ++ name ++ "S"
        nonlocFun = mkName $ "pprint" ++ name
        locFun = mkName $ "pprintL" ++ name
    in
        reify nonlocState >>= \ origFunInfo ->
        let astTy = return $ case origFunInfo of
                VarI _ (
                        AppT (AppT ArrowT inputTy) _
                    ) _ -> inputTy
                _ -> error "failure to extract type 't' out of variable of type 't -> a'"

            locatedAstTy :: Q Type
            locatedAstTy = AppT (ConT $ mkName "Located") <$> astTy

            sigs = sequence
                    [ SigD nonlocFun <$> [t| $(astTy) -> String |]
                    , SigD locFun <$> [t| $(locatedAstTy) -> String |]
                    ]

            decls = [d|
                    $(varP nonlocFun) = stateToFun $(varE nonlocState)
                    $(varP locFun) = $(varE nonlocFun) . unlocate
                |]

        in liftA2 (++) sigs decls

