{-# LANGUAGE TemplateHaskell #-}

module Message.PrettyPrintTH where

import Language.Haskell.TH
import Control.Applicative(liftA2)

makePrintVariants :: String -> Q [Dec]
makePrintVariants name =
    let nonlocState = mkName $ "pprint" ++ name ++ "S"
        locState = mkName $ "pprintL" ++ name ++ "S"
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
            stateTy = return $ ConT (mkName "State") `AppT` ConT (mkName "PPCtx") `AppT` ConT (mkName "()")

            sigs = sequence
                    [ SigD locState <$> [t| $(locatedAstTy) -> $(stateTy) |]
                    , SigD nonlocFun <$> [t| $(astTy) -> String |]
                    , SigD locFun <$> [t| $(locatedAstTy) -> String |]
                    ]

            decls = [d|
                    $(varP locState) = useWithLocated $(varE nonlocState)
                    $(varP nonlocFun) = stateToFun $(varE nonlocState)
                    $(varP locFun) = stateToFun $(varE locState)
                |]

        in liftA2 (++) sigs decls

