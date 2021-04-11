{-# LANGUAGE TemplateHaskell #-}

module Message.PrettyPrint
    ( pprintLMod
    , pprintMod
    , pprintLExpr
    , pprintExpr
    ) where

import Control.Monad.State.Lazy(State, state, evalState)

import qualified AST
import Location

import Message.PrettyPrintTH

newtype PPCtx = PPCtx Int
startCtx :: PPCtx
startCtx = PPCtx 0

useWithLocated :: (a -> State PPCtx String) -> Located a -> State PPCtx String
useWithLocated pprintfun (Located _ a) = pprintfun a
stateToFun :: (a -> State PPCtx String) -> a -> String
stateToFun statefun thing = evalState (statefun thing) startCtx

-- pprintLExpr :: Located AST.DExpr -> String
-- pprintLExpr = stateToFun pprintLExprS

-- pprintExpr :: AST.DExpr -> String
-- pprintExpr = stateToFun pprintExprS

-- pprintLExprS :: Located AST.DExpr -> State PPCtx String
-- pprintLExprS = useWithLocated pprintExprS

pprintExprS :: AST.DExpr -> State PPCtx String
pprintExprS = error "TODO"

pprintModS :: AST.DModule -> State PPCtx String
pprintModS = error "TODO"

$(makePrintVariants "Expr")
$(makePrintVariants "Mod")

