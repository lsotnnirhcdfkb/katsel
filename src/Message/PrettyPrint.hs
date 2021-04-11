{-# LANGUAGE TemplateHaskell #-}

module Message.PrettyPrint
    ( pprintLMod
    , pprintMod
    , pprintLDecl
    , pprintDecl
    , pprintLImplMember
    , pprintImplMember
    , pprintLStmt
    , pprintStmt
    , pprintLExpr
    , pprintExpr
    , pprintLParam
    , pprintParam
    , pprintLType
    , pprintType
    , pprintLPath
    , pprintPath
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

pprintModS :: AST.DModule -> State PPCtx String
pprintModS = undefined
pprintDeclS :: AST.DDecl -> State PPCtx String
pprintDeclS = undefined
pprintImplMemberS :: AST.DImplMember -> State PPCtx String
pprintImplMemberS = undefined
pprintStmtS :: AST.DStmt -> State PPCtx String
pprintStmtS = undefined
pprintExprS :: AST.DExpr -> State PPCtx String
pprintExprS = undefined
pprintParamS :: AST.DParam -> State PPCtx String
pprintParamS = undefined
pprintTypeS :: AST.DType -> State PPCtx String
pprintTypeS = undefined
pprintPathS :: AST.DPath -> State PPCtx String
pprintPathS = undefined

$(makePrintVariants "Mod")
$(makePrintVariants "Decl")
$(makePrintVariants "ImplMember")
$(makePrintVariants "Stmt")
$(makePrintVariants "Expr")
$(makePrintVariants "Param")
$(makePrintVariants "Type")
$(makePrintVariants "Path")
