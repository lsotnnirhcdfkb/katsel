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

import Control.Monad.State.Lazy(State, state, execState)

import qualified AST
import Location

import Message.PrettyPrintTH

data PPCtx = PPCtx Int String
startCtx :: PPCtx
startCtx = PPCtx 0 ""

useWithLocated :: (a -> State PPCtx ()) -> Located a -> State PPCtx ()
useWithLocated pprintfun (Located _ a) = pprintfun a

stateToFun :: (a -> State PPCtx ()) -> a -> String
stateToFun statefun thing =
    let (PPCtx _ res) = execState (statefun thing) startCtx
    in res

pprintModS :: AST.DModule -> State PPCtx ()
pprintModS = undefined

pprintDeclS :: AST.DDecl -> State PPCtx ()
pprintDeclS = undefined

pprintImplMemberS :: AST.DImplMember -> State PPCtx ()
pprintImplMemberS = undefined

pprintStmtS :: AST.DStmt -> State PPCtx ()
pprintStmtS = undefined

pprintExprS :: AST.DExpr -> State PPCtx ()
pprintExprS = undefined

pprintParamS :: AST.DParam -> State PPCtx ()
pprintParamS = undefined

pprintTypeS :: AST.DType -> State PPCtx ()
pprintTypeS = undefined

pprintPathS :: AST.DPath -> State PPCtx ()
pprintPathS = undefined

$(makePrintVariants "Mod")
$(makePrintVariants "Decl")
$(makePrintVariants "ImplMember")
$(makePrintVariants "Stmt")
$(makePrintVariants "Expr")
$(makePrintVariants "Param")
$(makePrintVariants "Type")
$(makePrintVariants "Path")
