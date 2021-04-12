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

import Data.List(foldl1')

import Message.PrettyPrintTH

data PPCtx = PPCtx Int String
startCtx :: PPCtx
startCtx = PPCtx 0 ""

stateToFun :: (a -> State PPCtx ()) -> a -> String
stateToFun statefun thing =
    let (PPCtx _ res) = execState (statefun thing) startCtx
    in res

put :: String -> State PPCtx ()
put str = state $ \ (PPCtx ind acc) -> ((), PPCtx ind $ acc ++ str)
indent :: State PPCtx ()
indent = state (\ (PPCtx ind acc) -> ((), PPCtx (ind + 1) acc)) >> putnl
dedent :: State PPCtx ()
dedent = state (\ (PPCtx ind acc) -> ((), PPCtx (ind - 1) acc)) >> putnl
putnl :: State PPCtx ()
putnl = state $ \ (PPCtx ind acc) -> ((), PPCtx ind $ acc ++ "\n" ++ replicate (ind * 4) ' ')

unlocate :: Located a -> a
unlocate (Located _ a) = a

pprintList :: (a -> State PPCtx ()) -> [a] -> State PPCtx ()
pprintList pprintfun things = foldl1' (>>) $ map pprintfun things

pprintListDelim :: (a -> State PPCtx ()) -> State PPCtx () -> [a] -> State PPCtx ()
pprintListDelim pprintfun delim things = foldl1' (>>) $ map pprintfun things

pprintModS :: AST.DModule -> State PPCtx ()
pprintModS (AST.DModule' decls) = pprintList (pprintDeclS . unlocate) decls

pprintDeclS :: AST.DDecl -> State PPCtx ()
pprintDeclS (AST.DDecl'Fun sf) = pprintFunDeclS $ unlocate sf
pprintDeclS (AST.DDecl'Impl ty members) = undefined

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

pprintFunDeclS :: AST.SFunDecl -> State PPCtx ()
pprintFunDeclS (AST.SFunDecl' retty (Located _ name) params expr) =
    put "fun " >> put name >>
    put "(" >> pprintListDelim (pprintParamS . unlocate) (put ", ") params >> put ")" >>
    (pprintTypeAnnotationS . unlocate) `maybeDo` retty

pprintTypeAnnotationS :: AST.DType -> State PPCtx ()
pprintTypeAnnotationS ty = put ": " >> pprintTypeS ty

maybeDo :: (a -> State PPCtx ()) -> Maybe a -> State PPCtx ()
maybeDo st m =
    case m of
        Just x -> st x
        Nothing -> return ()

$(makePrintVariants "Mod")
$(makePrintVariants "Decl")
$(makePrintVariants "ImplMember")
$(makePrintVariants "Stmt")
$(makePrintVariants "Expr")
$(makePrintVariants "Param")
$(makePrintVariants "Type")
$(makePrintVariants "Path")
