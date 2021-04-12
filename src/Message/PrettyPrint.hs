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
import qualified Control.Monad.State.Lazy as State(get, put)

import qualified AST
import Location

import Data.List(foldl', intersperse)
import Data.Char(isSpace)

import Message.PrettyPrintTH

-- NOTE: NOT an automated code formatter!! just a pretty printer to print things in error messages!

-- PPCtx {{{1
data PPCtx
    = PPCtx Int String LastIndentStatus LastNLStatus

data LastIndentStatus
    = LastIndIndent
    | LastIndDedent
    | LastIndOther

data LastNLStatus
    = NLYes
    | NLNo

startCtx :: PPCtx
startCtx = PPCtx 0 "" LastIndOther NLNo
-- run pprint state helper {{{1
stateToFun :: (a -> State PPCtx ()) -> a -> String
stateToFun statefun thing =
    let (PPCtx _ res _ _) = execState (statefun thing) startCtx
    in res
-- put, putch, indent, dedent, and putnl {{{1
putch :: Char -> State PPCtx ()
putch ch = state $
    \ (PPCtx ind acc lastIndStatus lastNlStatus) ->
        let accWithIndent =
                case lastNlStatus of
                    NLYes -> acc ++ replicate (ind * 4) ' '
                    NLNo -> acc
            accWithCh = accWithIndent ++ [ch]

            nlStatus =
                if ch == '\n'
                then NLYes
                else NLNo

            indStatus =
                if isSpace ch
                then lastIndStatus
                else LastIndOther

        in ((), PPCtx ind accWithCh indStatus nlStatus)

put :: String -> State PPCtx ()
put str = foldl (>>) (return ()) $ map putch str

chgInd :: Int -> LastIndentStatus -> State PPCtx ()
chgInd chgAmt newstatus =
    State.get >>= \ (PPCtx ind acc _ lastnl) ->
    State.put (PPCtx (ind + chgAmt) acc newstatus lastnl)

indent :: State PPCtx ()
indent =
    State.get >>= \ (PPCtx _ _ lastIndStatus _) ->
    (let putboom = put "boom\n"
    in case lastIndStatus of
        LastIndIndent -> putboom
        LastIndDedent -> putboom
        _ -> return ()) >>
    chgInd 1 LastIndIndent

dedent :: State PPCtx ()
dedent =
    State.get >>= \ (PPCtx _ _ lastIndStatus _) ->
    (case lastIndStatus of
        LastIndIndent -> put "boom\n"
        _ -> return ()) >>
    chgInd (-1) LastIndDedent

putnl :: State PPCtx ()
putnl = put "\n"
-- helper functions {{{1
unlocate :: Located a -> a
unlocate (Located _ a) = a

maybeDo :: (a -> State PPCtx ()) -> Maybe a -> State PPCtx ()
maybeDo st m =
    case m of
        Just x -> st x
        Nothing -> return ()

actOnMutability :: State PPCtx () -> State PPCtx () -> AST.Mutability -> State PPCtx ()
actOnMutability ifMut ifImmut mutability =
    case mutability of
        AST.Mutable -> ifMut
        AST.Immutable -> ifImmut

ifMutablePut :: String -> AST.Mutability -> State PPCtx ()
ifMutablePut str mutability = actOnMutability (put str) (return ()) mutability

pprintList :: (a -> State PPCtx ()) -> [a] -> State PPCtx ()
pprintList pprintfun things = foldl' (>>) (return ()) $ map pprintfun things

pprintListDelim :: (a -> State PPCtx ()) -> State PPCtx () -> [a] -> State PPCtx ()
pprintListDelim pprintfun delim things = foldl' (>>) (return ()) $ intersperse delim $ map pprintfun things
-- AST.DModule {{{1
pprintModS :: AST.DModule -> State PPCtx ()
pprintModS (AST.DModule' decls) = pprintList (pprintDeclS . unlocate) decls
-- AST.DDecl {{{1
pprintDeclS :: AST.DDecl -> State PPCtx ()
pprintDeclS (AST.DDecl'Fun sf) = pprintFunDeclS $ unlocate sf
pprintDeclS (AST.DDecl'Impl ty members) =
    put "impl " >> pprintTypeS (unlocate ty) >> indent >> putnl >>
    pprintList (pprintImplMemberS . unlocate) members >>
    dedent
-- AST.DImplMember {{{1
pprintImplMemberS :: AST.DImplMember -> State PPCtx ()
pprintImplMemberS (AST.DImplMember'Fun sf) = pprintFunDeclS $ unlocate sf
-- AST.DStmt {{{1
pprintStmtS :: AST.DStmt -> State PPCtx ()

pprintStmtS (AST.DStmt'Var ty mutability name maybeinitializer) =
    put "var " >> ifMutablePut "mut " mutability >> put (unlocate name) >>
    pprintTypeAnnotationS (unlocate ty) >>
    (case maybeinitializer of
        Just (_, initExpr) -> put " = " >> pprintExprS (unlocate initExpr)
        Nothing -> return ()) >>
    putnl

pprintStmtS (AST.DStmt'Ret expr) = put "return " >>  pprintExprS (unlocate expr) >> putnl
pprintStmtS (AST.DStmt'Expr expr) = pprintExprS (unlocate expr) >> putnl

-- AST.DExpr {{{1
-- precedence things {{{
exprRequiresPrec :: AST.DExpr -> AST.ExprPrec
exprRequiresPrec (AST.DExpr'Block _) = AST.PrecBlockLevel
exprRequiresPrec (AST.DExpr'If _ _ _ _) = AST.PrecBlockLevel
exprRequiresPrec (AST.DExpr'While _ _) = AST.PrecBlockLevel
exprRequiresPrec (AST.DExpr'Assign _ op _) = AST.PrecAssign
exprRequiresPrec (AST.DExpr'ShortCircuit _ op _) = AST.shortOpPrec $ unlocate op
exprRequiresPrec (AST.DExpr'Binary _ op _) = AST.binOpPrec $ unlocate op
exprRequiresPrec (AST.DExpr'Cast _ _) = AST.PrecCast
exprRequiresPrec (AST.DExpr'Unary _ _) = AST.PrecUnary
exprRequiresPrec (AST.DExpr'Ref _ _ _) = AST.PrecUnary
exprRequiresPrec (AST.DExpr'Call _ _ _) = AST.PrecCall
exprRequiresPrec (AST.DExpr'Field _ _ _) = AST.PrecCall
exprRequiresPrec (AST.DExpr'Method _ _ _ _ _) = AST.PrecCall
exprRequiresPrec (AST.DExpr'Bool _) = AST.PrecPrimary
exprRequiresPrec (AST.DExpr'Float _) = AST.PrecPrimary
exprRequiresPrec (AST.DExpr'Int _) = AST.PrecPrimary
exprRequiresPrec (AST.DExpr'Char _) = AST.PrecPrimary
exprRequiresPrec (AST.DExpr'String _) = AST.PrecPrimary
exprRequiresPrec (AST.DExpr'This) = AST.PrecPrimary
exprRequiresPrec (AST.DExpr'Path _) = AST.PrecPrimary

pprintExprWithPrecS :: AST.ExprPrec -> AST.DExpr -> State PPCtx ()
pprintExprWithPrecS curPrec ex =
    if exprRequiresPrec ex < curPrec
    then put "(" >> pprintExprWithPrecS AST.PrecBlockLevel ex >> put ")"
    else pprintExprS' ex

-- }}}
-- printing different kinds of expressions {{{
pprintExprS' :: AST.DExpr -> State PPCtx ()
pprintExprS' ex = undefined
-- }}}
pprintExprS :: AST.DExpr -> State PPCtx ()
pprintExprS = pprintExprWithPrecS AST.PrecBlockLevel

pprintBlockExprS :: AST.SBlockExpr -> State PPCtx ()
pprintBlockExprS (AST.SBlockExpr' stmts) =
    indent >> putnl >>
    pprintList (pprintStmtS . unlocate) stmts >>
    dedent
-- AST.DParam {{{1
pprintParamS :: AST.DParam -> State PPCtx ()
pprintParamS (AST.DParam'Normal mutability lty lname) =
    -- TODO: properly handle 'this' parameters
    ifMutablePut "mut " mutability >>
    put (unlocate lname) >>
    pprintTypeAnnotationS (unlocate lty)
-- AST.DType {{{1
pprintTypeS :: AST.DType -> State PPCtx ()
pprintTypeS (AST.DType'Path path) = pprintPathS $ unlocate path
pprintTypeS (AST.DType'Pointer mutability lty) =
    put "*" >>
    ifMutablePut "mut " mutability >>
    pprintTypeS (unlocate lty)
pprintTypeS (AST.DType'This) = put "this"
-- AST.DPath {{{1
pprintPathS :: AST.DPath -> State PPCtx ()
pprintPathS (AST.DPath' segments) = pprintListDelim (put . unlocate) (put "::") segments
-- AST.SFunDecl {{{1
pprintFunDeclS :: AST.SFunDecl -> State PPCtx ()
pprintFunDeclS (AST.SFunDecl' retty (Located _ name) params expr) =
    put "fun " >> put name >>
    put "(" >> pprintListDelim (pprintParamS . unlocate) (put ", ") params >> put ")" >>
    (pprintTypeAnnotationS . unlocate) `maybeDo` retty >>
    pprintBlockExprS (unlocate expr)
-- print type as type annotation {{{1
pprintTypeAnnotationS :: AST.DType -> State PPCtx () -- TODO: do not print if without it defaults to the type
pprintTypeAnnotationS ty = put ": " >> pprintTypeS ty
-- splices {{{1
$(makePrintVariants "Mod")
$(makePrintVariants "Decl")
$(makePrintVariants "ImplMember")
$(makePrintVariants "Stmt")
$(makePrintVariants "Expr")
$(makePrintVariants "Param")
$(makePrintVariants "Type")
$(makePrintVariants "Path")
