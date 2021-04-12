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

import Data.List(foldl', intersperse)

import Message.PrettyPrintTH

-- PPCtx {{{1
data PPCtx
    = PPCtx Int String Bool Bool
startCtx :: PPCtx
startCtx = PPCtx 0 "" False False
-- run pprint state helper {{{1
stateToFun :: (a -> State PPCtx ()) -> a -> String
stateToFun statefun thing =
    let (PPCtx _ res _ _) = execState (statefun thing) startCtx
    in res
-- put, putch, indent, dedent, and putnl {{{1
putch :: Char -> State PPCtx ()
putch ch = state $
    \ (PPCtx ind acc _ lastnl) ->
        let accWithIndent =
                if lastnl
                then acc ++ replicate (ind * 4) ' '
                else acc
            accWithCh = accWithIndent ++ [ch]
        in ((), PPCtx ind accWithCh False (ch == '\n'))

put :: String -> State PPCtx ()
put str =
    foldl (>>) (return ()) $ map putch str

indent :: State PPCtx ()
indent =
    state (
        \ (PPCtx ind acc lastChangesIndent _) ->
            let (curprintsnl, newacc) =
                    if lastChangesIndent
                    then (True, acc ++ "boom\n")
                    else (False, acc)
            in ((), PPCtx (ind + 1) newacc True curprintsnl)
    )

dedent :: State PPCtx ()
dedent =
    state (
        \ (PPCtx ind acc lastChangesIndent _) ->
            let (curprintsnl, newacc) =
                    if lastChangesIndent
                    then (True, acc ++ "boom\n")
                    else (False, acc)
            in ((), PPCtx (ind - 1) newacc True curprintsnl)
    )

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
pprintStmtS = undefined
-- AST.DExpr {{{1
pprintExprS :: AST.DExpr -> State PPCtx ()
pprintExprS = undefined
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
    putnl
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
