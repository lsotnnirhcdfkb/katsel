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

-- NOTE: NOT an automated code formatter!! just a pretty printer to print things in error messages!

-- PPrintSegment {{{1
data PPrintSegment
    = Literal String
    | Indent
    | Dedent
    | Newline
    | Boom
-- stringifySegments {{{1
newtype IndentAmt = IndentAmt { indentAmt :: Int }

processTwoSegments :: PPrintSegment -> PPrintSegment -> [PPrintSegment]
processTwoSegments = error "not implemented yet"

stringifySegments :: [PPrintSegment] -> String
stringifySegments segments = stringify $ process segments
    where
        stringify s = snd $ foldl' segmentToStr (IndentAmt 0, "") s

        process :: [PPrintSegment] -> [PPrintSegment]
        process (x : (more@(y:_))) = processTwoSegments x y ++ process more
        process [_] = []
        process [] = []

safeLast :: [a] -> Maybe a
safeLast [a] = Just a
safeLast (_:more) = safeLast more
safeLast [] = Nothing

addToAcc :: String -> IndentAmt -> String -> String
addToAcc acc (IndentAmt indamt) adding =
    acc ++
    (if safeLast acc == Just '\n'
    then replicate (indamt * 4) ' '
    else "") ++
    adding

segmentToStr :: (IndentAmt, String) -> PPrintSegment -> (IndentAmt, String)
segmentToStr (indamt, acc) (Literal s) = (indamt, addToAcc acc indamt s)
segmentToStr (indamt, acc) Indent = (IndentAmt $ indentAmt indamt + 1, addToAcc acc indamt "\n")
segmentToStr (indamt, acc) Dedent = (IndentAmt $ indentAmt indamt - 1, acc)
segmentToStr (indamt, acc) Newline = (indamt, addToAcc acc indamt "\n")
segmentToStr (indamt, acc) Boom = (indamt, addToAcc acc indamt "boom\n")
-- run pprint state helper {{{1
stateToFun :: (a -> State [PPrintSegment] ()) -> a -> String
stateToFun statefun thing = stringifySegments $ execState (statefun thing) []
-- put, putch, indent, dedent, and putnl {{{1
putch :: Char -> State [PPrintSegment] ()
putch '\n' = state $ \ segments -> ((), segments ++ [Newline])
putch ch = state $ \ segments -> ((), segments ++ [Literal [ch]])

put :: String -> State [PPrintSegment] ()
put str = foldl (>>) (return ()) $ map putch str

indent :: State [PPrintSegment] ()
indent = state $ \ segments -> ((), segments ++ [Indent])
dedent :: State [PPrintSegment] ()
dedent = state $ \ segments -> ((), segments ++ [Dedent])

putnl :: State [PPrintSegment] ()
putnl = state $ \ segments -> ((), segments ++ [Newline])
-- helper functions {{{1
unlocate :: Located a -> a
unlocate (Located _ a) = a

maybeDo :: (a -> State [PPrintSegment] ()) -> Maybe a -> State [PPrintSegment] ()
maybeDo st m =
    case m of
        Just x -> st x
        Nothing -> return ()

actOnMutability :: State [PPrintSegment] () -> State [PPrintSegment] () -> AST.Mutability -> State [PPrintSegment] ()
actOnMutability ifMut ifImmut mutability =
    case mutability of
        AST.Mutable -> ifMut
        AST.Immutable -> ifImmut

ifMutablePut :: String -> AST.Mutability -> State [PPrintSegment] ()
ifMutablePut str mutability = actOnMutability (put str) (return ()) mutability

pprintList :: (a -> State [PPrintSegment] ()) -> [a] -> State [PPrintSegment] ()
pprintList pprintfun things = foldl' (>>) (return ()) $ map pprintfun things

pprintListDelim :: (a -> State [PPrintSegment] ()) -> State [PPrintSegment] () -> [a] -> State [PPrintSegment] ()
pprintListDelim pprintfun delim things = foldl' (>>) (return ()) $ intersperse delim $ map pprintfun things
-- AST.DModule {{{1
pprintModS :: AST.DModule -> State [PPrintSegment] ()
pprintModS (AST.DModule' decls) = pprintList (pprintDeclS . unlocate) decls
-- AST.DDecl {{{1
pprintDeclS :: AST.DDecl -> State [PPrintSegment] ()
pprintDeclS (AST.DDecl'Fun sf) = pprintFunDeclS $ unlocate sf
pprintDeclS (AST.DDecl'Impl ty members) =
    put "impl " >> pprintTypeS (unlocate ty) >> indent >>
    pprintList (pprintImplMemberS . unlocate) members >>
    dedent
-- AST.DImplMember {{{1
pprintImplMemberS :: AST.DImplMember -> State [PPrintSegment] ()
pprintImplMemberS (AST.DImplMember'Fun sf) = pprintFunDeclS $ unlocate sf
-- AST.DStmt {{{1
pprintStmtS :: AST.DStmt -> State [PPrintSegment] ()

pprintStmtS (AST.DStmt'Var ty mutability name maybeinitializer) =
    put "var " >> ifMutablePut "mut " mutability >> put (unlocate name) >>
    pprintTypeAnnotationS (unlocate ty) >>
    (case maybeinitializer of
        Just (_, initExpr) -> put " = " >> pprintExprS (unlocate initExpr)
        Nothing -> return ()) >>
    putnl

pprintStmtS (AST.DStmt'Ret expr) = put "return " >>  pprintExprS (unlocate expr) >> putnl
-- TODO: properly handle {} expr stmt
pprintStmtS (AST.DStmt'Expr expr) =
    let neednl = case unlocate expr of
            AST.DExpr'Block _ -> False
            AST.DExpr'If _ _ _ _ -> False
            AST.DExpr'While _ _ -> False
            _ -> True
    in pprintExprS (unlocate expr) >> if neednl then putnl else return ()

-- AST.DExpr {{{1
-- precedence things {{{
exprRequiresPrec :: AST.DExpr -> AST.ExprPrec
exprRequiresPrec (AST.DExpr'Block _) = AST.PrecBlockLevel
exprRequiresPrec (AST.DExpr'If _ _ _ _) = AST.PrecBlockLevel
exprRequiresPrec (AST.DExpr'While _ _) = AST.PrecBlockLevel
exprRequiresPrec (AST.DExpr'Assign _ _ _) = AST.PrecAssign
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

pprintExprWithPrecS :: AST.ExprPrec -> AST.DExpr -> State [PPrintSegment] ()
pprintExprWithPrecS curPrec ex =
    if exprRequiresPrec ex < curPrec
    then put "(" >> pprintExprWithPrecS AST.PrecBlockLevel ex >> put ")"
    else pprintExprS' ex
-- }}}
-- printing different kinds of expressions {{{
pprintExprS' :: AST.DExpr -> State [PPrintSegment] ()

pprintExprS' (AST.DExpr'Block bl) = pprintBlockExprS $ unlocate bl

pprintExprS' (AST.DExpr'If _ cond trueb mfalseb) =
    put "if " >> pprintExprS (unlocate cond) >> pprintExprS (unlocate trueb) >>
    case mfalseb of
        Just (_, falseb) -> put "else " >> pprintExprS (unlocate falseb)
        Nothing -> return ()

pprintExprS' (AST.DExpr'While cond body) = put "while " >> pprintExprS (unlocate cond) >> pprintExprS (unlocate body)

pprintExprS' (AST.DExpr'Assign lhs op rhs) =
    let opstr = case unlocate op of
            AST.Equal -> "="
    in pprintExprWithPrecS AST.PrecBinOr (unlocate lhs) >> put " " >> put opstr >> put " " >> pprintExprWithPrecS AST.PrecAssign (unlocate rhs)

pprintExprS' (AST.DExpr'ShortCircuit lhs op rhs) =
    let opstr = case unlocate op of
            AST.DoubleAmper -> "&&"
            AST.DoublePipe -> "||"

        opprec = AST.shortOpPrec $ unlocate op

        lhsprec = opprec

        rhsprec = case opprec of
            AST.PrecBinOr -> AST.PrecBinAnd
            AST.PrecBinAnd -> AST.PrecCompEQ
            _ -> error "invalid precedence for precedence of short circuit operator"

    in pprintExprWithPrecS lhsprec (unlocate lhs) >> put " " >> put opstr >> put " " >> pprintExprWithPrecS rhsprec (unlocate rhs)

pprintExprS' (AST.DExpr'Binary lhs op rhs) =
    let opprec = AST.binOpPrec $ unlocate op

        lhsprec = opprec

        rhsprec = case opprec of
            AST.PrecCompEQ -> AST.PrecCompLGT
            AST.PrecCompLGT -> AST.PrecBitXor
            AST.PrecBitXor -> AST.PrecBitOr
            AST.PrecBitOr -> AST.PrecBitAnd
            AST.PrecBitAnd -> AST.PrecBitShift
            AST.PrecBitShift -> AST.PrecAdd
            AST.PrecAdd -> AST.PrecMult
            AST.PrecMult -> AST.PrecCast
            _ -> error "invalid precedence for precedence of binary operator"

        opstr = case unlocate op of
            AST.Plus -> "+"
            AST.Minus -> "-"
            AST.Star -> "*"
            AST.Slash -> "/"
            AST.Percent -> "%"
            AST.Greater -> ">"
            AST.Less -> "<"
            AST.GreaterEqual -> ">="
            AST.LessEqual -> "<="
            AST.Amper -> "&"
            AST.Pipe -> "|"
            AST.Caret -> "^"
            AST.DoubleGreater -> ">>"
            AST.DoubleLess -> "<<"
            AST.DoubleEqual -> "=="
            AST.BangEqual -> "!="

    in pprintExprWithPrecS lhsprec (unlocate lhs) >> put " " >> put opstr >> put " " >> pprintExprWithPrecS rhsprec (unlocate rhs)

pprintExprS' (AST.DExpr'Cast ty expr) = pprintExprWithPrecS AST.PrecUnary (unlocate expr) >> put " -> " >> pprintTypeS (unlocate ty)

pprintExprS' (AST.DExpr'Unary op expr) =
    let opstr = case unlocate op of
            AST.UnBang -> "!"
            AST.UnTilde -> "~"
            AST.UnMinus -> "-"
            AST.UnStar -> "*"
    in put opstr >> pprintExprWithPrecS AST.PrecUnary (unlocate expr)
pprintExprS' (AST.DExpr'Ref _ mutability expr) = put "&" >> ifMutablePut "mut " mutability >> pprintExprWithPrecS AST.PrecUnary (unlocate expr)

pprintExprS' (AST.DExpr'Call expr _ args) =
    (let calleeIsField = case unlocate expr of
            AST.DExpr'Field _ _ _ -> True
            _ -> False
    in if calleeIsField
    then put "(" >> pprintExprS (unlocate expr) >> put ")"
    else pprintExprWithPrecS AST.PrecCall (unlocate expr)) >>
    put "(" >> pprintListDelim (pprintExprS . unlocate) (put ", ") args >> put ")"

pprintExprS' (AST.DExpr'Field expr _ field) = pprintExprWithPrecS AST.PrecCall (unlocate expr) >> put "." >> put (unlocate field)
pprintExprS' (AST.DExpr'Method expr _ method _ args) =
    pprintExprWithPrecS AST.PrecCall (unlocate expr) >> put "." >> put (unlocate method) >>
    put "(" >> pprintListDelim (pprintExprS . unlocate) (put ", ") args >> put ")"

pprintExprS' (AST.DExpr'Bool val) = put $ if val then "true" else "false"
pprintExprS' (AST.DExpr'Float val) = put $ show val
pprintExprS' (AST.DExpr'Int val) = put $ show val
pprintExprS' (AST.DExpr'Char val) = put $ show val
-- TODO: escape things and properly print multiline strings
pprintExprS' (AST.DExpr'String val) = put $ show val
pprintExprS' (AST.DExpr'This) = put "this"
pprintExprS' (AST.DExpr'Path path) = pprintPathS $ unlocate path
-- }}}
pprintExprS :: AST.DExpr -> State [PPrintSegment] ()
pprintExprS = pprintExprWithPrecS AST.PrecBlockLevel

pprintBlockExprS :: AST.SBlockExpr -> State [PPrintSegment] ()
pprintBlockExprS (AST.SBlockExpr' stmts) =
    indent >>
    pprintList (pprintStmtS . unlocate) stmts >>
    dedent
-- AST.DParam {{{1
pprintParamS :: AST.DParam -> State [PPrintSegment] ()
pprintParamS (AST.DParam'Normal mutability lty lname) =
    -- TODO: properly handle 'this' parameters
    ifMutablePut "mut " mutability >>
    put (unlocate lname) >>
    pprintTypeAnnotationS (unlocate lty)
-- AST.DType {{{1
pprintTypeS :: AST.DType -> State [PPrintSegment] ()
pprintTypeS (AST.DType'Path path) = pprintPathS $ unlocate path
pprintTypeS (AST.DType'Pointer mutability lty) =
    put "*" >>
    ifMutablePut "mut " mutability >>
    pprintTypeS (unlocate lty)
pprintTypeS (AST.DType'This) = put "this"
-- AST.DPath {{{1
pprintPathS :: AST.DPath -> State [PPrintSegment] ()
pprintPathS (AST.DPath' segments) = pprintListDelim (put . unlocate) (put "::") segments
-- AST.SFunDecl {{{1
pprintFunDeclS :: AST.SFunDecl -> State [PPrintSegment] ()
pprintFunDeclS (AST.SFunDecl' retty (Located _ name) params expr) =
    put "fun " >> put name >>
    put "(" >> pprintListDelim (pprintParamS . unlocate) (put ", ") params >> put ")" >>
    (pprintTypeAnnotationS . unlocate) `maybeDo` retty >>
    pprintBlockExprS (unlocate expr)
-- print type as type annotation {{{1
pprintTypeAnnotationS :: AST.DType -> State [PPrintSegment] () -- TODO: do not print if without it defaults to the type
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
