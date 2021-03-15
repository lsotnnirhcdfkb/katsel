module Parse where

import Location
import qualified Lex
import qualified Message

{-
    most asts are just a plain adt, but some asts are required in multiple asts
    for example, function declarations are needed in a standard Decl and also in an ImplMember
    to prevent duplication, these asts are separated into separate datatypes

    so, a Decl is a variant, but a FunDecl is a separated datatype (due to its multiple uses)

    so, there are 2 types of datatypes in this module:
        - variant datatypes
        - separated datatypes

    to prevent name collisions, variant datatypes are prefixed with 'D', and separated datatypes are prefixed with 'S'
    constructor names are prefixed with the datatype name and a '''
    so, the Decl/FunctionDecl hierarchy would be

        data DDecl
            = DDecl'Fun SFunDecl
            ...

        data SFunDecl
            = SFunDecl' DType ...
-}

type LocStr = Located String

data Mutability = Mutable | Immutable
data ThisParamKind = Value | Ref | MutRef

data BinOp
    = Plus | Minus | Star | Slash | Percent
    | Greater | Less | GreaterEqual | LessEqual
    | Amper | Pipe | Caret
    | DoubleGreater | DoubleLess
    | DoubleEqual | BangEqual

data ShortOp = DoubleAmper | DoublePipe
data UnaryOp = UnBang | UnTilde | UnMinus | UnAmper | UnStar
data AssignOp = Equal

data SFunDecl = SFunDecl' DType LocStr [DParam] SBlockExpr

data SBlockExpr = SBlockExpr' [DStmt]

data DCU = DCU'CU [DDecl]

data DDecl
    = DDecl'Fun SFunDecl
    | DDecl'Impl DType [DImplMember]

data DImplMember
    = DImplMember'Fun SFunDecl

data DStmt
    = DStmt'Var DType Mutability LocStr (Maybe (Span, DExpr))
    | DStmt'Expr DExpr
    | DStmt'Ret DExpr

data DExpr
    = DExpr'Block SBlockExpr
    | DExpr'If Span DExpr DExpr (Maybe (Span, DExpr))
    | DExpr'While DExpr DExpr
    | DExpr'Assign DExpr AssignOp DExpr
    | DExpr'ShortCircuit DExpr ShortOp DExpr
    | DExpr'Binary DExpr BinOp DExpr
    | DExpr'Cast DType DExpr
    | DExpr'Unary UnaryOp DExpr
    | DExpr'Call DExpr Span [DExpr]
    | DExpr'Field DExpr Span LocStr
    | DExpr'Method DExpr Span LocStr Span [DExpr]
    | DExpr'Bool Bool
    | DExpr'Float Double
    | DExpr'Int Integer
    | DExpr'Char Char
    | DExpr'String String
    | DExpr'This Span
    | DExpr'Path DPath

data DParam
    = DParam'Normal Mutability DType LocStr
    | DParam'This ThisParamKind

data DType
    = DType'Path DPath
    | DType'Pointer Mutability DType
    | DType'This Span

data DPath = DPath' [LocStr]

data ParseError = ParseError [String]
instance Message.ToDiagnostic ParseError where
    toDiagnostic (ParseError _) = error "TODO"

type ParseFun a = [Located Lex.Token] -> (Either ParseError (a, [Located Lex.Token]))

consume :: (Located Lex.Token -> Maybe a) -> ParseFun a
consume predicate = \ tokens ->
    case tokens of
        [] -> Left $ ParseError ["unexpected eof"]
        tok:more ->
            case predicate tok of
                Just res -> Right (res, more)
                Nothing -> Left $ ParseError ["expected thing"]

empty :: ParseFun ()
empty = \ tokens ->
    Right $ ((), tokens)

sequence :: ParseFun a -> ParseFun b -> (a -> b -> c) -> ParseFun c
sequence a b converter = \ tokens ->
    a tokens >>= \ (ares, aftera) ->
    b aftera >>= \ (bres, afterb) ->
    Right $ (converter ares bres, afterb)

choice :: ParseFun a -> ParseFun b -> (a -> c) -> (b -> c) -> ParseFun c
choice a b aconv bconv = \ tokens ->
    case a tokens of
        Right (res, after) -> Right (aconv res, after)
        Left (ParseError aerr) ->
            case b tokens of
                Right (res, after) -> Right (bconv res, after)
                Left (ParseError berr) -> Left $ ParseError $ aerr ++ berr

zeromore :: ParseFun a -> ([a] -> b) -> ParseFun b
zeromore ex conv = \ tokens ->
    let (things, after) = helper tokens
    in Right (conv things, after)
    where
        helper cur =
            case ex cur of
                Right (res, after) ->
                    let (things, afterafter) = helper after
                    in (res:things, afterafter)
                Left _ -> ([], cur)

onemore :: ParseFun a -> ([a] -> b) -> ParseFun b
onemore ex conv = \ tokens ->
    helper [] tokens
    where
        helper acc cur =
            case ex cur of
                Right (thing, rest) ->
                    helper (acc ++ [thing]) rest

                Left _ ->
                    if length acc == 0
                    then Left $ ParseError ["expected one or more of thing"]
                    else Right (conv acc, cur)

optional :: ParseFun a -> ParseFun (Maybe a)
optional ex = choice ex empty Just (const Nothing)

andpred :: ParseFun a -> ParseFun ()
andpred ex = \ tokens ->
    case ex tokens of
        Right _ -> Right ((), tokens)
        Left err -> Left err

notpred :: ParseFun a -> ParseFun ()
notpred ex = \ tokens ->
    case ex tokens of
        Left _ -> Right ((), tokens)
        Right _ -> Left "expected not thing"

parse :: ParseFun DCU
parse = error "TODO"
