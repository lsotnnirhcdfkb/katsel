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
    deriving Show
data ThisParamKind = Value | Ref | MutRef
    deriving Show

data BinOp
    = Plus | Minus | Star | Slash | Percent
    | Greater | Less | GreaterEqual | LessEqual
    | Amper | Pipe | Caret
    | DoubleGreater | DoubleLess
    | DoubleEqual | BangEqual
    deriving Show

data ShortOp = DoubleAmper | DoublePipe
    deriving Show
data UnaryOp = UnBang | UnTilde | UnMinus | UnAmper | UnStar
    deriving Show
data AssignOp = Equal
    deriving Show

data SFunDecl = SFunDecl' DType LocStr [DParam] SBlockExpr
    deriving Show

data SBlockExpr = SBlockExpr' [DStmt]
    deriving Show

data DCU = DCU'CU [DDecl]
    deriving Show

data DDecl
    = DDecl'Fun SFunDecl
    | DDecl'Impl DType [DImplMember]
    deriving Show

data DImplMember
    = DImplMember'Fun SFunDecl
    deriving Show

data DStmt
    = DStmt'Var DType Mutability LocStr (Maybe (Span, DExpr))
    | DStmt'Expr DExpr
    | DStmt'Ret DExpr
    deriving Show

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
    deriving Show

data DParam
    = DParam'Normal Mutability DType LocStr
    | DParam'This ThisParamKind
    deriving Show

data DType
    = DType'Path DPath
    | DType'Pointer Mutability DType
    | DType'This Span
    deriving Show

data DPath = DPath' [LocStr]
    deriving Show

data ParseError = ParseError [String]
    deriving Show
instance Message.ToDiagnostic ParseError where
    toDiagnostic (ParseError msgs) =
        Message.SimpleDiag Message.Error Nothing Nothing Nothing texts
        where
            texts = map Message.SimpleText msgs

type TokenStream = [Located Lex.Token]
data ParseFun a = ParseFun String (TokenStream -> (Either ParseError (a, TokenStream)))

runParseFun :: ParseFun a -> TokenStream -> (Either ParseError (a, TokenStream))
runParseFun (ParseFun _ fun) = fun

consume :: (Located Lex.Token -> Maybe a) -> String -> ParseFun a
consume predicate name = ParseFun name fun
    where
        fun = \ tokens ->
            case tokens of
                [] -> Left $ ParseError ["unexpected eof, expected " ++ name]
                tok:more ->
                    case predicate tok of
                        Just res -> Right (res, more)
                        Nothing -> Left $ ParseError ["expected " ++ name]

empty :: String -> ParseFun ()
empty name = ParseFun name fun
    where
        fun = \ tokens -> Right $ ((), tokens)

sequence :: ParseFun a -> ParseFun b -> (a -> b -> c) -> String -> ParseFun c
sequence a b converter name = ParseFun name fun
    where
        fun = \ tokens ->
            runParseFun a tokens >>= \ (ares, aftera) ->
            runParseFun b aftera >>= \ (bres, afterb) ->
            Right $ (converter ares bres, afterb)

choice :: ParseFun a -> ParseFun b -> (a -> c) -> (b -> c) -> String -> ParseFun c
choice a b aconv bconv name = ParseFun name fun
    where
        fun = \ tokens ->
            case runParseFun a tokens of
                Right (res, after) -> Right (aconv res, after)
                Left (ParseError aerr) ->
                    case runParseFun b tokens of
                        Right (res, after) -> Right (bconv res, after)
                        Left (ParseError berr) -> Left $ ParseError $ aerr ++ berr

zeromore :: ParseFun a -> ([a] -> b) -> String -> ParseFun b
zeromore ex conv name = ParseFun name fun
    where
        fun = \ tokens ->
            let (things, after) = helper tokens
            in Right (conv things, after)

        helper cur =
            case runParseFun ex cur of
                Right (res, after) ->
                    let (things, afterafter) = helper after
                    in (res:things, afterafter)
                Left _ -> ([], cur)

onemore :: ParseFun a -> ([a] -> b) -> String -> ParseFun b
onemore ex@(ParseFun exname _) conv name = ParseFun name fun
    where
        fun = \ tokens -> helper [] tokens
        helper acc cur =
            case runParseFun ex cur of
                Right (thing, rest) ->
                    helper (acc ++ [thing]) rest

                Left (ParseError err) ->
                    if length acc == 0
                    -- there are no other ones that matched, then the error matched by this branch is the reason why the first one couldnt match
                    then Left $ ParseError ["expected one or more of " ++ exname ++ ", but could not find (at least) one because of the following reasons: " ++ show err]
                    else Right (conv acc, cur)

optional :: ParseFun a -> ParseFun (Maybe a)
optional ex@(ParseFun name _) = choice ex (empty $ "omitted " ++ name) Just (const Nothing) name

andpred :: ParseFun a -> ParseFun ()
andpred ex@(ParseFun name _) = ParseFun ("required " ++ name) fun
    where
        fun = \ tokens ->
            case runParseFun ex tokens of
                Right _ -> Right ((), tokens)
                Left err -> Left err

notpred :: ParseFun a -> ParseFun ()
notpred ex@(ParseFun name _) = ParseFun ("not " ++ name) fun
    where
        fun = \ tokens ->
            case runParseFun ex tokens of
                Left _ -> Right ((), tokens)
                Right _ -> Left $ ParseError ["cannot have " ++ name ++ " here"]

parse :: TokenStream -> Either ParseError DCU
parse toks =
    runParseFun fun toks >>= \(res, rest) ->
    if length rest == 0
    then Right res
    else Left $ ParseError ["expected eof, but got " ++ (show $ head rest) ++ ", rest" ++ (show $ tail rest)]
    where
        fun = onemore (choice parseVarStmt parseRetStmt makeunit makeunit "var or ret stmt") makecu "list of var or ret stmt"
        -- fun = choice parseVarStmt parseRetStmt makecu makecu "var or ret stmt"

        makeunit _ = ()
        makecu _ = DCU'CU []

        parseVarStmt =
            (Parse.sequence
                (Parse.sequence 
                    (consume (\ tok -> case tok of { Located _ Lex.Var -> Just (); _ -> Nothing }) "'var' for var stmt")
                    (consume (\ tok -> case tok of { Located _ (Lex.Identifier name) -> Just name; _ -> Nothing}) "var name")
                    (\ _ _ -> ())
                    "first two tokens of var stsmt")
                (consume (\ tok -> case tok of { Located _ Lex.Newline -> Just (); _ -> Nothing}) "nl")
                (\ _ _ -> ())
                "var stmt")

        parseRetStmt =
            (Parse.sequence 
                (consume (\ tok -> case tok of { Located _ Lex.Return -> Just (); _ -> Nothing }) "'return' for return stmt")
                (consume (\ tok -> case tok of { Located _ Lex.Newline -> Just (); _ -> Nothing}) "nl")
                (\ _ _ -> ())
                "retr stmt")
