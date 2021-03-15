{-# LANGUAGE GADTs #-}

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

data ParseError
    = MissingError String String (Maybe Span)
    | MustBeFollowedByFor String String String Span ParseError
    | InvalidChoice String String String Span ParseError ParseError

-- TODO: TreeSection which takes two sections and indents them to show them as children
parseErrorMsg :: ParseError -> (Maybe Span, String)
parseErrorMsg (MissingError construct thing sp) = (sp, construct ++ " is missing " ++ thing)
parseErrorMsg (MustBeFollowedByFor construct a b sp berr) = (Just sp, a ++ " of " ++ construct ++ " must be followed by " ++ b ++ "; " ++ b ++ " not matched because of error: '" ++ snd (parseErrorMsg berr) ++ "'")
parseErrorMsg (InvalidChoice construct a b sp aerr berr) =
    (Just sp, "invalid " ++ construct ++ "; must be " ++ a ++ " or " ++ b ++ ", but neither matched; " ++ a ++ " not matched because of error: '" ++ snd (parseErrorMsg aerr) ++ "'; " ++ b ++ " not matched because of error: '" ++ snd (parseErrorMsg berr) ++ "'")

instance Message.ToDiagnostic ParseError where
    toDiagnostic e =
        let (msp, msg) = parseErrorMsg e
        in case msp of
            Just sp ->
                Message.SimpleDiag Message.Error (Just sp) Nothing Nothing [
                    Message.makeUnderlinesSection [
                        Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary msg
                    ]
                ]

            Nothing ->
                Message.SimpleDiag Message.Error Nothing Nothing Nothing [
                    Message.SimpleText msg
                ]

data PEGExpr r where
    Consume :: String -> String -> (Located Lex.Token -> Maybe r) -> PEGExpr r
    Predicate :: String -> String -> (Maybe (Located Lex.Token) -> Maybe r) -> PEGExpr r
    Empty :: String -> PEGExpr ()
    Seq :: String -> (PEGExpr a) -> (PEGExpr b) -> (a -> b -> r) -> PEGExpr r
    Choice :: String -> (PEGExpr a) -> (PEGExpr b) -> (a -> r) -> (b -> r) -> PEGExpr r
    Zeromore :: String -> (PEGExpr a) -> ([a] -> r) -> PEGExpr r
    Onemore :: String -> (PEGExpr a) -> ([a] -> r) -> PEGExpr r
    Optional :: String -> (PEGExpr a) -> PEGExpr (Maybe a)
    Must :: (PEGExpr a) -> PEGExpr ()
    MustNot :: (PEGExpr a) -> PEGExpr ()

    Main :: (PEGExpr a) -> (PEGExpr a)

type TokenStream = [Located Lex.Token]
data Parser = Parser TokenStream (Maybe (Located Lex.Token))
type ParseResult a = (a, Parser)

advance :: Int -> Parser -> Parser
advance 0 p = p
advance 1 (Parser (t:ts) _) = Parser ts $ Just t
advance n p = advance 1 $ advance (n - 1) p

selectSpanFromParser :: Parser -> Maybe Span
selectSpanFromParser (Parser ((Located sp _):_) _) = Just sp
selectSpanFromParser (Parser [] (Just (Located sp _))) = Just sp
selectSpanFromParser (Parser [] Nothing) = Nothing

selectSpanFromParser' :: Parser -> Span
selectSpanFromParser' p =
    case selectSpanFromParser p of
        Just x -> x
        Nothing -> error "force select span from parser, but parser has no location information"

nameof :: PEGExpr a -> String
nameof (Consume n _ _) = n
nameof (Predicate n _ _) = n
nameof (Empty n) = n
nameof (Seq n _ _ _) = n
nameof (Choice n _ _ _ _) = n
nameof (Zeromore n _ _) = n
nameof (Onemore n _ _) = n
nameof (Optional n _) = n
nameof (Must ex) = nameof ex
nameof (MustNot ex) = nameof ex
nameof (Main ex) = nameof ex

runParseFun :: PEGExpr r -> Parser -> (Either ParseError (ParseResult r))

runParseFun (Consume construct thing predicate) parser@(Parser tokens _) =
    case tokens of
        (t@(Located tsp _)):_ ->
            case predicate t of
                Just x -> Right (x, advance 1 parser)
                Nothing -> Left $ MissingError construct thing $ Just tsp
        [] -> Left $ MissingError construct thing $ selectSpanFromParser parser

runParseFun (Empty _) parser = Right ((), parser)

runParseFun (Seq seqname a b conv) parser =
    runParseFun a parser >>= \ (ares, aftera) ->
    case runParseFun b aftera of
        Right (bres, afterb) -> Right (conv ares bres, afterb)
        Left berr ->
            Left $ MustBeFollowedByFor seqname (nameof a) (nameof b) (selectSpanFromParser' aftera) berr

runParseFun (Choice choicename a b aconv bconv) parser =
    case runParseFun a parser of
        Right (res, after) -> Right (aconv res, after)
        Left aerr ->
            case runParseFun b parser of
                Right (res, after) -> Right (bconv res, after)
                Left berr ->
                    Left $ InvalidChoice choicename (nameof a) (nameof b) (selectSpanFromParser' parser) aerr berr
{-

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
-}

grammar :: PEGExpr DCU
grammar =
    (Choice "program"
        (Consume "var variant" "introductory token 'var'" (\ tok -> case tok of { Located _ Lex.Var -> Just $ makecu (); _ -> Nothing }))
        (Consume "let variant" "introductory token 'let'" (\ tok -> case tok of { Located _ Lex.Let -> Just $ makecu (); _ -> Nothing }))
        makecu makecu)


makecu :: a -> DCU
makecu _ = DCU'CU []

parse :: TokenStream -> Either ParseError DCU
parse toks = fst <$> (runParseFun grammar $ Parser toks Nothing)
