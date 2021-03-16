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
    = MissingError String String Span
    | MustBeFollowedByFor String String String Span ParseError
    | InvalidChoice String String String Span ParseError ParseError
    | NeedOneOrMore String String Span ParseError
    | MustAppear String Span ParseError
    | NotAllowed String Span
    | ExcessTokens Span

-- TODO: TreeSection which takes two sections and indents them to show them as children
parseErrorMsg :: ParseError -> (Span, Message.Section)

parseErrorMsg (MissingError construct thing msp) = (msp,
    Message.makeUnderlinesSection [Message.UnderlineMessage msp Message.ErrorUnderline Message.Primary msg])
    where
        msg = construct ++ " is missing " ++ thing

parseErrorMsg (MustBeFollowedByFor construct a b sp berr) =
    (sp, Message.TreeSection (Just $ a ++ " of " ++ construct ++ " must be followed by " ++ b) [(Just $ b ++ " not found because of error:", snd (parseErrorMsg berr))])

parseErrorMsg (InvalidChoice construct a b sp aerr berr) =
    (sp, Message.TreeSection (Just $ "invalid " ++ construct ++ "; must be " ++ a ++ " or " ++ b) [
        (Just $ a ++ " not found because of error:", snd $ parseErrorMsg aerr), (Just $ b ++ " not found because of error:", snd $ parseErrorMsg berr)
    ])

parseErrorMsg (NeedOneOrMore construct item sp err) =
    (sp, Message.TreeSection (Just $ "need at least one " ++ item ++ " for " ++ construct ++ ", but found none") [
        (Just $ item ++ " not found because of error:", snd $ parseErrorMsg err)
    ])

parseErrorMsg (MustAppear thing sp err) =
    (sp, Message.TreeSection (Just $ thing ++ " must appear here") [
        (Just $ thing ++ " not found because of error:", snd $ parseErrorMsg err)
    ])

parseErrorMsg (NotAllowed thing sp) =
    (sp, Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary $ thing ++ " not allowed here"])

parseErrorMsg (ExcessTokens sp) =
    (sp, Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary "excess tokens in input"])

instance Message.ToDiagnostic ParseError where
    toDiagnostic e =
        let (sp, sec) = parseErrorMsg e
        in Message.SimpleDiag Message.Error (Just sp) Nothing Nothing [sec]

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

selectSpanFromParser :: Parser -> Span
selectSpanFromParser (Parser toks back) =
    let front =
            case toks of
                x:_ -> Just x
                [] -> Nothing
    in case (front, back) of
        (Just (Located _ Lex.EOF), Just (Located notEofSp _)) -> notEofSp
        (Just (Located eofSp Lex.EOF), Nothing) -> eofSp
        (Just (Located fsp _), _) -> fsp
        (Nothing, Just (Located bsp _)) -> bsp
        (Nothing, Nothing) -> error "parser has empty token stream, no last"

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

runParseFun (Consume construct thing predicate) parser = runParseFun asPredicate parser
    where
        asPredicate = Predicate construct thing maybeFilter
        maybeFilter (Just x) = predicate x
        maybeFilter Nothing = Nothing

runParseFun (Predicate construct thing predicate) parser@(Parser tokens _) =
    let arg = case tokens of
            t:_ -> Just t
            [] -> Nothing
    in case predicate arg of
        Just x -> Right (x, advance 1 parser)
        Nothing -> Left $ MissingError construct thing $ selectSpanFromParser parser

runParseFun (Empty _) parser = Right ((), parser)

runParseFun (Seq seqname a b conv) parser =
    runParseFun a parser >>= \ (ares, aftera) ->
    case runParseFun b aftera of
        Right (bres, afterb) -> Right (conv ares bres, afterb)
        Left berr ->
            Left $ MustBeFollowedByFor seqname (nameof a) (nameof b) (selectSpanFromParser aftera) berr

runParseFun (Choice choicename a b aconv bconv) parser =
    case runParseFun a parser of
        Right (res, after) -> Right (aconv res, after)
        Left aerr ->
            case runParseFun b parser of
                Right (res, after) -> Right (bconv res, after)
                Left berr ->
                    Left $ InvalidChoice choicename (nameof a) (nameof b) (selectSpanFromParser parser) aerr berr

runParseFun (Zeromore _ ex lconv) parser =
    let (things, parser') = helper parser
    in Right $ (lconv things, parser')
    where
        helper curparser =
            case runParseFun ex curparser of
                Right (res, after) ->
                    let (rest, afterrest) = helper after
                    in (res:rest, afterrest)
                Left _ -> ([], curparser)

runParseFun (Onemore listname ex lconv) parser = helper [] parser
    where
        helper acc curparser =
            case runParseFun ex curparser of
                Right (thing, nextparser) ->
                    helper (acc ++ [thing]) nextparser
                Left err ->
                    if length acc == 0
                    -- there are no other ones that matched, then the error matched by this branch is the reason why the first one couldnt match
                    then Left $ NeedOneOrMore listname (nameof ex) (selectSpanFromParser curparser) err
                    else Right (lconv acc, curparser)

runParseFun (Optional optname ex) parser = runParseFun asChoice parser
    where
        asChoice = Choice optname ex (Empty $ "omitted " ++ (nameof ex)) Just (const Nothing)

runParseFun (Must ex) parser =
    case runParseFun ex parser of
        Right _ -> Right $ ((), parser)
        Left err -> Left $ MustAppear (nameof ex) (selectSpanFromParser parser) err
runParseFun (MustNot ex) parser =
    case runParseFun ex parser of
        Right _ -> Left $ NotAllowed (nameof ex) (selectSpanFromParser parser)
        Left _ -> Right ((), parser)

runParseFun (Main ex) parser =
    runParseFun ex parser >>= \ totalres@(_, (Parser aftertokens _)) ->
    case aftertokens of
        [] -> error "parser should never be empty (in Main, EOF consumed elsewhere)"
        [(Located _ Lex.EOF)] -> Right totalres
        (Located sp _):_ -> Left $ ExcessTokens sp

grammar :: PEGExpr DCU
grammar =
    (Main (Choice "program"
        (Consume "var variant" "introductory token 'var'" (\ tok -> case tok of { Located _ Lex.Var -> Just $ makecu (); _ -> Nothing }))
        (Consume "let variant" "introductory token 'let'" (\ tok -> case tok of { Located _ Lex.Let -> Just $ makecu (); _ -> Nothing }))
        makecu makecu))

makecu :: a -> DCU
makecu _ = DCU'CU []

parse :: TokenStream -> Either ParseError DCU
parse toks = fst <$> (runParseFun grammar $ Parser toks Nothing)
