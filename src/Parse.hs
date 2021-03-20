{-# LANGUAGE GADTs #-}

module Parse(parse) where

import Location
import qualified Lex
import qualified Message
import qualified AST

data ParseError
    = ExpectedError String Span Lex.Token
    | NotAllowed String Span

instance Message.ToDiagnostic ParseError where
    toDiagnostic (ExpectedError name sp tok) =
        Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary $ "expected " ++ name ++ ", found " ++ Lex.fmtToken tok]]

    toDiagnostic (NotAllowed name sp) =
        Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary $ name ++ " not allowed here"]]


type TokenStream = [Located Lex.Token]
data Parser = Parser TokenStream (Maybe (Located Lex.Token))
type ParseResult a = (a, Parser)
data ParseFun a = ParseFun String (Parser -> (Either [ParseError] (ParseResult a)))

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

nameof :: ParseFun a -> String
nameof (ParseFun n _) = n

runParseFun :: ParseFun r -> Parser -> (Either [ParseError] (ParseResult r))
runParseFun (ParseFun _ fun) parser = fun parser

consume :: String -> (Located Lex.Token -> Maybe t) -> ParseFun t
consume name predicate = ParseFun name fun
    where
        fun parser@(Parser tokens _) =
            case tokens of
                (loct@(Located _ t)):_ ->
                    case predicate loct of
                        Just x -> Right (x, advance 1 parser)
                        Nothing -> Left [ExpectedError name (selectSpanFromParser parser) t]
                [] -> error "parser has an empty token stream"

empty :: String -> ParseFun ()
empty name = ParseFun name $ \ parser -> Right ((), parser)

choice :: String -> [ParseFun a] -> ParseFun a
choice name choices = ParseFun name fun
    where
        fun parser =
            case successes of
                res:_ -> Right res
                [] -> Left errors
            where
                results = map (\ ch -> runParseFun ch parser) choices
                successes = [res | Right res <- results]
                errors = concat [err | Left err <- results]

sequence :: String -> ParseFun a -> ParseFun b -> ParseFun (a, b)
sequence name a b = ParseFun name fun
    where
        fun parser =
            runParseFun a parser >>= \ (ares, aftera) ->
            runParseFun b parser >>= \ (bres, afterb) ->
            Right ((ares, bres), afterb)

zeromore :: String -> ParseFun a -> ParseFun [a]
zeromore name ex = ParseFun name (Right . fun)
    where
        fun parser =
            case runParseFun ex parser of
                Right (res, after) ->
                    let (rest, afterrest) = fun after
                    in (res:rest, afterrest)

                Left _ -> ([], parser)

onemore :: String -> ParseFun a -> ParseFun [a]
onemore name ex = ParseFun name $ fun []
    where
        fun acc parser =
            case runParseFun ex parser of
                Right (thing, nextparser) ->
                    fun (acc ++ [thing]) nextparser
                Left errs ->
                    if length acc == 0
                    then Left errs
                    else Right (acc, parser)

optional :: String -> ParseFun a -> ParseFun (Maybe a)
optional name ex = choice name [convert ex Just, convert (empty $ "omitted " ++ (nameof ex)) (const Nothing)]

convert :: ParseFun a -> (a -> b) -> ParseFun b
convert ex conv = ParseFun (nameof ex) fun
    where
        fun parser = runParseFun ex parser >>= \ (res, nextparser) -> Right $ (conv res, nextparser)

mustMatch :: ParseFun a -> ParseFun ()
mustMatch ex = ParseFun (nameof ex) fun
    where
        fun parser =
            runParseFun ex parser >>
            Right ((), parser)

mustNotMatch :: ParseFun a -> ParseFun ()
mustNotMatch ex = ParseFun ("not a " ++ nameof ex) fun
    where
        fun parser =
            case runParseFun ex parser of
                Right _ -> Left [NotAllowed (nameof ex) (selectSpanFromParser parser)]
                Left _ -> Right ((), parser)

mainParser :: ParseFun a -> ParseFun a
mainParser ex = ParseFun (nameof ex) fun
    where
        fun parser =
            runParseFun ex parser >>= \ (res, parser') ->
            runParseFun consumeEOF parser' >>= \ ((), parser'') ->
            Right (res, parser'')

        consumeEOF = consume "end of file" $ \ tok -> case tok of { Located _ Lex.EOF -> Just (); _ -> Nothing }

grammar :: ParseFun AST.DCU
grammar =
    (mainParser (choice "token" [
        (consume "'var' token" (\ tok -> case tok of { Located _ Lex.Var -> Just $ makecu (); _ -> Nothing })),
        (consume "'let' token" (\ tok -> case tok of { Located _ Lex.Let -> Just $ makecu (); _ -> Nothing }))
    ]))

makecu :: a -> AST.DCU
makecu _ = AST.DCU'CU []

parse :: TokenStream -> Either [ParseError] AST.DCU
parse toks = fst <$> (runParseFun grammar $ Parser toks Nothing)
