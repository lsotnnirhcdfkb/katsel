{-# LANGUAGE GADTs #-}

module Parse(parse) where

import Location
import qualified Lex
import qualified Message
import qualified AST

data ParseError
    = PredicateError String Span Lex.Token
    | MustBeFollowedByFor String String String Span ParseError
    | InvalidChoice String [(String, ParseError)] Span
    | NeedOneOrMore String String Span ParseError
    | MustAppear String Span ParseError
    | NotAllowed String Span

parseErrorMsg :: ParseError -> (Span, Message.Section)

parseErrorMsg (PredicateError thing msp gotInstead) = (msp,
    Message.makeUnderlinesSection [Message.UnderlineMessage msp Message.ErrorUnderline Message.Primary msg])
    where
        msg = thing ++ " is missing; " ++ Lex.fmtToken gotInstead ++ " was found instead"

parseErrorMsg (MustBeFollowedByFor construct a b sp berr) =
    (sp, Message.TreeSection (Just $ a ++ " of " ++ construct ++ " must be followed by " ++ b) [(Just $ b ++ " not recognized because of error:", snd (parseErrorMsg berr))])

parseErrorMsg (InvalidChoice construct choices sp) =
    -- TODO: format this nicer, don't include the brackets in the printing
    (sp, Message.TreeSection (Just $ "invalid " ++ construct ++ "; must be one of " ++ show (map fst choices)) $
    map (\ (name, err) -> (Just $ name ++ " not recognized because of error:", snd $ parseErrorMsg err)) choices)

parseErrorMsg (NeedOneOrMore construct item sp err) =
    (sp, Message.TreeSection (Just $ "need at least one " ++ item ++ " for " ++ construct ++ ", but found none") [
        (Just $ item ++ " not recognized because of error:", snd $ parseErrorMsg err)
    ])

parseErrorMsg (MustAppear thing sp err) =
    (sp, Message.TreeSection (Just $ thing ++ " must appear here") [
        (Just $ thing ++ " not recognized because of error:", snd $ parseErrorMsg err)
    ])

parseErrorMsg (NotAllowed thing sp) =
    (sp, Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary $ thing ++ " not allowed here"])

instance Message.ToDiagnostic ParseError where
    toDiagnostic e =
        let (sp, sec) = parseErrorMsg e
        in Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [ Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary "invalid syntax"]
            , sec
            ]

type TokenStream = [Located Lex.Token]
data Parser = Parser TokenStream (Maybe (Located Lex.Token))
type ParseResult a = (a, Parser)
data ParseFun a = ParseFun String (Parser -> (Either ParseError (ParseResult a)))

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

runParseFun :: ParseFun r -> Parser -> (Either ParseError (ParseResult r))
runParseFun (ParseFun _ fun) parser = fun parser

consume :: String -> (Located Lex.Token -> Maybe t) -> ParseFun t
consume name predicate = ParseFun name fun
    where
        fun parser@(Parser tokens _) =
            case tokens of
                (loct@(Located _ t)):_ ->
                    case predicate loct of
                        Just x -> Right (x, advance 1 parser)
                        Nothing -> Left $ PredicateError name (selectSpanFromParser parser) t
                [] -> error "parser has an empty token stream"

empty :: String -> ParseFun ()
empty name = ParseFun name $ \ parser -> Right ((), parser)

choice :: String -> [ParseFun a] -> ParseFun a
choice name choices = ParseFun name fun
    where
        fun parser =
            case success of
                Just s -> Right s
                Nothing -> Left $ InvalidChoice name errors $ selectSpanFromParser parser
            where
                results = map (\ ch -> (nameof ch, runParseFun ch parser)) choices
                successes = [x | (_, Right x) <- results]
                success = case successes of
                    x:_ -> Just x
                    [] -> Nothing

                errors = [(nm, err) | (nm, Left err) <- results]

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
                Left err ->
                    if length acc == 0
                    -- there are no other ones that matched, then the error matched by this branch is the reason why the first one couldnt match
                    then Left $ NeedOneOrMore name (nameof ex) (selectSpanFromParser parser) err
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
            case runParseFun ex parser of
                Right _ -> Right ((), parser)
                Left err -> Left $ MustAppear (nameof ex) (selectSpanFromParser parser) err

mustNotMatch :: ParseFun a -> ParseFun ()
mustNotMatch ex = ParseFun ("not a " ++ nameof ex) fun
    where
        fun parser =
            case runParseFun ex parser of
                Right _ -> Left $ NotAllowed (nameof ex) (selectSpanFromParser parser)
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

parse :: TokenStream -> Either ParseError AST.DCU
parse toks = fst <$> (runParseFun grammar $ Parser toks Nothing)
