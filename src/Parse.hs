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


data Parser = Parser [Located Lex.Token] (Maybe (Located Lex.Token)) [ParseError]

-- TODO: this replicates the functionality of Control.State.Monad; i am making this for practice, but in the future i might make is just use Control.State.Monad
newtype ParseFun a = ParseFun (Parser -> (a, Parser))

type ParseFunM a = ParseFun (Maybe a)

instance Functor ParseFun where
    fmap f pf =
        ParseFun $
        \ parser ->
        let (res, after) = runParseFun pf parser
        in (f res, after)

instance Applicative ParseFun where
    pure thing = ParseFun $ \ parser -> (thing, parser)
    x <*> y =
        x >>= \ xres ->
        y >>= \ yres ->
        return (xres yres)

instance Monad ParseFun where
    return = pure
    a >>= b = ParseFun $
        \ parser ->
        let (ares, aftera) = runParseFun a parser
        in runParseFun (b ares) aftera

advance :: Int -> Parser -> Parser
advance 0 p = p
advance 1 (Parser (t:ts) _ errs) = Parser ts (Just t) errs
advance n p = advance 1 $ advance (n - 1) p

advanceS :: Int -> ParseFun ()
advanceS x = ParseFun $ \ parser -> ((), advance x parser)

peek :: Parser -> Located Lex.Token
peek (Parser (x:_) _ _) = x
peek (Parser [] _ _) = error "peek on empty token stream"

peekS :: ParseFun (Located Lex.Token)
peekS = ParseFun $ \ parser -> (peek parser, parser)

newErr :: ParseError -> Parser -> Parser
newErr err (Parser toks l errs) = Parser toks l (errs ++ [err])

newErrS :: ParseError -> ParseFun ()
newErrS err = ParseFun $ \ parser -> ((), newErr err parser)

getParser :: ParseFun Parser
getParser = ParseFun $ \ parser -> (parser, parser)

saveLocation :: ParseFun ([Located Lex.Token], Maybe (Located Lex.Token))
saveLocation = ParseFun $ \ parser@(Parser toks l _) -> ((toks, l), parser)

restoreLocation :: ([Located Lex.Token], Maybe (Located Lex.Token)) -> ParseFun ()
restoreLocation (toks, l) = ParseFun $ \ (Parser _ _ errs) -> ((), Parser toks l errs)

selectSpanFromParser :: Parser -> Span
selectSpanFromParser (Parser toks back _) =
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

runParseFun :: ParseFun r -> Parser -> (r, Parser)
runParseFun (ParseFun fun) parser = fun parser

consume :: String -> (Located Lex.Token -> Maybe t) -> ParseFunM t
consume name predicate =
    peekS >>= \ locatedPeeked@(Located _ peeked) ->
    case predicate locatedPeeked of
        Just x ->
            advanceS 1 >>
            return (Just x)
        Nothing ->
            getParser >>= \ parser ->
            newErrS (ExpectedError name (selectSpanFromParser parser) peeked) >>
            return Nothing

empty :: ParseFun ()
empty = return ()

choice :: [ParseFunM a] -> ParseFunM a
choice choices =
    saveLocation >>= tryChoices choices
    where
        tryChoices (c:cs) originalLocation =
            restoreLocation originalLocation >>
            c >>= \ res ->
            case res of
                Just x -> return (Just x)
                Nothing -> tryChoices cs originalLocation

        tryChoices [] originalLocation =
            restoreLocation originalLocation >>
            return Nothing

sequence :: ParseFun a -> ParseFun b -> ParseFun (a, b)
sequence a b =
    a >>= \ ares ->
    b >>= \ bres ->
    return (ares, bres)

zeromore :: ParseFunM a -> ParseFun [a]
zeromore ex = fun
    where
        fun =
            saveLocation >>= \ saved ->
            ex >>= \ mres ->
            case mres of
                Just res ->
                    fun >>= \ rest ->
                    return ([res] ++ rest)

                Nothing ->
                    restoreLocation saved >>
                    return []

onemore :: ParseFunM a -> ParseFunM [a]
onemore ex = fun []
    where
        fun acc =
            ex >>= \ res ->
            case res of
                Just thing ->
                    fun (acc ++ [thing])

                Nothing ->
                    if length acc == 0
                    then return Nothing
                    else return (Just acc)

optional :: ParseFunM a -> ParseFunM a
optional ex = choice [ex, convert empty (const Nothing)]

convert :: ParseFun a -> (a -> b) -> ParseFun b
convert ex conv =
    ex >>= \ res ->
    return (conv res)

mustMatch :: ParseFunM a -> ParseFunM ()
mustMatch ex =
    saveLocation >>= \ saved ->
    ex >>= \ res ->
    restoreLocation saved >>
    return (const () <$> res)

mustNotMatch :: String -> ParseFunM a -> ParseFunM ()
mustNotMatch thingName ex =
    saveLocation >>= \ saved ->
    ex >>= \ res ->
    restoreLocation saved >>
    case res of
        Just _ ->
            getParser >>= \ parser ->
            newErrS (NotAllowed thingName (selectSpanFromParser parser)) >>
            return Nothing

        Nothing ->
            return (Just ())

mainParser :: ParseFun a -> ParseFun a
mainParser ex =
    ex >>= \ res ->
    consume "end of file" (\ tok -> case tok of { Located _ Lex.EOF -> Just (); _ -> Nothing }) >>
    return res

grammar :: ParseFunM AST.DCU
grammar = mainParser $ convert declList (AST.DCU'CU <$>)

declList :: ParseFunM [AST.DDecl]
declList = onemore decl

decl :: ParseFunM AST.DDecl
decl = error "todo"

parse :: [Located Lex.Token] -> (Maybe AST.DCU, [ParseError])
parse toks =
    let (res, (Parser _ _ errs)) = runParseFun grammar $ Parser toks Nothing []
    in (res, errs)
