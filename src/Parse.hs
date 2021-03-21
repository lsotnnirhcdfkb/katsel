module Parse(parse) where

import Location
import qualified Lex
import qualified Message
import qualified Message.Underlines as MsgUnds
import qualified AST

import Data.Data(toConstr, Data)

data ErrorCondition
    = XIsMissingYFound String String Span Lex.Token
    | NotAllowedBecause String String Span
    | NotAllowed String Span
    | ExcessTokens Span Lex.Token
    | DummyError

condToMsgs :: ErrorCondition -> [MsgUnds.Message]

condToMsgs (XIsMissingYFound x y sp tok) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ "; " ++ Lex.fmtToken tok ++ " was found instead"
    ]
condToMsgs (NotAllowedBecause thing reason sp) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ thing ++ " not allowed here " ++ reason
    ]
condToMsgs (NotAllowed thing sp) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ thing ++ " not allowed"
    ]
condToMsgs (ExcessTokens sp tok) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "extraneous tokens found in input (" ++ Lex.fmtToken tok ++ ")"
    ]
condToMsgs DummyError = []

data ParseError = ParseError [ErrorCondition]
instance Message.ToDiagnostic ParseError where
    toDiagnostic (ParseError msgs) =
        Message.SimpleDiag Message.Error Nothing Nothing Nothing
            [ Message.Underlines $ MsgUnds.UnderlinesSection $ concatMap condToMsgs msgs
            ]

data Parser = Parser [Located Lex.Token] (Maybe (Located Lex.Token)) [ErrorCondition]

-- TODO: this replicates the functionality of Control.State.Monad; i am making this for practice, but in the future i might make is just use Control.State.Monad
newtype ParseFun a = ParseFun (Parser -> (a, Parser))

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

type ParseFunM a = ParseFun (Maybe a)

constrEq :: (Data a, Data b) => a -> b -> Bool
constrEq a b = toConstr a == toConstr b

isTT :: Data a => a -> Located Lex.Token -> Bool
isTT a (Located _ b) = constrEq a b

-- isTTP :: Data a => a -> Located Lex.Token -> Maybe (Located Lex.Token)
-- isTTP a b = if isTT a b then Just b else Nothing

isTTU :: Data a => a -> Located Lex.Token -> Maybe ()
isTTU a b = if isTT a b then Just () else Nothing

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

newErr :: ErrorCondition -> Parser -> Parser
newErr err (Parser toks l errs) = Parser toks l (errs ++ [err])

newErrS :: ErrorCondition -> ParseFun ()
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

consume :: (Located Lex.Token -> Maybe t) -> (Span -> Located Lex.Token -> ErrorCondition) -> ParseFunM t
consume predicate onerr =
    peekS >>= \ locatedPeeked ->
    case predicate locatedPeeked of
        Just x ->
            advanceS 1 >>
            return (Just x)
        Nothing ->
            getParser >>= \ parser ->
            newErrS (onerr (selectSpanFromParser parser) locatedPeeked) >>
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

sequence :: ParseFunM a -> ParseFunM b -> ParseFunM (a, b)
sequence a b =
    a >>= \ mares ->
    case mares of
        Just ares ->
            b >>= \ mbres ->
                case mbres of
                    Just bres -> return (Just (ares, bres))
                    Nothing -> return Nothing
        Nothing -> return Nothing

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

mustNotMatch :: ParseFunM a -> (Span -> ErrorCondition) -> ParseFunM ()
mustNotMatch ex onerr =
    saveLocation >>= \ saved ->
    ex >>= \ res ->
    restoreLocation saved >>
    case res of
        Just _ ->
            getParser >>= \ parser ->
            newErrS (onerr $ selectSpanFromParser parser) >>
            return Nothing

        Nothing ->
            return (Just ())

mainParser :: ParseFun a -> ParseFun a
mainParser ex =
    ex >>= \ res ->
    consume (\ tok -> case tok of { Located _ Lex.EOF -> Just (); _ -> Nothing }) (\ sp (Located _ tok) -> ExcessTokens sp tok) >>
    return res


mkXYFConsume :: String -> String -> Span -> Located Lex.Token -> ErrorCondition
mkXYFConsume construct thing sp (Located _ tok) = XIsMissingYFound construct thing sp tok

mkDummy2 :: a -> b -> ErrorCondition
mkDummy2 _ _ = DummyError

grammar :: ParseFunM AST.DCU
grammar = mainParser $ convert declList (const $ Just $ AST.DCU'CU [])

declList :: ParseFunM [()]
declList = onemore decl

decl :: ParseFunM ()
decl = choice [functionDecl, implDecl]

functionDecl :: ParseFunM ()
functionDecl =
    convert
        (Parse.sequence
            (consume (isTTU Lex.Fun) (mkXYFConsume "function declaration" "introductory token 'fun'"))
            (mustNotMatch (consume (isTTU $ Lex.Identifier "") mkDummy2) (NotAllowed "function name")))
    (const () <$>)

implDecl :: ParseFunM ()
implDecl = consume (isTTU Lex.Impl) (mkXYFConsume "'impl' declaration" "introductory token 'impl'")

parse :: [Located Lex.Token] -> (Maybe AST.DCU, ParseError)
parse toks =
    let (res, (Parser _ _ errs)) = runParseFun grammar $ Parser toks Nothing []
    in (res, ParseError errs)
