{-# LANGUAGE GADTs #-}

module Parse(parse) where

import Location
import qualified Lex
import qualified Message
import qualified AST

data ParseError
    = PredicateError String Span Lex.Token
    | MustBeFollowedByFor String String String Span ParseError
    | InvalidChoice String String String Span ParseError ParseError
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

parseErrorMsg (InvalidChoice construct a b sp aerr berr) =
    (sp, Message.TreeSection (Just $ "invalid " ++ construct ++ "; must be " ++ a ++ " or " ++ b) [
        (Just $ a ++ " not recognized because of error:", snd $ parseErrorMsg aerr), (Just $ b ++ " not recognized because of error:", snd $ parseErrorMsg berr)
    ])

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
        in Message.SimpleDiag Message.Error (Just sp) Nothing Nothing [sec]

data PEGExpr r where
    Consume :: String -> (Located Lex.Token -> Maybe r) -> PEGExpr r
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
nameof (Consume n _) = n
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

runParseFun (Consume thing predicate) parser@(Parser tokens _) =
    case tokens of
        (loct@(Located _ t)):_ ->
            case predicate loct of
                Just x -> Right (x, advance 1 parser)
                Nothing -> Left $ PredicateError thing (selectSpanFromParser parser) t
        [] -> error "parser has an empty token stream"

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
    runParseFun newex parser
    where
        newex = Seq "compilation unit" ex consumeEOF const
        consumeEOF = Consume "end of file"
            (\ mtok ->
                case mtok of
                    Located _ Lex.EOF -> Just ()
                    _ -> Nothing
            )

grammar :: PEGExpr AST.DCU
grammar =
    (Main (Choice "token"
        (Consume "'var' token" (\ tok -> case tok of { Located _ Lex.Var -> Just $ makecu (); _ -> Nothing }))
        (Consume "'let' token" (\ tok -> case tok of { Located _ Lex.Let -> Just $ makecu (); _ -> Nothing }))
        makecu makecu))

makecu :: a -> AST.DCU
makecu _ = AST.DCU'CU []

parse :: TokenStream -> Either ParseError AST.DCU
parse toks = fst <$> (runParseFun grammar $ Parser toks Nothing)
