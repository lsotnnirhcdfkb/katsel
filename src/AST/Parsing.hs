module AST.Parsing
    ( parse_from_toks
    , ParseError
    ) where

import Location
import MonadUtils
import MaybeWithError

import AST.Datatypes

import qualified Tokens
import qualified Message
import qualified Message.Underlines as MsgUnds

import Data.Data (toConstr, Data)
import Data.List (find)

import Control.Monad.State.Lazy (State, state, runState)

-- ParseError {{{1
data ParseError
    = Expected String Span

type ParseErrorSpanPending = Span -> ParseError

instance Message.ToDiagnostic ParseError where
    to_diagnostic (Expected thing sp) =
        Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [ Message.Underlines
                [ MsgUnds.Underline sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error $ "expected " ++ thing]
                ]
            ]

-- Parser {{{1
data Parser
    = Parser
      { get_parse_errors :: [ParseError]
      , get_token_stream :: [Located Tokens.Token]
      , last_token :: Maybe (Located Tokens.Token)
      }
-- type synonyms {{{1
type ParseFun = State Parser
type ParseFunM a = ParseFun (Maybe a)
type ParseFunMWE a = ParseFun (MaybeWithError a ParseError)

type ParserPredicate = Parser -> Bool
type SynchronizationPredicate = ParserPredicate
type StopPredicate = ParserPredicate

type TokenPredicate = Located Tokens.Token -> Bool
-- error helpers {{{1
add_error :: ParseError -> ParseFun (ErrorLoggedPromise ParseError)
add_error err = state $ \ parser -> (error_logged_promise err, parser { get_parse_errors = err : get_parse_errors parser })

add_error_and_nothing :: ParseError -> ParseFunMWE r
add_error_and_nothing err = add_error err >>= return . NothingWithError
-- span_from_list {{{1
span_from_list :: [Located a] -> Span -> Span
span_from_list [] s = s
span_from_list items _ = get_span (head items) `join_span` get_span (last items)

-- predicates {{{1
constr_eq :: (Data a, Data b) => a -> b -> Bool
constr_eq a b = toConstr a == toConstr b

is_tt :: Tokens.Token -> TokenPredicate
is_tt a (Located _ b) = constr_eq a b

is_tt_u :: Tokens.Token -> Located Tokens.Token -> Maybe ()
is_tt_u a b = if is_tt a b then Just () else Nothing

is_tt_s :: Tokens.Token -> Located Tokens.Token -> Maybe Span
is_tt_s a b@(Located sp _) = if is_tt a b then Just sp else Nothing

is_at_end :: ParserPredicate
is_at_end = peek_matches_pred $ is_tt Tokens.EOF
-- predicate combinators {{{1
predicate_or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
predicate_or p1 p2 a = p1 a || p2 a

predicate_and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
predicate_and p1 p2 a = p1 a && p2 a

peek_matches_pred :: TokenPredicate -> ParserPredicate
peek_matches_pred predicate = predicate . head . get_token_stream
-- basic functions {{{1
peek :: ParseFun (Located Tokens.Token)
peek = state $ \ parser -> (head $ get_token_stream parser, parser)

peek_span :: ParseFun Span
peek_span = peek >>= \ (Located sp _) -> return sp

peek_matches :: TokenPredicate -> ParseFun Bool
peek_matches predicate = peek >>= return . predicate

peek_match :: [(TokenPredicate, ParseFun a)] -> ParseFun a -> ParseFun a
peek_match predicates fallback =
    peek >>= \ tok ->
    let results = map (\ (predicate, act) -> (predicate tok, act)) predicates
    in case find fst results of
        Just (_, act) -> act
        Nothing -> fallback


parser_matches :: ParserPredicate -> ParseFun Bool
parser_matches predicate = state $ \ parser -> (predicate parser, parser)

advance :: ParseFun (Located Tokens.Token)
advance = state $ \ parser ->
    let l:ts = get_token_stream parser
    in
    if null ts
        then error "advance past EOF"
        else
            ( l
            , parser
             { get_token_stream = ts
             , last_token = Just l
             }
            )

consume :: TokenPredicate -> ParseFunM (Located Tokens.Token)
consume predicate =
    peek_matches predicate >>= \case
        True -> advance >>= return . Just
        False -> return Nothing

consume_or_error :: ParseErrorSpanPending -> TokenPredicate -> ParseFunMWE (Located Tokens.Token)
consume_or_error err predicate =
    peek_matches predicate >>= \case
        True -> advance >>= return . JustWithError
        False -> peek >>= \ (Located sp _) -> add_error_and_nothing (err sp)

synchronize :: SynchronizationPredicate -> ParseFun ()
synchronize sync_predicate = advance >> go
    where
        go =
            parser_matches (sync_predicate `predicate_or` is_at_end) >>= \case
                True -> return ()
                False -> advance >> go
-- combinators {{{1
braced, indented, blocked :: ParseFunMWE a -> ParseFunMWE (Span, a)
(braced, indented) = (surround_with Tokens.OBrace Tokens.CBrace, surround_with Tokens.Indent Tokens.Dedent)
    where
        surround_with :: Tokens.Token -> Tokens.Token -> ParseFunMWE a -> ParseFunMWE (Span, a)
        surround_with open close pf =
            consume_or_error (Expected $ Tokens.format_token open) (is_tt open) >>=??> \ (Located open_sp _) ->
            pf >>=??> \ inside ->
            consume_or_error (Expected $ Tokens.format_token close) (is_tt close) >>=??> \ (Located close_sp _) ->
            return (JustWithError (open_sp `join_span` close_sp, inside))

blocked pf =
    peek_span >>= \ next_span ->
    peek_match
        [ (is_tt Tokens.OBrace, braced pf)
        , (is_tt Tokens.Indent, indented pf)
        ]
        (add_error_and_nothing (Expected "block" next_span))

thing_list_no_separator :: StopPredicate -> SynchronizationPredicate -> ParseFunMWE a -> ParseFun [a]
thing_list_no_separator stop_predicate sync_predicate pf = go []
    where
        go things =
            parser_matches (stop_predicate `predicate_or` is_at_end) >>= \case
                True -> return things

                False ->
                    pf >>= \case
                        JustWithError thing ->
                            go $ things ++ [thing]
                        NothingWithError _ ->
                            synchronize sync_predicate >>
                            go things

-- parse_decl {{{1
parse_decl :: ParseFunMWE LDDecl
parse_decl =
    peek_span >>= \ next_span ->
    peek_match
        [ (is_tt Tokens.Fun,
            advance >>= parse_fun >>=??> \ fun@(Located fun_sp _) ->
            return (JustWithError $ Located fun_sp (DDecl'Fun fun))
          )
        ]
        (add_error_and_nothing (Expected "a declaration" next_span))
-- parse_fun {{{2
parse_fun :: Located Tokens.Token -> ParseFunMWE LSFunDecl
parse_fun = undefined
-- parse_module {{{1
parse_module :: ParseFun LDModule
parse_module =
    (thing_list_no_separator
        (const False) -- stop predicate: will never stop until EOF
        (peek_matches_pred $ is_tt Tokens.Fun) -- synchronization predicate: will stop before 'fun'
        parse_decl
    ) >>= \ decl_list ->
    peek >>= \ (Located eof_span _) ->
    return (Located (span_from_list decl_list eof_span) (DModule' decl_list))
-- parsing entry point{{{1
parse_from_toks :: [Located Tokens.Token] -> ([ParseError], LDModule)
parse_from_toks toks =
    let parser = Parser [] toks Nothing
        (res, Parser errs _ _) = runState parse_module parser
    in (errs, res)
