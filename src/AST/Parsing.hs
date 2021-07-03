module AST.Parsing
    ( parse_from_toks
    , ParseError
    ) where

import Location
import MonadUtils

import AST.Datatypes

import qualified Tokens
import qualified Message
import qualified Message.Underlines as MsgUnds

import Data.Data (toConstr, Data)

import Control.Monad.State.Lazy (State, state, runState)

-- ParseError {{{1
data ParseError

instance Message.ToDiagnostic ParseError where

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

type ParserPredicate = Parser -> Bool
type SynchronizationPredicate = ParserPredicate
type StopPredicate = ParserPredicate

type TokenPredicate = Located Tokens.Token -> Bool
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
peek_matches_pred pred = pred . head . get_token_stream
-- basic functions {{{1
peek :: ParseFun (Located Tokens.Token)
peek = state $ \ parser -> (head $ get_token_stream parser, parser)

peek_matches :: TokenPredicate -> ParseFun Bool
peek_matches predicate = peek >>= return . predicate

parser_matches :: ParserPredicate -> ParseFun Bool
parser_matches predicate = state $ \ parser -> (predicate parser, parser)

advance :: ParseFun ()
advance = state $ \ parser ->
    let l:ts = get_token_stream parser
    in
    if null ts
        then error "advance past EOF"
        else
            ( ()
            , parser
             { get_token_stream = ts
             , last_token = Just l
             }
            )

consume :: TokenPredicate -> ParseFunM (Located Tokens.Token)
consume predicate =
    peek_matches predicate >>= \case
        True -> peek >>= \ tok -> advance >> return (Just tok)
        False -> return Nothing

add_err :: ParseError -> ParseFun ()
add_err err = state $ \ parser -> ((), parser { get_parse_errors = err : get_parse_errors parser })

synchronize :: SynchronizationPredicate -> ParseFun ()
synchronize sync_predicate = advance >> go
    where
        go =
            parser_matches (sync_predicate `predicate_or` is_at_end) >>= \case
                True -> return ()
                False -> advance >> go
-- combinators {{{1
braced, indented, blocked :: ParseFunM a -> ParseFunM (Span, a)
(braced, indented) = (surround_with Tokens.OBrace Tokens.CBrace, surround_with Tokens.Indent Tokens.Dedent)
    where
        surround_with :: Tokens.Token -> Tokens.Token -> ParseFunM a -> ParseFunM (Span, a)
        surround_with open close pf =
            consume (is_tt open) >>=? return Nothing $ \ (Located open_sp _) ->
            pf >>=? return Nothing $ \ inside ->
            consume (is_tt close) >>=? return Nothing $ \ (Located close_sp _) ->
            return (Just (open_sp `join_span` close_sp, inside))

blocked pf = peek_matches (is_tt Tokens.OBrace) >>= \case
        True -> braced pf
        False -> blocked pf

thing_list_no_separator :: StopPredicate -> SynchronizationPredicate -> ParseFunM a -> ParseFun [a]
thing_list_no_separator stop_predicate sync_predicate parse_fun = go []
    where
        go things =
            parser_matches (stop_predicate `predicate_or` is_at_end) >>= \case
                True -> return things

                False ->
                    parse_fun >>= \case
                        Just thing ->
                            go $ things ++ [thing]
                        Nothing ->
                            synchronize sync_predicate >>
                            go things

-- parse_decl {{{1
parse_decl :: ParseFunM LDDecl
parse_decl =
    parse_fun >>=? return Nothing $ \ fun@(Located fun_sp _) ->
    return (Just $ Located fun_sp (DDecl'Fun fun))
-- parse_fun {{{2
parse_fun :: ParseFunM LSFunDecl
parse_fun = _
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
