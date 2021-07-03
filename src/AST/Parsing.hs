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
is_at_end = is_tt Tokens.EOF . head . get_token_stream
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
    in ( ()
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
            (,) <$> parser_matches sync_predicate <*> parser_matches is_at_end >>= \case
                (False, False) -> advance >> go
                _ -> return ()
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
            (,) <$> parser_matches stop_predicate <*> parser_matches is_at_end >>= \case
                (False, False) ->
                    parse_fun >>= \case
                        Just thing ->
                            go $ things ++ [thing]
                        Nothing ->
                            synchronize sync_predicate >>
                            go things

                _ -> return things
-- parsing {{{1
parse_module :: ParseFun LDModule
parse_module = undefined

parse_from_toks :: [Located Tokens.Token] -> ([ParseError], LDModule)
parse_from_toks toks =
    let parser = Parser [] toks Nothing
        (res, Parser errs _ _) = runState parse_module parser
    in (errs, res)
