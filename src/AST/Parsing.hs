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

import Data.List (foldl', nub)
import Data.Data (toConstr, Data)
import Data.Maybe (fromMaybe)

import Control.Monad.State.Lazy (State, state, runState)

-- ParseError {{{1
data ParseError

instance Message.ToDiagnostic ParseError where

-- Parser {{{1
data Parser
    = Parser
      { get_parse_errors :: [ParseError]
      , get_token_stream :: [Tokens.Token]
      , last_token :: Maybe Tokens.Token
      }
-- type synonyms {{{1
type ParseFun = State Parser
type ParseFunM a = ParseFun (Maybe a)

type ParserPredicate = Parser -> Bool
type SynchronizationPredicate = ParserPredicate
type StopPredicate = ParserPredicate
-- combinators {{{1
braced, indented, blocked :: ParseFunM a -> ParseFunM a
(braced, indented) = (surround_with Tokens.OBrace Tokens.CBrace, surround_with Tokens.Indent Tokens.Dedent)
    where
        surround_with open close pf = _
blocked = _

thing_list_no_separator :: StopPredicate -> SynchronizationPredicate -> ParseFunM a -> ParseFun [a]
thing_list_no_separator = _
-- parsing {{{1
parse_from_toks :: [Located Tokens.Token] -> ([ParseError], LDModule)
parse_from_toks = undefined
