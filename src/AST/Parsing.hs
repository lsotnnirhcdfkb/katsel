module AST.Parsing
    ( parse_from_toks
    , ParseError
    ) where

import Location

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

-- commbinators {{{1
-- parsing {{{1
parse_from_toks :: [Located Tokens.Token] -> ([ParseError], LDModule)
parse_from_toks = undefined
