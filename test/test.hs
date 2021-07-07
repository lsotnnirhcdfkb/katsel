module Main where

import Test

import Tokens (tests)
import AST.Parsing (tests)

main :: IO ()
main = run_test_suite $ TestSuite [Tokens.tests, AST.Parsing.tests]
