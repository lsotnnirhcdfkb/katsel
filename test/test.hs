module Main where

import Test

import Tokens (tests)
import Mangle (tests)
import Message.PrettyPrint (tests)

main :: IO ()
main = run_test_suite $ TestSuite [Tokens.tests, Mangle.tests, Message.PrettyPrint.tests]
