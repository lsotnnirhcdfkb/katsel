module Main where

import Test

import Tokens(tests)

main :: IO ()
main = run_test_suite $ TestSuite [tests]
