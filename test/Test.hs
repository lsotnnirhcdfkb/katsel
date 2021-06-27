{-# LANGUAGE GADTs #-}

module Test
    ( TestSuite(..)
    , Test(..)
    , run_test_suite
    ) where

-- inspired by RSpec

import Control.Monad(unless)

import System.Exit(exitFailure)

data TestSuite = TestSuite [Test]
data Test
    = Describe String [Test]
    | When String [Test]
    | ItCan String (IO TestResult)

data TestResult = Pass | Fail String | Untested

run_test_suite :: TestSuite -> IO ()
run_test_suite (TestSuite tests) =
    run_test_list 0 tests >>= \ success ->
    unless success exitFailure

run_test :: Int -> Test -> IO Bool

run_test indent_amt (Describe name test_list) =
    indent_put_str indent_amt (name ++ ":\n") >>
    run_test_list (indent_amt + 4) test_list

run_test indent_amt (When context test_list) =
    indent_put_str indent_amt ("when " ++ context ++ ":\n") >>
    run_test_list (indent_amt + 4) test_list

run_test indent_amt (ItCan action result) =
    indent_put_str indent_amt "it " >>
    result >>= \case
        Pass -> putStr ("can " ++ action) >> return True
        Fail msg -> putStr ("cannot " ++ action ++ ": " ++ msg) >> return False
        Untested -> putStr ("maybe can " ++ action) >> return True

indent :: Int -> String
indent = (`replicate` ' ')

indent_put_str :: Int -> String -> IO ()
indent_put_str amt = putStr . (indent amt ++)

run_test_list :: Int -> [Test] -> IO Bool
run_test_list indent_amt test_list = and <$> mapM (run_test indent_amt) test_list
