{-# LANGUAGE GADTs #-}

module Test
    ( TestSuite(..)
    , Test(..)
    , run_test_suite
    ) where

-- inspired by RSpec

import Control.Monad(when)

import System.Exit(exitFailure)

data TestSuite = TestSuite [Test]
data Test
    = Describe String [Test]
    | When String [Test]
    | ItCan String (IO TestResult)

data TestResult = Pass | Fail String | Untested

run_test_suite :: TestSuite -> IO ()
run_test_suite (TestSuite tests) =
    run_test_list 0 tests >>= \ results ->
    let count b = length $ filter b results

        match_pass = \case
            Pass -> True
            _ -> False

        match_fail = \case
            Fail _ -> True
            _ -> False

        match_untested = \case
            Untested -> True
            _ -> False

        num_pass = count match_pass
        num_fail = count match_fail
        num_untested = count match_untested

    in putStrLn (show num_pass ++ " tests passed") >>
    putStrLn (show num_fail ++ " tests failed") >>
    putStrLn (show num_untested ++ " tests untested") >>
    when (num_fail > 0) exitFailure

run_test :: Int -> Test -> IO [TestResult]

run_test indent_amt (Describe name test_list) =
    indent_put_str indent_amt (name ++ ":\n") >>
    run_test_list (indent_amt + 4) test_list

run_test indent_amt (When context test_list) =
    indent_put_str indent_amt ("when " ++ context ++ ":\n") >>
    run_test_list (indent_amt + 4) test_list

run_test indent_amt (ItCan action result) =
    indent_put_str indent_amt "it " >>
    result >>= \ result' ->
    (case result' of
        Pass -> putStr ("can " ++ action)
        Fail msg -> putStr ("cannot " ++ action ++ ": " ++ msg)
        Untested -> putStr ("maybe can " ++ action)
    ) >>
    return [result']

indent :: Int -> String
indent = (`replicate` ' ')

indent_put_str :: Int -> String -> IO ()
indent_put_str amt = putStr . (indent amt ++)

run_test_list :: Int -> [Test] -> IO [TestResult]
run_test_list indent_amt test_list = concat <$> mapM (run_test indent_amt) test_list
