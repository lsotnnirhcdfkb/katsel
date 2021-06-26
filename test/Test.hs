{-# LANGUAGE GADTs #-}

module Test
    ( TestSuite(..)
    , Test(..)
    , run_test_suite
    ) where

-- inspired by RSpec

import System.Exit(exitFailure)

data TestSuite = TestSuite [Test]
data Test
    = Describe String [Test]
    | When String [Test]
    | ItCan String Bool

run_test_suite :: TestSuite -> IO ()
run_test_suite (TestSuite tests) =
    run_test_list 0 tests >>= \ success ->
    if success
        then return ()
        else exitFailure

run_test :: Int -> Test -> IO Bool

run_test indent_amt (Describe name test_list) =
    indent_put_str indent_amt (name ++ ":\n") >>
    run_test_list (indent_amt + 4) test_list

run_test indent_amt (When context test_list) =
    indent_put_str indent_amt ("when " ++ context ++ ":\n") >>
    run_test_list (indent_amt + 4) test_list

run_test indent_amt (ItCan action result) =
    indent_put_str indent_amt "it " >>
    (if result
        then putStr "can "
        else putStr "cannot "
    ) >>
    putStrLn action >>
    return result


indent :: Int -> String
indent = (`replicate` ' ')

indent_put_str :: Int -> String -> IO ()
indent_put_str amt = putStr . (indent amt ++)

run_test_list :: Int -> [Test] -> IO Bool
run_test_list indent_amt test_list = all id <$> mapM (run_test indent_amt) test_list
