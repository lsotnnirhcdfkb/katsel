module Main where

import System.Environment(getArgs, getProgName)
import System.IO(stderr, hPutStrLn)

import qualified Driver

main :: IO ()
main =
    getArgs >>= \ args ->
    getProgName >>= \ prog_name ->
    case args of
        [] -> usage_message prog_name
        filenames -> sequence_ $ map compile $ zip ([1..] :: [Int]) filenames
    where
        usage_message prog_name = hPutStrLn stderr $ "usage: " ++ prog_name ++ " FILENAMES..."

        compile (num, filename) = putStrLn ("=> [" ++ show num ++ "] compiling " ++ filename) >> Driver.compile filename
