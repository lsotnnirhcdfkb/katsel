module Main where

import System.Environment (getArgs, getProgName)
import System.IO (stderr, hPutStrLn)

import Driver qualified

main :: IO ()
main =
    getArgs >>= \ args ->
    getProgName >>= \ prog_name ->
    case args of
        [] -> usage_message prog_name
        filenames ->
            let maxnum = length filenames
                compile (num, filename) = Driver.compile num maxnum filename
            in mapM_ compile $ zip ([1..] :: [Int]) filenames
    where
        usage_message prog_name = hPutStrLn stderr $ "usage: " ++ prog_name ++ " FILENAMES..."

