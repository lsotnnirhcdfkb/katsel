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
        [filename] -> Driver.compile filename
        _ -> usage_message prog_name
    where
        usage_message prog_name = hPutStrLn stderr $ "usage: " ++ prog_name ++ " <filename>"
