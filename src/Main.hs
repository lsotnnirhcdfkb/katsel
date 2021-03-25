module Main where

import System.Environment(getArgs, getProgName)
import System.IO(stderr, hPutStrLn)

import qualified Driver

main :: IO ()
main =
    getArgs >>= \ args ->
    getProgName >>= \ progName ->
    case args of
        [] -> usageMessage progName
        filename:[] -> Driver.compile filename
        _ -> usageMessage progName
    where
        usageMessage progName = hPutStrLn stderr $ "usage: " ++ progName ++ " <filename>"
