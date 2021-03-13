module Driver
    ( Backend(..)
    , OutputFormat(..)
    , run
    ) where

import File

import Data.List(foldl')

import qualified Lex

data Backend = CBackend

data OutputFormat = Lexed
                  | Parsed
                  | KatselIR
                  | BackendCode Backend

run :: String -> IO ()
run filename =
    openFile filename >>= \ file ->
    putStrLn $ foldl' (\ acc new -> acc ++ " " ++ new) "" $ map show $ Lex.lex file
