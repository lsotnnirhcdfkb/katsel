module Driver
    ( Backend(..)
    , OutputFormat(..)
    , run
    ) where

import File

import qualified Lex

data Backend = CBackend

data OutputFormat = Lexed
                  | Parsed
                  | KatselIR
                  | BackendCode Backend

run :: String -> IO ()
run filename =
    openFile filename >>= \ file ->
    putStrLn $ concat $ map show $ Lex.lex file
