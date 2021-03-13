module Driver where

data Backend = CBackend

data OutputFormat = Lexed
                  | Parsed
                  | KatselIR
                  | BackendCode Backend

run :: String -> IO ()
run fn = putStrLn $ "compile " ++ fn
