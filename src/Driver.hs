module Driver
    ( Backend(..)
    , OutputFormat(..)
    , run
    ) where

import File
import Location

import qualified Message
import qualified Lex

data Backend = CBackend

data OutputFormat = Lexed
                  | Parsed
                  | KatselIR
                  | BackendCode Backend

run :: String -> IO ()
run filename =
    openFile filename >>= \ file ->
    putStrLn $ concat $ map (Message.report . TokenDebugMessage) $ Lex.lex file

data TokenDebugMessage = TokenDebugMessage (Located Lex.Token)
instance Message.ToDiagnostic TokenDebugMessage where
    toDiagnostic (TokenDebugMessage (Located (Span loc _) tok)) =
        Message.SimpleDiag (Message.DebugMessage) loc (Message.DiagCode "DXXXX") "token" [
            Message.SimpleText $ show tok
        ]
