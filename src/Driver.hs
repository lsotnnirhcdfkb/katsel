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

data TokenDebugMessage = TokenDebugMessage (Either Lex.LexError (Located Lex.Token))
instance Message.ToDiagnostic TokenDebugMessage where
    toDiagnostic (TokenDebugMessage (Right (Located sp tok))) =
        Message.SimpleDiag Message.DebugMessage (Just sp) Nothing Nothing [
            Message.SimpleText $ show tok
        ]

    toDiagnostic (TokenDebugMessage (Left err)) = Message.toDiagnostic err
