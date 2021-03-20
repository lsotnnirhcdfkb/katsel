module Driver
    ( Backend(..)
    , OutputFormat(..)
    , run
    ) where

import File
import Location

import qualified Message
import qualified Lex
import qualified Parse
import qualified AST

import System.IO(hPutStr, stderr)
import Control.Exception(try, SomeException, evaluate)

data Backend = CBackend

data OutputFormat = Lexed | Parsed | KatselIR | BackendCode Backend

type Stage a b = (a -> (b, [Message.SimpleDiag]))

joinStages :: Stage a b -> Stage b c -> Stage a c
joinStages stage1fn stage2fn = \ input ->
    let (stage1res, stage1errs) = stage1fn input
        (stage2res, stage2errs) = stage2fn stage1res
    in (stage2res, stage1errs ++ stage2errs)

lexStage :: Stage File [Located Lex.Token]
lexStage contents =
    let lexed = Lex.lex contents
    in ([x | Right x <- lexed], [Message.toDiagnostic x | Left x <- lexed])

parseStage :: Stage [Located Lex.Token] (Maybe AST.DCU)
parseStage toks =
    let (res, err) = Parse.parse toks
    in (res, [Message.toDiagnostic err])

run :: String -> IO ()
run filename =
    openFile filename >>= \ file ->
    let totalStages = lexStage `joinStages` parseStage
        (finalOutput, finalErrs) = totalStages file
        putErrs = hPutStr stderr $ concat $ map Message.report finalErrs
    in
    (try putErrs :: IO (Either SomeException ())) >>= \ei ->
    case ei of
        Right () -> return ()
        Left err ->
            -- TODO: make this a diagnostic, print correctly and with color
            hPutStr stderr ("\n!!! the compiler is broken! caught internal error: \n" ++ (unlines $ map ("  > " ++) $ lines $ show err)) >>
            (evaluate $ error "stop after catching internal error")
