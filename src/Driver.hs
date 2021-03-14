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

parseStage :: Stage [Located Lex.Token] Parse.DCU
parseStage toks =
    let (parsed, errs) = Parse.parse toks
    in (parsed, map Message.toDiagnostic errs)

run :: String -> IO ()
run filename =
    openFile filename >>= \ file ->
    let totalStages = lexStage `joinStages` parseStage
        (finalOutput, finalErrs) = totalStages file
    in
    (putStr $ concat $ map Message.report finalErrs)
