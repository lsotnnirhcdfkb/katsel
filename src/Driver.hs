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

data OutputFormat = Lexed | Parsed | KatselIR | BackendCode Backend

joinStages :: (a -> (b, [Message.SimpleDiag])) -> (b -> (c, [Message.SimpleDiag])) -> (a -> (c, [Message.SimpleDiag]))
joinStages stage1fn stage2fn = \ input ->
    let (stage1res, stage1errs) = stage1fn input
        (stage2res, stage2errs) = stage2fn stage1res
    in (stage2res, stage1errs ++ stage2errs)

lexStage :: File -> ([Located Lex.Token], [Message.SimpleDiag])
lexStage contents =
    let lexed = Lex.lex contents
    in ([x | Right x <- lexed], [Message.toDiagnostic x | Left x <- lexed])

run :: String -> IO ()
run filename =
    openFile filename >>= \ file ->
    let totalStages = lexStage
        (finalOutput, finalErrs) = totalStages file
    in
    (putStr $ concat $ map Message.report finalErrs) >>
    (putStrLn $ show $ finalOutput)
