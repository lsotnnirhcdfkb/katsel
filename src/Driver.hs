module Driver
    -- TODO: this
    -- ( Backend(..)
    -- , OutputFormat(..)
    ( compile
    ) where

import File
import Location

import qualified Message
import qualified Lex
import qualified Parse
import qualified AST

import System.IO(hPutStr, stderr)
import Control.Exception(try, SomeException, evaluate)

-- data Backend = CBackend

-- data OutputFormat = Lexed | Parsed | KatselIR | BackendCode Backend

data ErrorAccumulated r = ErrorAcc r [Message.SimpleDiag]

instance Functor ErrorAccumulated where
    fmap f (ErrorAcc v errs) = ErrorAcc (f v) errs

instance Applicative ErrorAccumulated where
    pure x = ErrorAcc x []

    eaf <*> eav =
        eaf >>= \ f ->
        eav >>= \ v ->
        pure $ f v

instance Monad ErrorAccumulated where
    (ErrorAcc aval aerrs) >>= f =
        let (ErrorAcc bval berrs) = f aval
        in ErrorAcc bval $ aerrs ++ berrs

addErrors :: [Message.SimpleDiag] -> ErrorAccumulated ()
addErrors errs = ErrorAcc () errs

lexStage :: File -> ErrorAccumulated [Located Lex.Token]
lexStage contents =
    let lexed = Lex.lex contents
        errs = [Message.toDiagnostic x | Left x <- lexed]
        toks = [x | Right x <- lexed]
    in addErrors errs >> return toks

parseStage :: [Located Lex.Token] -> ErrorAccumulated (Maybe AST.LDCU)
parseStage toks =
    let (res, err) = Parse.parse toks
    in addErrors [Message.toDiagnostic err] >> return res

compile :: String -> IO ()
compile filename =
    openFile filename >>= \ file ->
    let final = lexStage file >>= parseStage
        (ErrorAcc finalOutput finalErrs) = final

        putErrs = hPutStr stderr $ concat $ map Message.report finalErrs
    in (try putErrs :: IO (Either SomeException ())) >>= \ei ->
    case ei of
        Right () -> return ()
        Left err ->
            -- TODO: make this a diagnostic, print correctly and with color
            hPutStr stderr ("\n!!! the compiler is broken! caught internal error: \n" ++ (unlines $ map ("  > " ++) $ lines $ show err)) >>
            (evaluate $ error "stop after catching internal error")
