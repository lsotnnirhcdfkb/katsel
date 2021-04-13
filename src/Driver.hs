module Driver
    -- TODO: this
    -- ( Backend(..)
    -- , OutputFormat(..)
    ( compile
    ) where

import File
import Location

import qualified Message
import qualified Message.PrettyPrint as PPrint

import qualified Tokens
import qualified AST
import qualified IR

import System.IO(hPutStr, stderr)
import Control.Exception(SomeException, try, evaluate, displayException)

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
addErrors = ErrorAcc ()

lexStage :: File -> ErrorAccumulated [Located Tokens.Token]
lexStage contents =
    let lexed = Tokens.lex contents
        errs = [Message.toDiagnostic x | Left x <- lexed]
        toks = [x | Right x <- lexed]
    in addErrors errs >> return toks

parseStage :: [Located Tokens.Token] -> ErrorAccumulated (Maybe AST.LDModule)
parseStage toks =
    case AST.parse toks of
        Right result ->
            return $ Just result

        Left err ->
            addErrors [Message.toDiagnostic err] >>
            return Nothing

lowerASTStage :: AST.LDModule -> ErrorAccumulated IR.Module
lowerASTStage = return . IR.buildIR

compile :: String -> IO ()
compile filename =
    openFile filename >>= \ file ->
    let (ErrorAcc finalOutput finalErrs) =
            lexStage file >>=
            parseStage >>= \ mast ->
            case mast of
                Just ast -> lowerASTStage ast >>= \ lowered -> return $ Just lowered
                Nothing -> return Nothing

        putErrs = hPutStr stderr $ concatMap Message.report finalErrs

        -- TODO: do not catch user interrupt
        doTry x = (try x :: IO (Either SomeException ())) >>= \ ei ->
            case ei of
                Right () -> return ()
                Left err ->
                    hPutStr stderr ("\n" ++
                        Message.report (Message.SimpleDiag Message.InternalError Nothing Nothing Nothing
                            [ Message.SimpleText "the compiler is broken! caught internal error:"
                            , Message.SimpleMultilineText $ unlines $ map ("> " ++) $ lines $ displayException err
                            ]
                    )) >>
                    evaluate (error "stop after catching internal error")

    in doTry (
        seq finalOutput putErrs
    )
