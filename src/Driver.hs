module Driver
    -- TODO: this
    -- ( Backend(..)
    -- , OutputFormat(..)
    ( compile
    ) where

import File
import Location

import qualified Message

import qualified Tokens
import qualified AST
import qualified IR

import ErrorAcc

import System.IO(hPutStr, stderr)
import Control.Exception(SomeException, try, evaluate, displayException)

-- data Backend = CBackend

-- data OutputFormat = Lexed | Parsed | KatselIR | BackendCode Backend

lex_stage :: File -> ErrorAccumulated [Located Tokens.Token]
lex_stage contents =
    let lexed = Tokens.lex contents
        errs = [Message.to_diagnostic x | Left x <- lexed]
        toks = [x | Right x <- lexed]
    in add_errors errs >> return toks

parse_stage :: [Located Tokens.Token] -> ErrorAccumulated (Maybe AST.LDModule)
parse_stage toks =
    case AST.parse toks of
        Right result ->
            return $ Just result

        Left err ->
            add_errors [Message.to_diagnostic err] >>
            return Nothing

lower_ast_stage :: AST.LDModule -> ErrorAccumulated IR.Module
lower_ast_stage = return . IR.build_ir

compile :: String -> IO ()
compile filename =
    open_file filename >>= \ file ->
    let (ErrorAcc final_output final_errs) =
            lex_stage file >>=
            parse_stage >>= \ mast ->
            case mast of
                Just ast -> Just <$> lower_ast_stage ast
                Nothing -> return Nothing

        put_errs = hPutStr stderr $ concatMap Message.report final_errs

        -- TODO: do not catch user interrupt
        do_try x = (try x :: IO (Either SomeException ())) >>= \ ei ->
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

    in do_try (
        seq final_output put_errs
    )
