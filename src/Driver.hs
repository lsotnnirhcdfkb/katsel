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

import qualified Colors

import System.IO(hPutStr, hPutStrLn, stderr)
import Control.Exception(SomeException, try, evaluate, displayException)

import Data.Maybe(catMaybes)
import Data.List(intersperse)

import qualified System.Console.ANSI as ANSI

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

add_errors :: [Message.SimpleDiag] -> ErrorAccumulated ()
add_errors = ErrorAcc ()

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

lower_ast_stage :: AST.LDModule -> ErrorAccumulated (IR.Module, IR.TyCtx)
lower_ast_stage mod_ast =
    let (ir_mod, tyctx, errs) = IR.build_ir mod_ast
    in add_errors (map Message.to_diagnostic errs) >> return (ir_mod, tyctx)

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

        put_ending_line =
            let find_diag diag_ty (Message.SimpleDiag ty _ _ _ _) = ty == diag_ty
                make_msg diag_ty kind sgr =
                    let amount = length $ filter (find_diag diag_ty) final_errs
                    in if amount > 0
                        then Just $
                            hPutStr stderr (show amount ++ " ") >>
                            ANSI.hSetSGR stderr sgr >>
                            hPutStr stderr (kind ++ (if amount > 1 then "s" else "")) >>
                            ANSI.hSetSGR stderr [] >>
                            hPutStr stderr " emitted"
                        else Nothing

                error_msg = make_msg Message.Error "error" Colors.error_sgr
                warning_msg = make_msg Message.Warning "warning" Colors.warning_sgr
                dbg_msg = make_msg Message.DebugMessage "debug message" Colors.dbgmsg_sgr

                msgs :: [IO ()]
                msgs = catMaybes [error_msg, warning_msg, dbg_msg]
            in if length msgs > 0
                then sequence_ (intersperse (hPutStr stderr ", ") msgs) >> hPutStrLn stderr ""
                else return ()

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
        seq final_output put_errs >>
        put_ending_line
    )
