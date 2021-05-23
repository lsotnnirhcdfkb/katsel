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

import System.IO(hPutStr, hFlush, stdout, stderr)
import Control.Exception(SomeException, try, evaluate, displayException)

import Data.Maybe(catMaybes)
import Data.List(intersperse)

import qualified System.Console.ANSI as ANSI

-- data Backend = CBackend

-- data OutputFormat = Lexed | Parsed | KatselIR | BackendCode Backend

-- ErrorAccumulated {{{1
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
-- stages {{{1
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

        Left errs ->
            add_errors errs >>
            return Nothing

lower_ast_stage :: AST.LDModule -> ErrorAccumulated (IR.Module, IR.IRCtx)
lower_ast_stage mod_ast =
    let (ir_mod, irctx, errs) = IR.build_ir mod_ast
    in add_errors (map (\err -> Message.to_diagnostic (err, irctx)) errs) >> return (ir_mod, irctx)
-- compile {{{1
compile :: Int -> Int -> String -> IO ()
compile num max_num filename =
    do_try $

    put_status_start num max_num filename >> hFlush stdout >>

    open_file filename >>= \ file ->
    let (ErrorAcc result diagnostics) =
            lex_stage file >>=
            parse_stage >>= \ mast ->
            case mast of
                Just ast -> Just <$> lower_ast_stage ast
                Nothing -> return Nothing

    in (if compilation_failed diagnostics
        then put_failed
        else put_success
    ) >>
    (if not $ null diagnostics
        then
            putStr ", " >>
            put_counts diagnostics
        else return ()
    ) >>
    hPutStr stderr (concatMap Message.report diagnostics)
-- compile helpers {{{1
amount_of_diag :: Message.SimpleDiagType -> [Message.SimpleDiag] -> Int
amount_of_diag diag_ty errs =
    let find_diag t (Message.SimpleDiag t' _ _ _ _) = t == t'
    in length $ filter (find_diag diag_ty) errs
compilation_failed :: [Message.SimpleDiag] -> Bool
compilation_failed = (>0) . amount_of_diag Message.Error

-- TODO: do not catch user interrupt
do_try :: IO () -> IO ()
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

put_status_start :: Int -> Int -> String -> IO ()
put_status_start num max_num filename =
    putStr ("[" ++ show num ++ "/" ++ show max_num ++ "] compiling ") >>
    ANSI.setSGR Colors.file_path_sgr >>
    putStr filename >>
    ANSI.setSGR [] >>
    putStr ": "

put_success, put_failed :: IO ()
put_success =
    ANSI.setSGR Colors.success_sgr >>
    putStr "success" >>
    ANSI.setSGR []
put_failed =
    ANSI.setSGR Colors.failure_sgr >>
    putStr "failed" >>
    ANSI.setSGR []

put_counts :: [Message.SimpleDiag] -> IO ()
put_counts diagnostics =
    let make_msg diag_ty kind sgr =
            let amount = amount_of_diag diag_ty diagnostics
            in if amount > 0
                then Just $
                    ANSI.setSGR sgr >>
                    putStr (show amount ++ " ") >>
                    putStr (kind ++ (if amount > 1 then "s" else "")) >>
                    ANSI.setSGR [] >>
                    putStr " emitted"
                else Nothing

        error_msg = make_msg Message.Error "error" Colors.error_sgr
        warning_msg = make_msg Message.Warning "warning" Colors.warning_sgr
        dbg_msg = make_msg Message.DebugMessage "debug message" Colors.dbgmsg_sgr

        msgs = catMaybes [error_msg, warning_msg, dbg_msg]
    in if length msgs > 0
        then sequence_ (intersperse (putStr ", ") msgs) >> putStrLn ""
        else return ()
