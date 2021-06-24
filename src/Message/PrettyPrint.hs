{-# LANGUAGE TemplateHaskell #-}

module Message.PrettyPrint
    ( pprint_mod
    , pprint_decl
    , pprint_expr
    , pprint_param
    , pprint_path
    , pprint_stmt
    , pprint_type
    -- , pprint_impl_entity

    , pprint_ldecl
    , pprint_lexpr
    , pprint_lmod
    , pprint_lparam
    , pprint_lpath
    , pprint_lstmt
    , pprint_ltype
    -- , pprint_limpl_entity
    ) where

import Control.Monad.State.Lazy(State, state, execState)

import qualified AST
import Location

import Data.List(foldl', intersperse)

import Message.PrettyPrintTH

-- NOTE: NOT an automated code formatter!! just a pretty printer to print things in error messages!

-- PPrintSegment {{{1
data PPrintSegment
    = Literal String
    | Indent
    | Dedent
    | Newline
    | Boom
    | Semi
-- stringify_segments {{{1
newtype IndentAmt = IndentAmt { indent_amt :: Int }

process_two_segments :: PPrintSegment -> PPrintSegment -> [PPrintSegment]
process_two_segments Indent Indent = [Indent, Boom]
process_two_segments Indent Dedent = [Indent, Boom]
process_two_segments Indent Newline = [Indent, Semi]
process_two_segments Dedent Indent = [Dedent, Boom]
process_two_segments Dedent Newline = [Dedent, Semi]
process_two_segments Newline Indent = [Semi]
process_two_segments x _ = [x]

stringify_segments :: [PPrintSegment] -> String
stringify_segments segments = stringify $ process segments
    where
        stringify s = snd $ foldl' segment_to_str (IndentAmt 0, "") s

        process :: [PPrintSegment] -> [PPrintSegment]
        process (x : more@(y:_)) = process_two_segments x y ++ process more
        process [x] = [x]
        process [] = []

safe_last :: [a] -> Maybe a
safe_last [] = Nothing
safe_last l = Just $ last l

add_to_acc :: String -> IndentAmt -> String -> String
add_to_acc acc (IndentAmt indamt) adding =
    acc ++
    (if safe_last acc == Just '\n'
    then replicate (indamt * 4) ' '
    else "") ++
    adding

segment_to_str :: (IndentAmt, String) -> PPrintSegment -> (IndentAmt, String)
segment_to_str (indamt, acc) (Literal s) = (indamt, add_to_acc acc indamt s)
segment_to_str (indamt, acc) Indent = (IndentAmt $ indent_amt indamt + 1, add_to_acc acc indamt "\n")
segment_to_str (indamt, acc) Dedent = (IndentAmt $ indent_amt indamt - 1, acc)
segment_to_str (indamt, acc) Newline = (indamt, add_to_acc acc indamt "\n")
segment_to_str (indamt, acc) Semi = (indamt, add_to_acc acc indamt ";\n")
segment_to_str (indamt, acc) Boom = (indamt, add_to_acc acc indamt "boom\n")
-- run pprint state helper {{{1
state_to_fun :: (a -> State [PPrintSegment] ()) -> a -> String
state_to_fun statefun thing = stringify_segments $ execState (statefun thing) []
-- put, putch, indent, dedent, and putnl {{{1
putch :: Char -> State [PPrintSegment] ()
putch '\n' = state $ \ segments -> ((), segments ++ [Newline])
putch ch = state $ \ segments -> ((), segments ++ [Literal [ch]])

put :: String -> State [PPrintSegment] ()
put str = foldl (>>) (return ()) $ map putch str

indent :: State [PPrintSegment] ()
indent = state $ \ segments -> ((), segments ++ [Indent])
dedent :: State [PPrintSegment] ()
dedent = state $ \ segments -> ((), segments ++ [Dedent])

putnl :: State [PPrintSegment] ()
putnl = state $ \ segments -> ((), segments ++ [Newline])
-- helper functions {{{1
maybe_do :: (a -> State [PPrintSegment] ()) -> Maybe a -> State [PPrintSegment] ()
maybe_do = maybe (return ())

pprint_list :: (a -> State [PPrintSegment] ()) -> [a] -> State [PPrintSegment] ()
pprint_list pprintfun things = foldl' (>>) (return ()) $ map pprintfun things

pprint_list_delim :: (a -> State [PPrintSegment] ()) -> State [PPrintSegment] () -> [a] -> State [PPrintSegment] ()
pprint_list_delim pprintfun delim things = foldl' (>>) (return ()) $ intersperse delim $ map pprintfun things
-- AST.DModule {{{1
pprint_mod_s :: AST.DModule -> State [PPrintSegment] ()
pprint_mod_s (AST.DModule' decls) = pprint_list (pprint_decl_s . unlocate) decls
-- AST.DDecl {{{1
pprint_decl_s :: AST.DDecl -> State [PPrintSegment] ()
pprint_decl_s (AST.DDecl'Fun sf) = pprint_fun_decl_s $ unlocate sf
{-
pprint_decl_s (AST.DDecl'Impl ty entities) =
    put "impl " >> pprint_type_s (unlocate ty) >> indent >>
    pprint_list (pprint_impl_entity_s . unlocate) entities >>
    dedent
-}
-- AST.DImplEntity {{{1
{-
pprint_impl_entity_s :: AST.DImplEntity -> State [PPrintSegment] ()
pprint_impl_entity_s (AST.DImplEntity'Fun sf) = pprint_fun_decl_s $ unlocate sf
-}
-- AST.DStmt {{{1
pprint_stmt_s :: AST.DStmt -> State [PPrintSegment] ()

pprint_stmt_s (AST.DStmt'Var ty name maybeinitializer) =
    put "var " >> put (unlocate name) >>
    pprint_type_annotation_s (unlocate ty) >>
    (case maybeinitializer of
        Just init_expr -> put " = " >> pprint_expr_s (unlocate init_expr)
        Nothing -> return ()) >>
    putnl

pprint_stmt_s (AST.DStmt'Expr expr) =
    let neednl = case unlocate expr of
            AST.DExpr'Block _ -> False
            AST.DExpr'If _ _ _ -> False
            AST.DExpr'While _ _ -> False
            _ -> True
    in pprint_expr_s (unlocate expr) >> if neednl then putnl else return ()

-- AST.DExpr {{{1
-- precedence things {{{
expr_requires_prec :: AST.DExpr -> AST.ExprPrec
expr_requires_prec (AST.DExpr'Assign _ _ _) = AST.PrecAssign
expr_requires_prec (AST.DExpr'ShortCircuit _ op _) = AST.prec_of_short_op $ unlocate op
expr_requires_prec (AST.DExpr'Binary _ op _) = AST.prec_of_bin_op $ unlocate op
expr_requires_prec (AST.DExpr'Cast _ _) = AST.PrecCast
expr_requires_prec (AST.DExpr'Unary _ _) = AST.PrecUnary
-- expr_requires_prec (AST.DExpr'Ref _) = AST.PrecUnary
-- expr_requires_prec (AST.DExpr'Deref _) = AST.PrecUnary
expr_requires_prec (AST.DExpr'Call _ _) = AST.PrecCall
-- expr_requires_prec (AST.DExpr'Field _ _) = AST.PrecCall
-- expr_requires_prec (AST.DExpr'Method _ _ _) = AST.PrecCall
expr_requires_prec (AST.DExpr'Bool _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'Float _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'Int _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'Char _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'String _) = AST.PrecPrimary
-- expr_requires_prec (AST.DExpr'This) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'Path _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'Block _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'If _ _ _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'While _ _) = AST.PrecPrimary
expr_requires_prec (AST.DExpr'Ret _) = AST.PrecPrimary

pprint_expr_with_prec_s :: AST.ExprPrec -> AST.DExpr -> State [PPrintSegment] ()
pprint_expr_with_prec_s cur_prec ex =
    if expr_requires_prec ex < cur_prec
    then put "(" >> pprint_expr_with_prec_s AST.PrecAssign ex >> put ")"
    else pprint_expr_s' ex
-- }}}
-- printing different kinds of expressions {{{
pprint_expr_s' :: AST.DExpr -> State [PPrintSegment] ()

pprint_expr_s' (AST.DExpr'Block bl) = pprint_block_expr_s $ unlocate bl

pprint_expr_s' (AST.DExpr'If cond trueb mfalseb) =
    put "if " >> pprint_expr_s (unlocate cond) >> pprint_expr_s (unlocate trueb) >>
    case mfalseb of
        Just falseb -> put "else " >> pprint_expr_s (unlocate falseb)
        Nothing -> return ()

pprint_expr_s' (AST.DExpr'While cond body) = put "while " >> pprint_expr_s (unlocate cond) >> pprint_expr_s (unlocate body)

pprint_expr_s' (AST.DExpr'Assign lhs op rhs) =
    let opstr = case unlocate op of
            AST.Equal -> "="
    in pprint_expr_with_prec_s AST.PrecBinOr (unlocate lhs) >> put " " >> put opstr >> put " " >> pprint_expr_with_prec_s AST.PrecAssign (unlocate rhs)

pprint_expr_s' (AST.DExpr'ShortCircuit lhs op rhs) =
    let opstr = case unlocate op of
            AST.DoubleAmper -> "&&"
            AST.DoublePipe -> "||"

        opprec = AST.prec_of_short_op $ unlocate op

        lhsprec = opprec

        rhsprec = case opprec of
            AST.PrecBinOr -> AST.PrecBinAnd
            AST.PrecBinAnd -> AST.PrecCompEQ
            _ -> error "invalid precedence for precedence of short circuit operator"

    in pprint_expr_with_prec_s lhsprec (unlocate lhs) >> put " " >> put opstr >> put " " >> pprint_expr_with_prec_s rhsprec (unlocate rhs)

pprint_expr_s' (AST.DExpr'Binary lhs op rhs) =
    let opprec = AST.prec_of_bin_op $ unlocate op

        lhsprec = opprec

        rhsprec = case opprec of
            AST.PrecCompEQ -> AST.PrecCompLGT
            AST.PrecCompLGT -> AST.PrecBitXor
            AST.PrecBitXor -> AST.PrecBitOr
            AST.PrecBitOr -> AST.PrecBitAnd
            AST.PrecBitAnd -> AST.PrecBitShift
            AST.PrecBitShift -> AST.PrecAdd
            AST.PrecAdd -> AST.PrecMult
            AST.PrecMult -> AST.PrecCast
            _ -> error "invalid precedence for precedence of binary operator"

        opstr = case unlocate op of
            AST.Plus -> "+"
            AST.Minus -> "-"
            AST.Star -> "*"
            AST.Slash -> "/"
            AST.Percent -> "%"
            AST.Greater -> ">"
            AST.Less -> "<"
            AST.GreaterEqual -> ">="
            AST.LessEqual -> "<="
            AST.Amper -> "&"
            AST.Pipe -> "|"
            AST.Caret -> "^"
            AST.DoubleGreater -> ">>"
            AST.DoubleLess -> "<<"
            AST.DoubleEqual -> "=="
            AST.BangEqual -> "!="

    in pprint_expr_with_prec_s lhsprec (unlocate lhs) >> put " " >> put opstr >> put " " >> pprint_expr_with_prec_s rhsprec (unlocate rhs)

pprint_expr_s' (AST.DExpr'Cast ty expr) = pprint_expr_with_prec_s AST.PrecUnary (unlocate expr) >> put " -> " >> pprint_type_s (unlocate ty)

pprint_expr_s' (AST.DExpr'Unary op expr) =
    let opstr = case unlocate op of
            AST.UnBang -> "!"
            AST.UnTilde -> "~"
            AST.UnMinus -> "-"
    in put opstr >> pprint_expr_with_prec_s AST.PrecUnary (unlocate expr)
-- pprint_expr_s' (AST.DExpr'Ref expr) = put "&" >> pprint_expr_with_prec_s AST.PrecUnary (unlocate expr)
-- pprint_expr_s' (AST.DExpr'Deref expr) = put "*" >> pprint_expr_with_prec_s AST.PrecUnary (unlocate expr)

pprint_expr_s' (AST.DExpr'Call expr args) =
    (let callee_is_field = False
        {-
        case unlocate expr of
            AST.DExpr'Field _ _ -> True
            _ -> False
        -}
    in if callee_is_field
    then put "(" >> pprint_expr_s (unlocate expr) >> put ")"
    else pprint_expr_with_prec_s AST.PrecCall (unlocate expr)) >>
    put "(" >> pprint_list_delim (pprint_expr_s . unlocate) (put ", ") args >> put ")"

{-
pprint_expr_s' (AST.DExpr'Field expr field) = pprint_expr_with_prec_s AST.PrecCall (unlocate expr) >> put "." >> put (unlocate field)
pprint_expr_s' (AST.DExpr'Method expr method args) =
    pprint_expr_with_prec_s AST.PrecCall (unlocate expr) >> put "." >> put (unlocate method) >>
    put "(" >> pprint_list_delim (pprint_expr_s . unlocate) (put ", ") args >> put ")"
-}

pprint_expr_s' (AST.DExpr'Bool val) = put $ if val then "true" else "false"
pprint_expr_s' (AST.DExpr'Float val) = put $ show val
pprint_expr_s' (AST.DExpr'Int val) = put $ show val
pprint_expr_s' (AST.DExpr'Char val) = put $ show val
-- TODO: escape things and properly print multiline strings
pprint_expr_s' (AST.DExpr'String val) = put $ show val
-- pprint_expr_s' (AST.DExpr'This) = put "this"
pprint_expr_s' (AST.DExpr'Path path) = pprint_path_s $ unlocate path

pprint_expr_s' (AST.DExpr'Ret expr) = put "return " >> pprint_expr_s (unlocate expr) >> putnl
-- }}}
pprint_expr_s :: AST.DExpr -> State [PPrintSegment] ()
pprint_expr_s = pprint_expr_with_prec_s AST.PrecAssign

pprint_block_expr_s :: AST.SBlockExpr -> State [PPrintSegment] ()
pprint_block_expr_s (AST.SBlockExpr' stmts) =
    indent >>
    pprint_list (pprint_stmt_s . unlocate) stmts >>
    dedent
-- AST.DParam {{{1
pprint_param_s :: AST.DParam -> State [PPrintSegment] ()
{-
pprint_param_s (AST.DParam'Normal AST.Immutable (
        (Located _ AST.DType'This)
    ) (Located _ "this")) = put "this"
pprint_param_s (AST.DParam'Normal AST.Immutable (
        (Located _ (AST.DType'Pointer AST.Immutable (Located _ AST.DType'This)))
    ) (Located _ "this")) = put "*this"
pprint_param_s (AST.DParam'Normal AST.Immutable (
        (Located _ (AST.DType'Pointer AST.Mutable (Located _ AST.DType'This)))
    ) (Located _ "this")) = put "*mut this"
pprint_param_s (AST.DParam'Normal _ _ (Located _ "this")) = error "pretty print malformed 'this' parameter"
-}
pprint_param_s (AST.DParam'Normal lty lname) =
    put (unlocate lname) >>
    pprint_type_annotation_s (unlocate lty)
-- AST.DType {{{1
pprint_type_s :: AST.DType -> State [PPrintSegment] ()
pprint_type_s (AST.DType'Path path) = pprint_path_s $ unlocate path
{-
pprint_type_s (AST.DType'Pointer lty) =
    put "*" >>
    pprint_type_s (unlocate lty)
-}
-- pprint_type_s (AST.DType'This) = put "this"
-- AST.DPath {{{1
pprint_path_s :: AST.DPath -> State [PPrintSegment] ()
pprint_path_s (AST.DPath' segments) = pprint_list_delim (put . unlocate) (put "::") segments
-- AST.SFunDecl {{{1
pprint_fun_decl_s :: AST.SFunDecl -> State [PPrintSegment] ()
pprint_fun_decl_s (AST.SFunDecl' retty (Located _ name) params expr) =
    put "fun " >> put name >>
    put "(" >> pprint_list_delim (pprint_param_s . unlocate) (put ", ") params >> put ")" >>
    (pprint_type_annotation_s . unlocate) `maybe_do` retty >>
    pprint_block_expr_s (unlocate expr)
-- print type as type annotation {{{1
pprint_type_annotation_s :: AST.DType -> State [PPrintSegment] () -- TODO: do not print if without it defaults to the type
pprint_type_annotation_s ty = put ": " >> pprint_type_s ty
-- splices {{{1
$(make_print_variants "mod")
$(make_print_variants "decl")
-- $(make_print_variants "impl_entity")
$(make_print_variants "stmt")
$(make_print_variants "expr")
$(make_print_variants "param")
$(make_print_variants "type")
$(make_print_variants "path")
