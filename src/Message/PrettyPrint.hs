module Message.PrettyPrint
    ( pprint_mod
    , pprint_decl
    , pprint_expr
    , pprint_param
    , pprint_path
    , pprint_stmt
    , pprint_type
    -- , pprint_impl_entity

    , pprint_lmod
    , pprint_ldecl
    , pprint_lexpr
    , pprint_lparam
    , pprint_lpath
    , pprint_lstmt
    , pprint_ltype
    -- , pprint_limpl_entity
    ) where

import qualified Tokens
import qualified AST

import Location

import Data.List (foldl', intersperse, intercalate)

-- NOTE: NOT an automated code formatter!! just a pretty printer to print things in error messages!

-- PPrintSegment {{{1
data PPrintSegment
    = Token Tokens.Token
    | LToken (Located Tokens.Token)
    | LocStr (Located String)
    | Space

data PPrintSegment'
    = Literal' String
    | Indent'
    | Dedent'
    | Newline'
    | Boom'
    | Semi'

stringify_segments :: [PPrintSegment] -> String
stringify_segments = stringify . process . map to_segment'
    where
        stringify s = snd $ foldl' segment_to_str (0, "") s

        process :: [PPrintSegment'] -> [PPrintSegment']
        process (x : more@(y:_)) = process_two_segments x y ++ process more
        process [x] = [x]
        process [] = []

to_segment' :: PPrintSegment -> PPrintSegment'
to_segment' Space = Literal' " "
to_segment' (LToken (Located _ tok)) = to_segment' (Token tok)
to_segment' (LocStr (Located _ str)) = Literal' str
to_segment' (Token (Tokens.OParen)) = Literal' "("
to_segment' (Token (Tokens.CParen)) = Literal' ")"
to_segment' (Token (Tokens.OBrack)) = Literal' "["
to_segment' (Token (Tokens.CBrack)) = Literal' "]"
to_segment' (Token (Tokens.Comma)) = Literal' ","
to_segment' (Token (Tokens.Period)) = Literal' "."
to_segment' (Token (Tokens.Question)) = Literal' "?"
to_segment' (Token (Tokens.Colon)) = Literal' ":"
to_segment' (Token (Tokens.Bang)) = Literal' "!"
to_segment' (Token (Tokens.Plus)) = Literal' "+"
to_segment' (Token (Tokens.Minus)) = Literal' "-"
to_segment' (Token (Tokens.Star)) = Literal' "*"
to_segment' (Token (Tokens.Slash)) = Literal' "/"
to_segment' (Token (Tokens.Percent)) = Literal' "%"
to_segment' (Token (Tokens.Equal)) = Literal' "="
to_segment' (Token (Tokens.Greater)) = Literal' ">"
to_segment' (Token (Tokens.Less)) = Literal' "<"
to_segment' (Token (Tokens.Tilde)) = Literal' "~"
to_segment' (Token (Tokens.Amper)) = Literal' "&"
to_segment' (Token (Tokens.Pipe)) = Literal' "|"
to_segment' (Token (Tokens.Caret)) = Literal' "^"
to_segment' (Token (Tokens.Dollar)) = Literal' "$"
to_segment' (Token (Tokens.Hash)) = Literal' "#"
to_segment' (Token (Tokens.RightArrow)) = Literal' "->"
to_segment' (Token (Tokens.LeftArrow)) = Literal' "<-"
to_segment' (Token (Tokens.DoublePlus)) = Literal' "++"
to_segment' (Token (Tokens.DoubleMinus)) = Literal' "--"
to_segment' (Token (Tokens.DoubleGreater)) = Literal' ">>"
to_segment' (Token (Tokens.DoubleLess)) = Literal' "<<"
to_segment' (Token (Tokens.DoubleAmper)) = Literal' "&&"
to_segment' (Token (Tokens.DoublePipe)) = Literal' "||"
to_segment' (Token (Tokens.DoubleEqual)) = Literal' "=="
to_segment' (Token (Tokens.DoubleColon)) = Literal' "::"
to_segment' (Token (Tokens.PlusEqual)) = Literal' "+="
to_segment' (Token (Tokens.MinusEqual)) = Literal' "-="
to_segment' (Token (Tokens.StarEqual)) = Literal' "*="
to_segment' (Token (Tokens.SlashEqual)) = Literal' "/="
to_segment' (Token (Tokens.BangEqual)) = Literal' "!="
to_segment' (Token (Tokens.GreaterEqual)) = Literal' ">="
to_segment' (Token (Tokens.LessEqual)) = Literal' "<="
to_segment' (Token (Tokens.PercentEqual)) = Literal' "%="
to_segment' (Token (Tokens.DoubleLessEqual)) = Literal' "<<="
to_segment' (Token (Tokens.DoubleGreaterEqual)) = Literal' ">>="
to_segment' (Token (Tokens.AmperEqual)) = Literal' "&="
to_segment' (Token (Tokens.PipeEqual)) = Literal' "|="
to_segment' (Token (Tokens.CaretEqual)) = Literal' "^="
to_segment' (Token (Tokens.Identifier iden)) = Literal' iden
to_segment' (Token (Tokens.CharLit ch)) = Literal' ['\'', ch, '\'']
-- TODO: escape things and properly print multiline strings
to_segment' (Token (Tokens.StringLit str)) = Literal' $ show str
to_segment' (Token (Tokens.IntLit _ val)) = Literal' $ show val
to_segment' (Token (Tokens.FloatLit val)) = Literal' $ show val
to_segment' (Token (Tokens.BoolLit val)) = Literal' $ if val then "true" else "false"
to_segment' (Token (Tokens.This)) = Literal' "this"
to_segment' (Token (Tokens.Fun)) = Literal' "fun"
to_segment' (Token (Tokens.Root)) = Literal' "root"
to_segment' (Token (Tokens.Let)) = Literal' "let"
to_segment' (Token (Tokens.Mut)) = Literal' "mut"
to_segment' (Token (Tokens.Data)) = Literal' "data"
to_segment' (Token (Tokens.Impl)) = Literal' "impl"
to_segment' (Token (Tokens.Return)) = Literal' "return"
to_segment' (Token (Tokens.While)) = Literal' "while"
to_segment' (Token (Tokens.For)) = Literal' "for"
to_segment' (Token (Tokens.If)) = Literal' "if"
to_segment' (Token (Tokens.Else)) = Literal' "else"
to_segment' (Token (Tokens.Case)) = Literal' "case"
to_segment' (Token (Tokens.Break)) = Literal' "break"
to_segment' (Token (Tokens.Continue)) = Literal' "continue"
to_segment' (Token (Tokens.Boom)) = Boom'
to_segment' (Token (Tokens.OBrace)) = Indent'
to_segment' (Token (Tokens.CBrace)) = Dedent'
to_segment' (Token (Tokens.Semicolon)) = Newline'
to_segment' (Token (Tokens.Indent)) = Indent'
to_segment' (Token (Tokens.Dedent)) = Dedent'
to_segment' (Token (Tokens.Newline)) = Newline'
to_segment' (Token (Tokens.EOF)) = Literal' ""

process_two_segments :: PPrintSegment' -> PPrintSegment' -> [PPrintSegment']
process_two_segments Indent' Indent' = [Indent', Boom']
process_two_segments Indent' Dedent' = [Indent', Boom']
process_two_segments Indent' Newline' = [Indent', Semi']
process_two_segments Dedent' Indent' = [Dedent', Boom']
process_two_segments Dedent' Newline' = [Dedent', Semi']
process_two_segments Newline' Indent' = [Semi']
process_two_segments x _ = [x]

add_to_acc :: String -> Int -> String -> String
add_to_acc acc indamt adding =
    acc ++
    (if safe_last acc == Just '\n'
        then replicate (indamt * 4) ' '
        else ""
    ) ++
    adding
    where
        safe_last [] = Nothing
        safe_last l = Just $ last l

segment_to_str :: (Int, String) -> PPrintSegment' -> (Int, String)
segment_to_str (indamt, acc) (Literal' s) = (indamt     , add_to_acc acc indamt s)
segment_to_str (indamt, acc) Indent'      = (indamt + 1 , add_to_acc acc indamt "\n")
segment_to_str (indamt, acc) Dedent'      = (indamt - 1 , acc)
segment_to_str (indamt, acc) Newline'     = (indamt     , add_to_acc acc indamt "\n")
segment_to_str (indamt, acc) Semi'        = (indamt     , add_to_acc acc indamt ";\n")
segment_to_str (indamt, acc) Boom'        = (indamt     , add_to_acc acc indamt "boom\n")

-- AST.DModule {{{1
to_segments_mod :: AST.DModule -> [PPrintSegment]
to_segments_mod (AST.DModule' decls) = concatMap to_segments_ldecl decls
-- AST.DDecl {{{1
to_segments_decl :: AST.DDecl -> [PPrintSegment]
to_segments_decl (AST.DDecl'Fun sf) = to_segments_fun_decl $ unlocate sf
-- AST.DStmt {{{1
to_segments_stmt :: AST.DStmt -> [PPrintSegment]

to_segments_stmt (AST.DStmt'Var ty name maybeinitializer) =
    [Token Tokens.Let, Space, LocStr name] ++ to_segments_type_annotation (unlocate ty) ++
    (case maybeinitializer of
        Just init_expr -> [Space, Token Tokens.Equal, Space] ++ to_segments_lexpr init_expr
        Nothing -> []
    ) ++ [Token Tokens.Newline]

to_segments_stmt (AST.DStmt'Expr expr) =
    let neednl = case unlocate expr of
            AST.DExpr'Block _ -> False
            AST.DExpr'If _ _ _ -> False
            AST.DExpr'While _ _ -> False
            _ -> True
    in to_segments_lexpr expr ++ if neednl then [Token Tokens.Newline] else []

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

to_segments_expr_with_prec :: AST.ExprPrec -> AST.DExpr -> [PPrintSegment]
to_segments_expr_with_prec cur_prec ex =
    if expr_requires_prec ex < cur_prec
        then [Token Tokens.OParen] ++ to_segments_expr_with_prec AST.PrecAssign ex ++ [Token Tokens.CParen]
        else to_segments_expr' ex
-- }}}
-- printing different kinds of expressions {{{
to_segments_expr' :: AST.DExpr -> [PPrintSegment]

to_segments_expr' (AST.DExpr'Block bl) = to_segments_block_expr $ unlocate bl

to_segments_expr' (AST.DExpr'If cond trueb mfalseb) =
    [Token Tokens.If, Space] ++ to_segments_lexpr cond ++
     to_segments_lexpr trueb ++
    (case mfalseb of
        Just falseb -> [Token Tokens.Else] ++ to_segments_lexpr falseb
        Nothing -> []
    )

to_segments_expr' (AST.DExpr'While cond body) = [Token Tokens.While, Space] ++ to_segments_lexpr cond ++ to_segments_lexpr body

to_segments_expr' (AST.DExpr'Assign lhs op rhs) =
    let op_tok = case unlocate op of
            AST.Equal -> Tokens.Equal
    in to_segments_expr_with_prec AST.PrecBinOr (unlocate lhs) ++ [Space, Token op_tok, Space] ++ to_segments_expr_with_prec AST.PrecAssign (unlocate rhs)

to_segments_expr' (AST.DExpr'ShortCircuit lhs op rhs) =
    let op_tok = case unlocate op of
            AST.DoubleAmper -> Tokens.DoubleAmper
            AST.DoublePipe -> Tokens.DoublePipe

        opprec = AST.prec_of_short_op $ unlocate op

        lhsprec = opprec

        rhsprec = case opprec of
            AST.PrecBinOr -> AST.PrecBinAnd
            AST.PrecBinAnd -> AST.PrecCompEQ
            _ -> error "invalid precedence for precedence of short circuit operator"

    in to_segments_expr_with_prec lhsprec (unlocate lhs) ++ [Space, Token op_tok, Space] ++ to_segments_expr_with_prec rhsprec (unlocate rhs)

to_segments_expr' (AST.DExpr'Binary lhs op rhs) =
    let op_tok = case unlocate op of
            AST.Plus -> Tokens.Plus
            AST.Minus -> Tokens.Minus
            AST.Star -> Tokens.Star
            AST.Slash -> Tokens.Slash
            AST.Percent -> Tokens.Percent
            AST.Greater -> Tokens.Greater
            AST.Less -> Tokens.Less
            AST.GreaterEqual -> Tokens.GreaterEqual
            AST.LessEqual -> Tokens.LessEqual
            AST.Amper -> Tokens.Amper
            AST.Pipe -> Tokens.Pipe
            AST.Caret -> Tokens.Caret
            AST.DoubleGreater -> Tokens.DoubleGreater
            AST.DoubleLess -> Tokens.DoubleLess
            AST.DoubleEqual -> Tokens.DoubleEqual
            AST.BangEqual -> Tokens.BangEqual

        opprec = AST.prec_of_bin_op $ unlocate op

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

    in to_segments_expr_with_prec lhsprec (unlocate lhs) ++ [Space, Token op_tok, Space] ++ to_segments_expr_with_prec rhsprec (unlocate rhs)

to_segments_expr' (AST.DExpr'Cast ty expr) = to_segments_expr_with_prec AST.PrecUnary (unlocate expr) ++ [Space, Token Tokens.RightArrow, Space] ++ to_segments_ltype ty

to_segments_expr' (AST.DExpr'Unary op expr) =
    let op_tok = case unlocate op of
            AST.UnBang -> Tokens.Bang
            AST.UnTilde -> Tokens.Tilde
            AST.UnMinus -> Tokens.Minus

    in [Token op_tok] ++ to_segments_expr_with_prec AST.PrecUnary (unlocate expr)

to_segments_expr' (AST.DExpr'Call expr args) =
    let callee_is_field = False
    in if callee_is_field
        then [Token Tokens.OParen] ++ to_segments_lexpr expr ++ [Token Tokens.CParen]
        else to_segments_expr_with_prec AST.PrecCall (unlocate expr)
    ++
    [Token Tokens.OParen] ++ intercalate [Token Tokens.Comma, Space] (map to_segments_lexpr args) ++ [Token Tokens.CParen]

to_segments_expr' (AST.DExpr'Bool val) = [Token $ Tokens.BoolLit val]
to_segments_expr' (AST.DExpr'Float val) = [Token $ Tokens.FloatLit val]
to_segments_expr' (AST.DExpr'Int val) = [Token $ Tokens.IntLit Tokens.Dec val]
to_segments_expr' (AST.DExpr'Char val) = [Token $ Tokens.CharLit val]
to_segments_expr' (AST.DExpr'String val) = [Token $ Tokens.StringLit val]
to_segments_expr' (AST.DExpr'Path path) = to_segments_lpath path

to_segments_expr' (AST.DExpr'Ret expr) = [Token Tokens.Return, Space] ++ to_segments_lexpr expr ++ [Token Tokens.Newline]
-- }}}
to_segments_expr :: AST.DExpr -> [PPrintSegment]
to_segments_expr = to_segments_expr_with_prec AST.PrecAssign

to_segments_block_expr :: AST.SBlockExpr -> [PPrintSegment]
to_segments_block_expr (AST.SBlockExpr' stmts) = [Token Tokens.Indent] ++ concatMap to_segments_lstmt stmts ++ [Token Tokens.Dedent]
-- AST.DParam {{{1
to_segments_param :: AST.DParam -> [PPrintSegment]
to_segments_param (AST.DParam'Normal ty name) =
    [LocStr name] ++ to_segments_type_annotation (unlocate ty)
-- AST.DType {{{1
to_segments_type :: AST.DType -> [PPrintSegment]
to_segments_type (AST.DType'Path path) = to_segments_lpath path
-- AST.DPath {{{1
to_segments_path :: AST.DPath -> [PPrintSegment]
to_segments_path (AST.DPath' segments) = intersperse (Token Tokens.DoubleColon) $ map LocStr segments
-- AST.SFunDecl {{{1
to_segments_fun_decl :: AST.SFunDecl -> [PPrintSegment]
to_segments_fun_decl (AST.SFunDecl' retty name params expr) =
    [Token Tokens.Fun, Space, LocStr name, Token Tokens.OParen] ++ intercalate [Token Tokens.Comma, Space] (map to_segments_lparam params) ++ [Token Tokens.CParen] ++
    maybe [] (to_segments_type_annotation . unlocate) retty ++
    to_segments_block_expr (unlocate expr)
-- print type as type annotation {{{1
to_segments_type_annotation :: AST.DType -> [PPrintSegment] -- TODO: do not print if without it defaults to the type
to_segments_type_annotation ty = [Token Tokens.Semicolon, Space] ++ to_segments_type ty
-- located varaints {{{1
to_segments_lmod :: AST.LDModule -> [PPrintSegment]
to_segments_lmod = to_segments_mod . unlocate
to_segments_ldecl :: AST.LDDecl -> [PPrintSegment]
to_segments_ldecl = to_segments_decl . unlocate
to_segments_lstmt :: AST.LDStmt -> [PPrintSegment]
to_segments_lstmt = to_segments_stmt . unlocate
to_segments_lexpr :: AST.LDExpr -> [PPrintSegment]
to_segments_lexpr = to_segments_expr . unlocate
to_segments_lparam :: AST.LDParam -> [PPrintSegment]
to_segments_lparam = to_segments_param . unlocate
to_segments_ltype :: AST.LDType -> [PPrintSegment]
to_segments_ltype = to_segments_type . unlocate
to_segments_lpath :: AST.LDPath -> [PPrintSegment]
to_segments_lpath = to_segments_path . unlocate
-- to_segments_limpl_entity = to_segments_impl_entity . unlocate
-- string versions {{{1
pprint_mod :: AST.DModule -> String
pprint_mod = stringify_segments . to_segments_mod
pprint_decl :: AST.DDecl -> String
pprint_decl = stringify_segments . to_segments_decl
pprint_expr :: AST.DExpr -> String
pprint_expr = stringify_segments . to_segments_expr
pprint_param :: AST.DParam -> String
pprint_param = stringify_segments . to_segments_param
pprint_path :: AST.DPath -> String
pprint_path = stringify_segments . to_segments_path
pprint_stmt :: AST.DStmt -> String
pprint_stmt = stringify_segments . to_segments_stmt
pprint_type :: AST.DType -> String
pprint_type = stringify_segments . to_segments_type

pprint_lmod :: AST.LDModule -> String
pprint_lmod = stringify_segments . to_segments_lmod
pprint_ldecl :: AST.LDDecl -> String
pprint_ldecl = stringify_segments . to_segments_ldecl
pprint_lexpr :: AST.LDExpr -> String
pprint_lexpr = stringify_segments . to_segments_lexpr
pprint_lparam :: AST.LDParam -> String
pprint_lparam = stringify_segments . to_segments_lparam
pprint_lpath :: AST.LDPath -> String
pprint_lpath = stringify_segments . to_segments_lpath
pprint_lstmt :: AST.LDStmt -> String
pprint_lstmt = stringify_segments . to_segments_lstmt
pprint_ltype :: AST.LDType -> String
pprint_ltype = stringify_segments . to_segments_ltype
