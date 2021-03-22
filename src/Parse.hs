module Parse(parse) where

import Location
import qualified Lex
import qualified Message
import qualified Message.Underlines as MsgUnds
import qualified AST

import Data.Data(toConstr, Data)
import Data.Maybe(isJust)

-- errors {{{1
data ErrorCondition
    = XIsMissingYFound String String Span (Located Lex.Token)
    | XIsMissingYAfterZFound String String String Span (Located Lex.Token)
    | ExcessTokens Span (Located Lex.Token)
    | InvalidToken String String [String] Span (Located Lex.Token)
    | Unclosed String String Span Span (Located Lex.Token)

condToMsgs :: ErrorCondition -> [MsgUnds.Message]

condToMsgs (XIsMissingYFound x y sp (Located _ tok)) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " (found " ++ Lex.fmtToken tok ++ ")"
    ]
condToMsgs (XIsMissingYAfterZFound x y z sp (Located _ tok)) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " after " ++ z ++ " (found " ++ Lex.fmtToken tok ++ ")"
    ]
condToMsgs (ExcessTokens sp (Located _ tok)) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "extraneous tokens found in input (" ++ Lex.fmtToken tok ++ ")"
    ]
condToMsgs (InvalidToken construct thing possibilities sp (Located _ found)) =
    -- TODO: format this nicer, do not print braces of list, also add 'or' before the last one
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "invalid " ++ thing ++ " for " ++ construct ++ "; must be one of " ++ show possibilities ++ " (found " ++ Lex.fmtToken found ++ ")"
    ]
condToMsgs (Unclosed construct delimiterName openSp sp (Located _ found)) =
    [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "unclosed " ++ delimiterName ++ " for " ++ construct ++ " (found " ++ Lex.fmtToken found ++ ")"
    , MsgUnds.Message openSp MsgUnds.Note MsgUnds.Secondary $ "opening " ++ delimiterName ++ " here"
    ]


data ParseError = ParseError [ErrorCondition]
instance Message.ToDiagnostic ParseError where
    toDiagnostic (ParseError msgs) =
        Message.SimpleDiag Message.Error Nothing Nothing Nothing
            -- $ map ((\ ecmsgs -> Message.Underlines $ MsgUnds.UnderlinesSection ecmsgs) . condToMsgs) msgs
            [Message.Underlines $ MsgUnds.UnderlinesSection $ concatMap condToMsgs msgs]

-- parser {{{1
data Parser = Parser [Located Lex.Token] (Maybe (Located Lex.Token)) [ErrorCondition]

-- TODO: this replicates the functionality of Control.State.Monad; i am making this for practice, but in the future i might make is just use Control.State.Monad
newtype ParseFun a = ParseFun (Parser -> (a, Parser))

instance Functor ParseFun where
    fmap f pf =
        ParseFun $
        \ parser ->
        let (res, after) = runParseFun pf parser
        in (f res, after)

instance Applicative ParseFun where
    pure thing = ParseFun $ \ parser -> (thing, parser)
    x <*> y =
        x >>= \ xres ->
        y >>= \ yres ->
        return (xres yres)

instance Monad ParseFun where
    return = pure
    a >>= b = ParseFun $
        \ parser ->
        let (ares, aftera) = runParseFun a parser
        in runParseFun (b ares) aftera

type ParseFunM a = ParseFun (Maybe a)

-- utility functions {{{1
constrEq :: (Data a, Data b) => a -> b -> Bool
constrEq a b = toConstr a == toConstr b

isTT :: Lex.Token -> Located Lex.Token -> Bool
isTT a (Located _ b) = constrEq a b

isTTU :: Lex.Token -> Located Lex.Token -> Maybe ()
isTTU a b = if isTT a b then Just () else Nothing

isTTS :: Lex.Token -> Located Lex.Token -> Maybe Span
isTTS a b@(Located sp _) = if isTT a b then Just sp else Nothing

unmfp :: ParseFunM a -> (a -> ParseFunM b) -> ParseFunM b
unmfp exa cont =
    exa >>= \ mres ->
    case mres of
        Just res -> cont res
        Nothing -> return Nothing
-- parser helpers {{{1
advance :: Int -> Parser -> Parser
advance 0 p = p
advance 1 (Parser (t:ts) _ errs) = Parser ts (Just t) errs
advance n p = advance 1 $ advance (n - 1) p

advanceS :: Int -> ParseFun ()
advanceS x = ParseFun $ \ parser -> ((), advance x parser)

peek :: Parser -> Located Lex.Token
peek (Parser (x:_) _ _) = x
peek (Parser [] _ _) = error "peek on empty token stream"

peekS :: ParseFun (Located Lex.Token)
peekS = ParseFun $ \ parser -> (peek parser, parser)

newErr :: ErrorCondition -> Parser -> Parser
newErr err (Parser toks l errs) = Parser toks l (errs ++ [err])

newErrS :: ErrorCondition -> ParseFun ()
newErrS err = ParseFun $ \ parser -> ((), newErr err parser)

getParser :: ParseFun Parser
getParser = ParseFun $ \ parser -> (parser, parser)

saveLocation :: ParseFun ([Located Lex.Token], Maybe (Located Lex.Token))
saveLocation = ParseFun $ \ parser@(Parser toks l _) -> ((toks, l), parser)

restoreLocation :: ([Located Lex.Token], Maybe (Located Lex.Token)) -> ParseFun ()
restoreLocation (toks, l) = ParseFun $ \ (Parser _ _ errs) -> ((), Parser toks l errs)

selectSpanFromParser :: Parser -> Span
selectSpanFromParser (Parser toks back _) =
    let front =
            case toks of
                x:_ -> Just x
                [] -> Nothing
    in case (front, back) of
        (Just (Located _ Lex.EOF), Just (Located notEofSp _)) -> notEofSp
        (Just (Located eofSp Lex.EOF), Nothing) -> eofSp
        (Just (Located fsp _), _) -> fsp
        (Nothing, Just (Located bsp _)) -> bsp
        (Nothing, Nothing) -> error "parser has empty token stream, no last"
-- runParseFun {{{1
runParseFun :: ParseFun r -> Parser -> (r, Parser)
runParseFun (ParseFun fun) parser = fun parser
-- combinators {{{1
consume :: (Located Lex.Token -> Maybe t) -> (Span -> Located Lex.Token -> ErrorCondition) -> ParseFunM t
consume predicate onerr =
    peekS >>= \ locatedPeeked ->
    case predicate locatedPeeked of
        Just x ->
            advanceS 1 >>
            return (Just x)
        Nothing ->
            getParser >>= \ parser ->
            newErrS (onerr (selectSpanFromParser parser) locatedPeeked) >>
            return Nothing
-- consume combinators {{{
consumeTokU :: Lex.Token -> (Span -> Located Lex.Token -> ErrorCondition) -> ParseFunM ()
consumeTokU tok = consume (isTTU tok)

consumeTokS :: Lex.Token -> (Span -> Located Lex.Token -> ErrorCondition) -> ParseFunM Span
consumeTokS tok = consume (isTTS tok)

consumeIden :: (Span -> Located Lex.Token -> ErrorCondition) -> ParseFunM (Located String)
consumeIden = consume $
    \ tok ->
    case tok of
        Located sp (Lex.Identifier n) -> Just $ Located sp n
        _ -> Nothing
-- }}}

-- TODO: allow error override, if used then silence all choice's errors, and if none of them match then emit that custom error
choice :: [ParseFunM a] -> ParseFunM a
choice choices =
    saveLocation >>= tryChoices choices
    where
        tryChoices (c:cs) originalLocation =
            restoreLocation originalLocation >>
            c >>= \ res ->
            case res of
                Just x -> return (Just x)
                Nothing -> tryChoices cs originalLocation

        tryChoices [] originalLocation =
            restoreLocation originalLocation >>
            return Nothing

zeromore :: ParseFunM a -> ParseFun [a]
zeromore ex = fun
    where
        fun =
            saveLocation >>= \ saved ->
            ex >>= \ mres ->
            case mres of
                Just res ->
                    fun >>= \ rest ->
                    return ([res] ++ rest)

                Nothing ->
                    restoreLocation saved >>
                    return []

onemore :: ParseFunM a -> ParseFunM [a]
onemore ex = fun []
    where
        fun acc =
            ex >>= \ res ->
            case res of
                Just thing ->
                    fun (acc ++ [thing])

                Nothing ->
                    if length acc == 0
                    then return Nothing
                    else return (Just acc)

onemoredelim :: ParseFunM a -> ParseFunM b -> ParseFunM [a]
onemoredelim ex delim =
    ex `unmfp` \ first ->
    zeromore (
        delim `unmfp` \ _ -> ex
    ) >>= \ rest ->
    return $ Just $ first:rest

convert :: ParseFun a -> (a -> b) -> ParseFun b
convert ex conv =
    ex >>= \ res ->
    return (conv res)

mustMatch :: ParseFunM a -> ParseFunM ()
mustMatch ex =
    saveLocation >>= \ saved ->
    ex >>= \ res ->
    restoreLocation saved >>
    return (const () <$> res)

mustNotMatch :: ParseFunM a -> (Span -> ErrorCondition) -> ParseFunM ()
mustNotMatch ex onerr =
    saveLocation >>= \ saved ->
    ex >>= \ res ->
    restoreLocation saved >>
    case res of
        Just _ ->
            getParser >>= \ parser ->
            newErrS (onerr $ selectSpanFromParser parser) >>
            return Nothing

        Nothing ->
            return (Just ())

mainParser :: ParseFun a -> ParseFun a
mainParser ex =
    ex >>= \ res ->
    consumeTokU Lex.EOF ExcessTokens >>
    return res
-- grammar {{{1
-- grammar helpers {{{
maybeToMutability :: Maybe () -> AST.Mutability
maybeToMutability (Just ()) = AST.Mutable
maybeToMutability Nothing = AST.Immutable
-- }}}
-- TODO: these all need to return located asts
grammar :: ParseFunM AST.DCU
grammar = mainParser (convert declList (AST.DCU'CU <$>))
-- lists {{{2
declList :: ParseFunM [AST.DDecl]
declList = onemore parseDecl

paramList :: ParseFunM [AST.DParam]
-- TODO: improve separator errors
paramList = onemoredelim parseParam (consumeTokU Lex.Comma (XIsMissingYAfterZFound "parameter list" "parameter separator ','" "parameter"))

stmtList :: ParseFunM [AST.DStmt]
stmtList = onemore parseStmt

argList :: ParseFunM [AST.DExpr]
-- TODO: also improve separator errors
argList = onemoredelim parseExpr (consumeTokU Lex.Comma (XIsMissingYAfterZFound "argument list" "argument separator ','" "argument"))
-- line endings {{{2
lnend :: String -> ParseFunM ()
lnend what = consume
    (\ tok ->
    case tok of
        Located _ Lex.Newline -> Just ()
        Located _ Lex.Semicolon -> Just ()
        _ -> Nothing
    ) (XIsMissingYFound what "terminator (newline or ';')")
-- blocks {{{2
blocked, braced, indented :: String -> ParseFun a -> ParseFunM a
blocked what ex = choice [braced what ex, indented what ex]

braced what ex =
    consumeTokS Lex.OBrace (XIsMissingYFound what "opening '{'") `unmfp` \ obracesp ->
    ex >>= \ inside ->
    consumeTokU Lex.CBrace (Unclosed what "'{'" obracesp) `unmfp` \ _ ->
    return $ Just inside

indented what ex =
    consumeTokS Lex.Indent (XIsMissingYFound what "opening indent") `unmfp` \ indentsp ->
    ex >>= \ inside ->
    consumeTokU Lex.Dedent (Unclosed what "indent" indentsp) `unmfp` \ _ ->
    return $ Just inside
-- decl {{{2
parseDecl :: ParseFunM AST.DDecl
parseDecl = choice [convert functionDecl (AST.DDecl'Fun <$>), convert implDecl (const AST.DDecl'Dummy <$>)]

functionDecl :: ParseFunM AST.SFunDecl
functionDecl =
    (consumeTokU Lex.Fun (XIsMissingYFound "function declaration" "introductory 'fun'")) `unmfp` \ _ ->
    (consumeIden (XIsMissingYAfterZFound "function declaration" "function name" "'fun'")) `unmfp` \ name ->
    (consumeTokU Lex.OParen (XIsMissingYAfterZFound "function declaration" "'('" "function name")) `unmfp` \ _ ->
    paramList >>= \ mparamlist ->
    (consumeTokU Lex.CParen (XIsMissingYAfterZFound "function declaration" "')'" "(optional) parameter list")) `unmfp` \ _ ->
    -- TODO: make this type annotation optional
    typeAnnotation `unmfp` \ retty ->
    blockExpr `unmfp` \ body ->
    lnend "function declaration" >>= \ _ ->
    let params = case mparamlist of
            Just l -> l
            Nothing -> []
    in return $ Just $ AST.SFunDecl' retty name params body

implDecl :: ParseFunM ()
-- TODO: impls
implDecl = consumeTokU Lex.Impl (XIsMissingYFound "implementation block" "introductory 'impl'")
-- types {{{2
typeAnnotation :: ParseFunM AST.DType
typeAnnotation =
    (consumeTokU Lex.Colon (XIsMissingYFound "type annotation" "introductory ':'")) `unmfp` \ _ ->
    parseType

parseType, pointerType, thisType, pathType :: ParseFunM AST.DType
parseType = choice [pointerType, thisType, pathType]

pointerType =
    (consumeTokU Lex.Star (XIsMissingYFound "pointer type" "introductory '*'")) `unmfp` \ _ ->
    (consumeTokU Lex.Mut (XIsMissingYAfterZFound "mutable pointer type" "'mut'" "'*'")) >>= \ mmut ->
    parseType `unmfp` \ pointeeTy ->
    return $ Just $ AST.DType'Pointer (maybeToMutability mmut) pointeeTy

thisType = convert (consumeTokU Lex.This (XIsMissingYFound "'this' type" "'this'")) (const AST.DType'This <$>)

pathType = convert parsePath (AST.DType'Path <$>)
-- paths {{{2
parsePath :: ParseFunM AST.DPath
parsePath =
    convert
    -- TODO: improve delimiter error
    (onemoredelim
        (consumeIden (XIsMissingYFound "path" "path segment (identifier)"))
        (consumeTokU Lex.DoubleColon (XIsMissingYAfterZFound "path" "segment separator (':')" "segment")))
    (AST.DPath' <$>)
-- params {{{2
parseParam, normalParam, thisParam :: ParseFunM AST.DParam
parseParam = choice [normalParam, thisParam]

normalParam =
    (consumeTokU Lex.Mut (XIsMissingYFound "mutable parameter" "'mut'")) >>= \ mmut ->
    (consumeIden (XIsMissingYFound "parameter" "parameter name")) `unmfp` \ name ->
    typeAnnotation `unmfp` \ ty ->
    return $ Just $ AST.DParam'Normal (maybeToMutability mmut) ty name

thisParam =
    (
        consumeTokU Lex.Star (XIsMissingYFound "'this' reference parameter" "'*'") `unmfp` \ _ ->
        consumeTokU Lex.Mut (XIsMissingYAfterZFound "'this' mutable reference parameter" "'mut'" "'*'") >>= \ mmut ->
        return $ Just $ isJust mmut
    ) >>= \ mstarmut ->
    consumeTokU Lex.This (XIsMissingYFound "'this' parameter" "'this'") `unmfp` \ _ ->
    let kind = case mstarmut of
            Just True -> AST.MutRef
            Just False -> AST.Ref
            Nothing -> AST.Value
    in return $ Just $ AST.DParam'This kind
-- expr {{{2
parseExpr, ifExpr, whileExpr :: ParseFunM AST.DExpr
parseExpr = choice [assignExpr, ifExpr, whileExpr, convert blockExpr (AST.DExpr'Block <$>)]

blockStmtList :: ParseFunM [AST.DStmt]
blockStmtList =
    blocked "code block" (
        stmtList >>= \ sl ->
        return $ case sl of
            Just x -> x
            Nothing -> []
    )

blockExpr :: ParseFunM AST.SBlockExpr
blockExpr = convert blockStmtList (AST.SBlockExpr' <$>)

ifExpr =
    (consumeTokS Lex.If (XIsMissingYFound "'if' expression" "'if'")) `unmfp` \ ifsp ->
    parseExpr `unmfp` \ cond ->
    blockExpr `unmfp` \ trueb ->
    (
        (consumeTokS Lex.Else (XIsMissingYFound "'else' branch of 'if' expression" "'else'")) `unmfp` \ elsesp ->
        choice [convert blockExpr (AST.DExpr'Block <$>), ifExpr] `unmfp` \ falseb ->
        return $ Just (elsesp, falseb)
    ) >>= \ melseb ->
    return $ Just $ AST.DExpr'If ifsp cond (AST.DExpr'Block trueb) melseb

whileExpr =
    (consumeTokU Lex.While (XIsMissingYFound "'while' expression" "'while'")) `unmfp` \ _ ->
    parseExpr `unmfp` \ cond ->
    blockExpr `unmfp` \ block ->
    return $ Just $ AST.DExpr'While cond (AST.DExpr'Block block)

mkBinExpr :: ParseFunM AST.DExpr -> [ParseFunM a] -> (AST.DExpr -> a -> AST.DExpr -> AST.DExpr) -> ParseFunM AST.DExpr
mkBinExpr next operators constructor =
    next `unmfp` \ lhs ->
    maybeNextRep lhs
    where
        maybeNextRep lhs =
            (
                choice operators `unmfp` \ op ->
                next `unmfp` \ rhs ->
                return $ Just (op, rhs)
            ) >>= \ mrhs ->
            case mrhs of
                Just (op, rhs) -> maybeNextRep $ constructor lhs op rhs
                Nothing -> return $ Just lhs

assignExpr, binOrExpr, binAndExpr, compEQExpr, compLGTExpr, bitXorExpr, bitOrExpr, bitAndExpr, bitShiftExpr, additionExpr, multExpr, castExpr, unaryExpr, callExpr, primaryExpr, pathExpr :: ParseFunM AST.DExpr

assignExpr =
    binOrExpr `unmfp` \ lhs ->
    (
        choice
        [ mkOp Lex.Equal AST.Equal "assignment expression" "operator '='"
        ] `unmfp` \ op ->
        assignExpr `unmfp` \ rhs ->
        return $ Just (op, rhs)
    ) >>= \ mrhs ->
    return $ Just (case mrhs of
        Just (op, rhs) -> AST.DExpr'Assign lhs op rhs
        Nothing -> lhs
    )

binOrExpr = mkBinExpr binAndExpr
        [ mkOp Lex.DoublePipe AST.DoublePipe "binary or expression" "operator '||'"
        ]
        AST.DExpr'ShortCircuit
binAndExpr = mkBinExpr compEQExpr
        [ mkOp Lex.DoubleAmper AST.DoubleAmper "binary and expression" "operator '&&'"
        ]
        AST.DExpr'ShortCircuit
compEQExpr = mkBinExpr compLGTExpr
        [ mkOp Lex.BangEqual AST.BangEqual     exprName "operator '!='"
        , mkOp Lex.DoubleEqual AST.DoubleEqual exprName "operator '=='"
        ]
        AST.DExpr'Binary
    where
        exprName = "equality test expression"
compLGTExpr = mkBinExpr bitXorExpr
        [ mkOp Lex.Greater AST.Greater           exprName "operator '>'"
        , mkOp Lex.Less AST.Less                 exprName "operator '<'"
        , mkOp Lex.GreaterEqual AST.GreaterEqual exprName "operator '>='"
        , mkOp Lex.LessEqual AST.LessEqual       exprName "operator '<='"
        ]
        AST.DExpr'Binary
    where
        exprName = "comparison expression"
bitXorExpr = mkBinExpr bitOrExpr
        [ mkOp Lex.Caret AST.Caret exprName "operator '^'"
        ]
        AST.DExpr'Binary
    where
        exprName = "bitwise xor expression"
bitOrExpr = mkBinExpr bitAndExpr
        [ mkOp Lex.Pipe AST.Pipe exprName "operator '|'"
        ]
        AST.DExpr'Binary
    where
        exprName = "bitwise or expression"
bitAndExpr = mkBinExpr bitShiftExpr
        [ mkOp Lex.Amper AST.Amper exprName "operator '&'"
        ]
        AST.DExpr'Binary
    where
        exprName = "bitwise and expression"
bitShiftExpr = mkBinExpr additionExpr
        [ mkOp Lex.DoubleLess AST.DoubleLess exprName "operator '<<'"
        , mkOp Lex.DoubleGreater AST.DoubleGreater exprName "operator '>>'"
        ]
        AST.DExpr'Binary
    where
        exprName = "bitshift expression"
additionExpr = mkBinExpr multExpr
        [ mkOp Lex.Plus AST.Plus "addition expression" "operator '+'"
        , mkOp Lex.Minus AST.Minus "subtraction expression" "operator '-'"
        ]
        AST.DExpr'Binary
multExpr = mkBinExpr castExpr
        [ mkOp Lex.Star AST.Star "multiplication expression" "operator '*'"
        , mkOp Lex.Slash AST.Slash "division expression" "operator '/'"
        , mkOp Lex.Percent AST.Percent "modulo expression" "operator '%'"
        ]
        AST.DExpr'Binary

castExpr =
    unaryExpr `unmfp` \ lhs ->
    parseMore lhs
    where
        parseMore lhs =
            (
                mkOp Lex.RightArrow () "cast expression" "operator '->'" `unmfp` \ _ ->
                parseType `unmfp` \ ty ->
                return $ Just ty
            ) >>= \ mty ->
            case mty of
                Just ty -> parseMore $ AST.DExpr'Cast ty lhs
                Nothing -> return $ Just lhs

unaryExpr =
    choice [punop, amperExpr, callExpr]
    where
        amperExpr =
            consumeTokS Lex.Amper (XIsMissingYFound "reference expression" "operator '&'") `unmfp` \ ampersp ->
            consumeTokU Lex.Mut (XIsMissingYFound "mutable reference expression" "'mut'") >>= \ mmut ->
            unaryExpr `unmfp` \ operand ->
            return $ Just $ AST.DExpr'Ref ampersp (maybeToMutability mmut) operand

        punop =
            choice
            [ mkOp Lex.Tilde AST.UnTilde "bitwise negation expression" "operator '~'"
            , mkOp Lex.Minus AST.UnMinus "negation expression" "operator '-'"
            , mkOp Lex.Bang AST.UnBang "logical negation expression" "operator '!'"
            , mkOp Lex.Star AST.UnStar "dereference expression" "operator '*'"
            ] `unmfp` \ op ->
            unaryExpr `unmfp` \ operand ->
            return $ Just $ AST.DExpr'Unary op operand

callExpr =
    primaryExpr `unmfp` \ lhs ->
    parseMore lhs
    where
        parseMore lhs =
            choice [method lhs, field lhs, call lhs] >>= \ mres ->
            case mres of
                Just newlhs -> parseMore newlhs
                Nothing -> return $ Just lhs

        consumeDot exprName operandName =
            consumeTokS Lex.Period (XIsMissingYAfterZFound exprName "'.'" operandName)

        field lhs =
            consumeDot "field access expression" "expression with fields" `unmfp` \ dot ->
            consumeIden (XIsMissingYAfterZFound "field access expression" "field name" "'.'") `unmfp` \ fieldname ->
            return $ Just $ AST.DExpr'Field lhs dot fieldname

        method lhs =
            consumeDot "method call expression" "expression with methods" `unmfp` \ dot ->
            consumeIden (XIsMissingYAfterZFound "method call expression" "method name" "'.'") `unmfp` \ methodname ->
            consumeTokS Lex.OParen (XIsMissingYAfterZFound "method call expression" "'('" "method name") `unmfp` \ oparensp ->
            argList `unmfp` \ arglist ->
            consumeTokU Lex.CParen (XIsMissingYAfterZFound "method call expression" "')'" "(optional) argument list") `unmfp` \ _ ->
            return $ Just $ AST.DExpr'Method lhs dot methodname oparensp arglist

        call lhs =
            consumeTokS Lex.OParen (XIsMissingYAfterZFound "call expression" "'('" "callee") `unmfp` \ oparensp ->
            argList `unmfp` \ arglist ->
            consumeTokU Lex.CParen (XIsMissingYAfterZFound "call expression" "')'" "(optional) argument list") `unmfp` \ _ ->
            return $ Just $ AST.DExpr'Call lhs oparensp arglist

primaryExpr = choice [tokExpr, parenExpr, pathExpr]
    where
        tokExpr = consume (
                \ tok ->
                case tok of
                    Located _ (Lex.BoolLit b) -> Just $ AST.DExpr'Bool b
                    Located _ (Lex.FloatLit f) -> Just $ AST.DExpr'Float f
                    Located _ (Lex.IntLit _ i) -> Just $ AST.DExpr'Int i
                    Located _ (Lex.CharLit c) -> Just $ AST.DExpr'Char c
                    Located _ (Lex.StringLit s) -> Just $ AST.DExpr'String s
                    Located _ Lex.This -> Just AST.DExpr'This
                    _ -> Nothing
            ) (XIsMissingYFound "primary expression" "token")
            -- TODO: change this into a 'invalid token for primary expression'
        parenExpr =
            consumeTokS Lex.OParen (XIsMissingYFound "parenthesized expression" "introductory '('") `unmfp` \ oparensp ->
            parseExpr `unmfp` \ inside ->
            consumeTokU Lex.CParen (Unclosed "parenthesized expression" "')'" oparensp) `unmfp` \ _ ->
            return $ Just inside

pathExpr = convert parsePath (AST.DExpr'Path <$>)
-- stmt {{{2
parseStmt, varStmt, retStmt, exprStmt :: ParseFunM AST.DStmt
parseStmt = choice [varStmt, retStmt, exprStmt]

varStmt =
    (consumeTokU Lex.Var (XIsMissingYFound "variable statement" "introductory 'var'")) `unmfp` \ _ ->
    (consumeTokU Lex.Mut (XIsMissingYAfterZFound "variable statement" "'mut'" "'var'")) >>= \ mmut ->
    (consumeIden (XIsMissingYFound "variable statement" "variable name")) `unmfp` \ name ->
    typeAnnotation `unmfp` \ ty ->
    (
        (consumeTokS Lex.Equal (XIsMissingYAfterZFound "variable initialization" "'='" "variable name")) `unmfp` \ eqsp ->
        parseExpr `unmfp` \ initializer ->
        return $ Just (eqsp, initializer)
    ) >>= \ minit ->
    lnend "variable statement" `unmfp` \ _ ->
    return $ Just $ AST.DStmt'Var ty (maybeToMutability mmut) name minit

retStmt =
    (consumeTokU Lex.Return (XIsMissingYFound "return statement" "introductory 'return'")) `unmfp` \ _ ->
    parseExpr `unmfp` \ expr ->
    lnend "return statement" `unmfp` \ _ ->
    return $ Just $ AST.DStmt'Ret expr

exprStmt =
    parseExpr `unmfp` \ expr ->
    -- TODO: blocked exprs do not need line endings
    lnend "expression statement" `unmfp` \ _ ->
    return $ Just $ AST.DStmt'Expr expr

-- parse {{{1
parse :: [Located Lex.Token] -> (Maybe AST.DCU, ParseError)
parse toks =
    let (res, (Parser _ _ errs)) = runParseFun grammar $ Parser toks Nothing []
    in (res, ParseError errs)
