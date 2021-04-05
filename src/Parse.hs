module Parse(parse) where

import Location
import qualified Lex
import qualified Message
import qualified Message.Underlines as MsgUnds
import qualified AST

import Data.List(foldl', nub)
import Data.Data(toConstr, Data)
import Data.Maybe(isJust, fromMaybe)

import Control.Monad.State.Lazy(State, state, runState)

-- TODO: boom tokens

-- errors {{{1
data ErrorCondition = ErrorCondition Int ErrorConditionVariant
    deriving Eq
data ErrorConditionVariant
    = XIsMissingYFound String String Span (Located Lex.Token)
    | XIsMissingYAfterZFound String String String Span (Located Lex.Token)
    | ExcessTokens Span (Located Lex.Token)
    | InvalidToken String String [String] Span (Located Lex.Token)
    | Unclosed String String Span Span (Located Lex.Token)
    | IfContinuedNeeds String String String Span (Located Lex.Token)
    deriving Eq

condSections :: ErrorConditionVariant -> ([MsgUnds.Message], [Message.Section])

condSections (XIsMissingYFound x y sp (Located _ tok)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " (found " ++ Lex.fmtToken tok ++ " instead)"
        ],
    [])
condSections (XIsMissingYAfterZFound x y z sp (Located _ tok)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " after " ++ z ++ " (found " ++ Lex.fmtToken tok ++ " instead)"
        ],
    [])
condSections (ExcessTokens sp (Located _ tok)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "extraneous tokens found in input (found " ++ Lex.fmtToken tok ++ ")"
        ],
    [])
condSections (InvalidToken construct thing possibilities sp (Located _ found)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "invalid " ++ thing ++ " for " ++ construct ++ "; must be " ++ fmtPossibilities possibilities ++ " (found " ++ Lex.fmtToken found ++ " instead)"
        ],
    [])
    where
        fmtPossibilities [] = error "no possibilities"
        fmtPossibilities [x] = x
        fmtPossibilities [x, y] = x ++ " or " ++ y
        fmtPossibilities [x, y, z] = x ++ ", " ++ y ++ ", or " ++ z
        fmtPossibilities (x:xs) = x ++ ", " ++ fmtPossibilities xs
condSections (Unclosed construct delimiterName openSp sp (Located _ found)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ construct ++ " has unclosed " ++ delimiterName ++ " (found " ++ Lex.fmtToken found ++ " instead)"
        , MsgUnds.Message openSp MsgUnds.Note MsgUnds.Secondary $ "opening " ++ delimiterName ++ " is here"
        ],
    [])
condSections (IfContinuedNeeds listname delimiter item sp (Located _ found)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ listname ++ ", if continued, needs " ++ delimiter ++ " after " ++ item ++ " (found " ++ Lex.fmtToken found ++ " instead)"
        ],
    [])

combine :: ErrorCondition -> ErrorCondition -> Maybe ErrorCondition
combine
    (ErrorCondition ind1 (InvalidToken construct1 thing1 pos1 sp1 found1))
    (ErrorCondition ind2 (InvalidToken construct2 thing2 pos2 _ _))
    | construct1 == construct2 && thing1 == thing2 && ind1 == ind2 =
        Just $ ErrorCondition ind1 $ InvalidToken construct1 thing1 (nub $ pos1 ++ pos2) sp1 found1
combine _ _ = Nothing

newtype ParseError = ParseError [ErrorCondition]
instance Message.ToDiagnostic ParseError where
    -- TODO: put span in error line (pass into Message.SimpleDiag constructor)
    toDiagnostic (ParseError msgs) =
        Message.SimpleDiag Message.Error Nothing Nothing Nothing $
            Message.Underlines (MsgUnds.UnderlinesSection messages) : extraSections
        where
            maxind = maximum $ map condloc msgs
            toShow = map condv $ combineAll $ nub $ filter ((maxind==) . condloc) msgs

            (messages, extraSections) =
                (concatMap fst msgsecs, concatMap snd msgsecs)
                where
                    msgsecs = map condSections toShow

            condloc (ErrorCondition i _) = i
            condv (ErrorCondition _ v) = v

            combineAll = foldl' combineOnce []
                where
                    combineOnce acc current =
                        let combinations = [(i, j) | (i, Just j) <- zip [0..] (map (combine current) acc)]
                        in case combinations of
                            (ind, combined):_ ->
                                let (before, after) = splitAt ind acc
                                in before ++ [combined] ++ drop 1 after

                            [] -> acc ++ [current]
                                -- if current coult not combine with any other things
-- parser {{{1
data Parser = Parser Int [Located Lex.Token] [ErrorCondition]
type ParseFun = State Parser
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
-- parser helpers {{{1
advance :: Int -> Parser -> Parser
advance 0 p = p
advance 1 (Parser ind toks errs) = Parser (ind + 1) toks errs
advance n p = advance 1 $ advance (n - 1) p

advanceS :: Int -> ParseFun ()
advanceS x = state $ \ parser -> ((), advance x parser)

peek :: Parser -> Located Lex.Token
peek (Parser ind toks _) =
    case drop ind toks of
        x:_ -> x
        [] -> error "peek on empty token stream"

peekS :: ParseFun (Located Lex.Token)
peekS = state $ \ parser -> (peek parser, parser)

newErr :: ErrorConditionVariant -> Parser -> Parser
newErr err (Parser ind toks errs) = Parser ind toks (errs ++ [ErrorCondition ind err])

newErrS :: ErrorConditionVariant -> ParseFun ()
newErrS err = state $ \ parser -> ((), newErr err parser)

getParser :: ParseFun Parser
getParser = state $ \ parser -> (parser, parser)

saveLocation :: ParseFun (Int, [Located Lex.Token])
saveLocation = state $ \ parser@(Parser ind toks _) -> ((ind, toks), parser)

restoreLocation :: (Int, [Located Lex.Token]) -> ParseFun ()
restoreLocation (ind, toks) = state $ \ (Parser _ _ errs) -> ((), Parser ind toks errs)

selectSpanFromParser :: Parser -> Span
selectSpanFromParser (Parser ind toks _) =
    let front =
            case drop ind toks of
                x:_ -> Just x
                [] -> Nothing
        back = head $ drop (ind - 1) toks
        (Located backsp _) = back
    in
    case front of
        Just (Located _ Lex.EOF) -> backsp
        Nothing -> backsp

        Just (Located sp _) -> sp
-- combinators {{{1
consume :: (Located Lex.Token -> Maybe t) -> (Span -> Located Lex.Token -> ErrorConditionVariant) -> ParseFunM t
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
consumeTokU :: Lex.Token -> (Span -> Located Lex.Token -> ErrorConditionVariant) -> ParseFunM ()
consumeTokU tok = consume (isTTU tok)

consumeTokS :: Lex.Token -> (Span -> Located Lex.Token -> ErrorConditionVariant) -> ParseFunM Span
consumeTokS tok = consume (isTTS tok)

consumeIden :: (Span -> Located Lex.Token -> ErrorConditionVariant) -> ParseFunM (Located String)
consumeIden = consume $
    \ tok ->
    case tok of
        Located sp (Lex.Identifier n) -> Just $ Located sp n
        _ -> Nothing
-- }}}


seqparser :: ParseFunM a -> (a -> ParseFunM b) -> ParseFunM b
seqparser exa cont =
    saveLocation >>= \ saved ->
    exa >>= \ mres ->
    case mres of
        Just res ->
            cont res >>= \ contres ->
            case contres of
                jx@(Just _) -> return jx
                Nothing ->
                    restoreLocation saved >>
                    return Nothing
        Nothing ->
            restoreLocation saved >>
            return Nothing

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
                    return $ res:rest

                Nothing ->
                    restoreLocation saved >>
                    return []

onemore :: ParseFunM a -> ParseFunM [a]
onemore ex = fun []
    where
        fun acc =
            saveLocation >>= \ saved ->
            ex >>= \ res ->
            case res of
                Just thing ->
                    fun (acc ++ [thing])

                Nothing ->
                    restoreLocation saved >>
                    if null acc
                    then return Nothing
                    else return (Just acc)

onemoredelim :: ParseFunM a -> ParseFunM b -> ParseFunM [a]
onemoredelim ex delim =
    ex `seqparser` \ first ->
    zeromore (
        delim `seqparser` const ex
    ) >>= \ rest ->
    return $ Just $ first:rest

{- UNUSED
   but keep if necessary in the future

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

mustNotMatch :: ParseFunM a -> (Span -> ErrorConditionVariant) -> ParseFunM ()
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
-}
-- grammar {{{1
-- grammar helpers {{{
maybeToMutability :: Maybe a -> AST.Mutability
maybeToMutability (Just _) = AST.Mutable
maybeToMutability Nothing = AST.Immutable
-- }}}
-- span helpers {{{
spanFromList :: Span -> [Located a] -> Span
spanFromList fallback [] = fallback
spanFromList _ list =
    joinSpan a b
    where 
        Located a _ = head list
        Located b _ = last list

-- }}}
grammar :: ParseFunM AST.LDModule
grammar =
    declList >>= \ mdl ->
    consumeTokS Lex.EOF ExcessTokens `seqparser` \ eofsp ->
    case mdl of
        Just dl -> return $ Just $ Located (spanFromList eofsp dl) (AST.DModule' dl)
        Nothing -> return $ Just $ Located eofsp $ AST.DModule' []
-- lists {{{2
declList :: ParseFunM [AST.LDDecl]
declList = onemore parseDecl

paramList :: ParseFunM [AST.LDParam]
paramList = onemoredelim parseParam (consumeTokU Lex.Comma (IfContinuedNeeds "parameter list" "separator ','" "parameter"))

stmtList :: ParseFunM [AST.LDStmt]
stmtList = onemore parseStmt

argList :: ParseFunM [AST.LDExpr]
argList = onemoredelim parseExpr (consumeTokU Lex.Comma (IfContinuedNeeds "argument list" "separator ','" "argument"))
-- line endings {{{2
lnend :: String -> ParseFunM Span
lnend what = consume
    (\ tok ->
    case tok of
        Located sp Lex.Newline -> Just sp
        Located sp Lex.Semicolon -> Just sp
        _ -> Nothing
    ) (XIsMissingYFound what "terminator (newline or ';')")
-- blocks {{{2
blocked, braced, indented :: String -> ParseFun a -> ParseFunM (Span, a)
blocked what ex = choice [braced what ex, indented what ex]

braced what ex =
    consumeTokS Lex.OBrace (XIsMissingYFound what "opening '{'") `seqparser` \ obracesp ->
    ex >>= \ inside ->
    consumeTokS Lex.CBrace (Unclosed what "'{'" obracesp) `seqparser` \ cbracesp ->
    return $ Just (obracesp `joinSpan` cbracesp, inside)

indented what ex =
    consumeTokS Lex.Indent (XIsMissingYFound what "opening indent") `seqparser` \ indentsp ->
    ex >>= \ inside ->
    consumeTokS Lex.Dedent (Unclosed what "indent" indentsp) `seqparser` \ dedentsp ->
    return $ Just (indentsp `joinSpan` dedentsp, inside)
-- decl {{{2
parseDecl :: ParseFunM AST.LDDecl
parseDecl =
    choice
        [ implDecl
        , functionDecl `seqparser` \ lsfd@(Located sp _) ->
          return $ Just $ Located sp $ AST.DDecl'Fun lsfd
        ]

functionDecl :: ParseFunM AST.LSFunDecl
functionDecl =
    consumeTokS Lex.Fun (XIsMissingYFound "function declaration" "introductory 'fun'") `seqparser` \ funsp ->
    consumeIden (XIsMissingYAfterZFound "function declaration" "function name" "'fun'") `seqparser` \ name ->
    consumeTokS Lex.OParen (XIsMissingYAfterZFound "function declaration" "'('" "function name") `seqparser` \ oparensp ->
    paramList >>= \ mparamlist ->
    consumeTokS Lex.CParen (Unclosed "function declaration parameter list" "'('" oparensp) `seqparser` \ cparensp ->
    typeAnnotation >>= \ retty ->
    blockExpr `seqparser` \ body ->
    lnend "function declaration" >>
    (let params = fromMaybe [] mparamlist
         fdsp = joinSpan funsp $
            case retty of
                Just (Located rettysp _) -> rettysp
                Nothing -> cparensp

    in return $ Just $ Located fdsp $ AST.SFunDecl' retty name params body)

implDecl :: ParseFunM AST.LDDecl
implDecl =
    consumeTokS Lex.Impl (XIsMissingYFound "implementation block" "introductory 'impl'") `seqparser` \ implsp ->
    parseType `seqparser` \ implFor@(Located tysp _)->
    implBody `seqparser` \ body ->
    return $ Just $ Located (joinSpan implsp tysp) $ AST.DDecl'Impl implFor $ snd body
    where
        implBody = blocked "implementation body" implList
        implList = zeromore $
            choice
                [ functionDecl `seqparser` \ lsfd@(Located sp _) ->
                  return $ Just $ Located sp $ AST.DImplMember'Fun lsfd
                ]
-- types {{{2
typeAnnotation :: ParseFunM AST.LDType
typeAnnotation =
    consumeTokU Lex.Colon (XIsMissingYFound "type annotation" "introductory ':'") `seqparser` \ _ ->
    parseType

parseType, pointerType, thisType, pathType :: ParseFunM AST.LDType
parseType = choice [pointerType, thisType, pathType]

pointerType =
    consumeTokS Lex.Star (XIsMissingYFound "pointer type" "introductory '*'") `seqparser` \ starsp ->
    consumeTokU Lex.Mut (XIsMissingYAfterZFound "mutable pointer type" "'mut'" "'*'") >>= \ mmut ->
    parseType `seqparser` \ pointeeTy@(Located pointeesp _) ->
    return $ Just $ Located (joinSpan starsp pointeesp) $ AST.DType'Pointer (maybeToMutability mmut) pointeeTy

thisType =
    consumeTokS Lex.This (XIsMissingYFound "'this' type" "'this'") `seqparser` \ thsp ->
    return $ Just $ Located thsp AST.DType'This

pathType =
    parsePath `seqparser` \ path@(Located pathsp _) ->
    return $ Just $ Located pathsp $ AST.DType'Path path
-- paths {{{2
parsePath :: ParseFunM AST.LDPath
parsePath =
    onemoredelim
        (consumeIden (XIsMissingYFound "path" "path segment (identifier)"))
        (consumeTokU Lex.DoubleColon (IfContinuedNeeds "path" "segment separator ('::')" "segment (identifier)"))
        `seqparser` \ list ->
    let totalsp = spanFromList (error "path should always have at least one element") list
    in return $ Just $ Located totalsp $ AST.DPath' list
-- params {{{2
parseParam, normalParam, thisParam :: ParseFunM AST.LDParam
parseParam = choice [normalParam, thisParam]

normalParam =
    consumeTokS Lex.Mut (XIsMissingYFound "mutable parameter" "'mut'") >>= \ mmut ->
    consumeIden (XIsMissingYFound "parameter" "parameter name") `seqparser` \ name@(Located namesp _) ->
    typeAnnotation `seqparser` \ ty@(Located tysp _) ->
    let startsp = fromMaybe namesp mmut
        endsp = tysp
    in return $ Just $ Located (joinSpan startsp endsp) $ AST.DParam'Normal (maybeToMutability mmut) ty name

thisParam =
    (
        consumeTokS Lex.Star (XIsMissingYFound "'this' reference parameter" "'*'") `seqparser` \ starsp ->
        consumeTokU Lex.Mut (XIsMissingYAfterZFound "'this' mutable reference parameter" "'mut'" "'*'") >>= \ mmut ->
        return $ Just $ Located starsp $ isJust mmut
    ) >>= \ mstarmut ->
    consumeTokS Lex.This (XIsMissingYFound "'this' parameter" "'this'") `seqparser` \ thissp ->
    let (mstartsp, kind) = case mstarmut of
            Just (Located s True) -> (Just s, AST.MutRef)
            Just (Located s False) -> (Just s, AST.Ref)
            Nothing -> (Nothing, AST.Value)

        startsp = fromMaybe thissp mstartsp
        endsp = thissp

    in return $ Just $ Located (startsp `joinSpan` endsp) $ AST.DParam'This kind
-- expr {{{2
parseExpr, ifExpr, whileExpr :: ParseFunM AST.LDExpr
parseExpr =
    choice
        [ assignExpr
        , ifExpr
        , whileExpr
        , blockExpr `seqparser` \ bl@(Located blsp _) ->
          return $ Just $ Located blsp $ AST.DExpr'Block bl
        ]

blockStmtList :: ParseFunM (Span, [AST.LDStmt])
blockStmtList =
    blocked "code block" (
        stmtList >>= \ sl ->
        return $ fromMaybe [] sl
    )

blockExpr :: ParseFunM AST.LSBlockExpr
blockExpr =
    blockStmtList `seqparser` \ (slsp, sl) ->
    return $ Just $ Located slsp $ AST.SBlockExpr' sl

ifExpr =
    consumeTokS Lex.If (XIsMissingYFound "'if' expression" "'if'") `seqparser` \ ifsp ->
    parseExpr `seqparser` \ cond ->
    blockExpr `seqparser` \ trueb@(Located truebsp _) ->
    (
        consumeTokS Lex.Else (XIsMissingYFound "'else' branch of 'if' expression" "'else'") `seqparser` \ elsesp ->
        choice
            [ blockExpr `seqparser` \ block@(Located blocksp _) ->
              return $ Just $ Located blocksp $ AST.DExpr'Block block
            , ifExpr
            ] `seqparser` \ falseb@(Located falsebsp _) ->
        return $ Just (falsebsp, (elsesp, falseb))
    ) >>= \ melsebandspan ->
    let startsp = ifsp
        endsp = maybe truebsp fst melsebandspan
        melseb = snd <$> melsebandspan

        loctrueb = Located truebsp $ AST.DExpr'Block trueb
    in return $ Just $ Located (startsp `joinSpan` endsp) $ AST.DExpr'If ifsp cond loctrueb melseb

whileExpr =
    consumeTokS Lex.While (XIsMissingYFound "'while' expression" "'while'") `seqparser` \ whilesp ->
    parseExpr `seqparser` \ cond ->
    blockExpr `seqparser` \ block@(Located blocksp _) ->
    return $ Just $ Located (whilesp `joinSpan` blocksp) $ AST.DExpr'While cond (Located blocksp $ AST.DExpr'Block block)

mkBinExpr :: ParseFunM AST.LDExpr -> ParseFunM a -> (AST.LDExpr -> a -> AST.LDExpr -> AST.DExpr) -> ParseFunM AST.LDExpr
mkBinExpr next operators constructor =
    next `seqparser` \ lhs ->
    maybeNextRep lhs
    where
        maybeNextRep lhs@(Located lhssp _) =
            (
                operators `seqparser` \ op ->
                next `seqparser` \ rhs ->
                return $ Just (op, rhs)
            ) >>= \ mrhs ->
            case mrhs of
                Just (op, rhs@(Located rhssp _)) -> maybeNextRep $ Located (lhssp `joinSpan` rhssp) $ constructor lhs op rhs
                Nothing -> return $ Just lhs

assignExpr, binOrExpr, binAndExpr, compEQExpr, compLGTExpr, bitXorExpr, bitOrExpr, bitAndExpr, bitShiftExpr, additionExpr, multExpr, castExpr, unaryExpr, callExpr, primaryExpr, pathExpr :: ParseFunM AST.LDExpr

assignExpr =
    binOrExpr `seqparser` \ lhs@(Located lhssp _) ->
    (
        consume (\ (Located sp tok) ->
        case tok of
            Lex.Equal -> Just $ Located sp AST.Equal
            _ -> Nothing
        ) (InvalidToken "expression" "operator" ["'='"]) `seqparser` \ op ->
        assignExpr `seqparser` \ rhs ->
        return $ Just (op, rhs)
    ) >>= \ mrhs ->
    return $ Just (case mrhs of
        Just (op, rhs@(Located rhssp _)) -> Located (lhssp `joinSpan` rhssp) $ AST.DExpr'Assign lhs op rhs
        Nothing -> lhs
    )

binOrExpr = mkBinExpr binAndExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.DoublePipe -> Just AST.DoublePipe
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'||'"]))
        AST.DExpr'ShortCircuit
binAndExpr = mkBinExpr compEQExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.DoubleAmper -> Just AST.DoubleAmper
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'&&'"]))
        AST.DExpr'ShortCircuit
compEQExpr = mkBinExpr compLGTExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.BangEqual -> Just AST.BangEqual
                    Lex.DoubleEqual -> Just AST.DoubleEqual
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'!='", "'=='"]))
        AST.DExpr'Binary
compLGTExpr = mkBinExpr bitXorExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.Greater -> Just AST.Greater
                    Lex.Less -> Just AST.Less
                    Lex.GreaterEqual -> Just AST.GreaterEqual
                    Lex.LessEqual -> Just AST.LessEqual
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'>'", "'<'", "'>='", "'<='"]))
        AST.DExpr'Binary
bitXorExpr = mkBinExpr bitOrExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.Caret -> Just AST.Caret
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'^'"]))
        AST.DExpr'Binary
bitOrExpr = mkBinExpr bitAndExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.Pipe -> Just AST.Pipe
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'|'"]))
        AST.DExpr'Binary
bitAndExpr = mkBinExpr bitShiftExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.Amper -> Just AST.Amper
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'&'"]))
        AST.DExpr'Binary
bitShiftExpr = mkBinExpr additionExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.DoubleLess -> Just AST.DoubleLess
                    Lex.DoubleGreater -> Just AST.DoubleGreater
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'<<'", "'>>'"]))
        AST.DExpr'Binary
additionExpr = mkBinExpr multExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.Plus -> Just AST.Plus
                    Lex.Minus -> Just AST.Minus
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'+'", "'-'"]))
        AST.DExpr'Binary
multExpr = mkBinExpr castExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Lex.Star -> Just AST.Star
                    Lex.Slash -> Just AST.Slash
                    Lex.Percent -> Just AST.Percent
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'*'", "'/'", "'%'"]))
        AST.DExpr'Binary

castExpr =
    unaryExpr `seqparser` \ lhs ->
    parseMore lhs
    where
        parseMore lhs@(Located lhssp _) =
            (
                consumeTokU Lex.RightArrow (XIsMissingYAfterZFound "cast expression" "'->'" "expression to cast") `seqparser` \ _ ->
                parseType `seqparser` \ ty ->
                return $ Just ty
            ) >>= \ mty ->
            case mty of
                Just ty@(Located tysp _) -> parseMore $ Located (lhssp `joinSpan` tysp) $ AST.DExpr'Cast ty lhs
                Nothing -> return $ Just lhs

unaryExpr =
    choice [punop, amperExpr, callExpr]
    where
        amperExpr =
            consumeTokS Lex.Amper (XIsMissingYFound "reference expression" "operator '&'") `seqparser` \ ampersp ->
            consumeTokU Lex.Mut (XIsMissingYFound "mutable reference expression" "'mut'") >>= \ mmut ->
            unaryExpr `seqparser` \ operand@(Located operandsp _) ->
            return $ Just $ Located (ampersp `joinSpan` operandsp) $ AST.DExpr'Ref ampersp (maybeToMutability mmut) operand

        punop =
            consume (\ (Located sp tok) ->
                let op = case tok of
                        Lex.Tilde -> Just AST.UnTilde
                        Lex.Minus -> Just AST.UnMinus
                        Lex.Bang -> Just AST.UnBang
                        Lex.Star -> Just AST.UnStar
                        _ -> Nothing
                in Located sp <$> op
            ) (InvalidToken "unary expression" "operator" ["'~'", "'-'", "'!'", "'*'"]) `seqparser` \ op@(Located opsp _) ->
            unaryExpr `seqparser` \ operand@(Located operandsp _) ->
            return $ Just $ Located (opsp `joinSpan` operandsp) $ AST.DExpr'Unary op operand

callExpr =
    primaryExpr `seqparser` \ lhs ->
    parseMore lhs
    where
        parseMore lhs =
            choice [method lhs, field lhs, call lhs] >>= \ mres ->
            case mres of
                Just newlhs -> parseMore newlhs
                Nothing -> return $ Just lhs

        consumeDot exprName operandName =
            consumeTokS Lex.Period (XIsMissingYAfterZFound exprName "'.'" operandName)

        field lhs@(Located lhssp _) =
            consumeDot "field access expression" "expression with fields" `seqparser` \ dot ->
            consumeIden (XIsMissingYAfterZFound "field access expression" "field name" "'.'") `seqparser` \ fieldname@(Located fieldsp _) ->
            return $ Just $ Located (lhssp `joinSpan` fieldsp) $ AST.DExpr'Field lhs dot fieldname

        method lhs@(Located lhssp _) =
            consumeDot "method call expression" "expression with methods" `seqparser` \ dot ->
            consumeIden (XIsMissingYAfterZFound "method call expression" "method name" "'.'") `seqparser` \ methodname ->
            consumeTokS Lex.OParen (XIsMissingYAfterZFound "method call expression" "'('" "method name") `seqparser` \ oparensp ->
            argList >>= \ marglist ->
            consumeTokS Lex.CParen (Unclosed "method call expression" "'('" oparensp) `seqparser` \ cparensp ->
            let arglist = fromMaybe [] marglist
            in return $ Just $ Located (lhssp `joinSpan` cparensp) $ AST.DExpr'Method lhs dot methodname oparensp arglist

        call lhs@(Located lhssp _) =
            consumeTokS Lex.OParen (XIsMissingYAfterZFound "call expression" "'('" "callee") `seqparser` \ oparensp ->
            argList >>= \ marglist ->
            consumeTokS Lex.CParen (Unclosed "call expression" "'('" oparensp) `seqparser` \ cparensp ->
            let arglist = fromMaybe [] marglist
            in return $ Just $ Located (lhssp `joinSpan` cparensp) $ AST.DExpr'Call lhs oparensp arglist

primaryExpr = choice [tokExpr, parenExpr, pathExpr]
    where
        tokExpr = consume (
                \ (Located sp tok) ->
                    let e = case tok of
                            Lex.BoolLit b -> Just $ AST.DExpr'Bool b
                            Lex.FloatLit f -> Just $ AST.DExpr'Float f
                            Lex.IntLit _ i -> Just $ AST.DExpr'Int i
                            Lex.CharLit c -> Just $ AST.DExpr'Char c
                            Lex.StringLit s -> Just $ AST.DExpr'String s
                            Lex.This -> Just AST.DExpr'This
                            _ -> Nothing
                    in Located sp <$> e
            ) (InvalidToken "primary expression" "token" ["a literal", "'this'"])
        parenExpr =
            consumeTokS Lex.OParen (XIsMissingYFound "parenthesized expression" "introductory '('") `seqparser` \ oparensp ->
            parseExpr `seqparser` \ inside ->
            consumeTokU Lex.CParen (Unclosed "parenthesized expression" "'('" oparensp) `seqparser` \ _ ->
            return $ Just inside

pathExpr =
    parsePath `seqparser` \ path@(Located pathsp _) ->
    return $ Just $ Located pathsp $ AST.DExpr'Path path
-- stmt {{{2
parseStmt, varStmt, retStmt, exprStmt :: ParseFunM AST.LDStmt
parseStmt = choice [exprStmt, varStmt, retStmt]

varStmt =
    consumeTokS Lex.Var (XIsMissingYFound "variable statement" "introductory 'var'") `seqparser` \ varsp ->
    consumeTokU Lex.Mut (XIsMissingYAfterZFound "variable statement" "'mut'" "'var'") >>= \ mmut ->
    consumeIden (XIsMissingYFound "variable statement" "variable name") `seqparser` \ name ->
    typeAnnotation `seqparser` \ ty ->
    (
        consumeTokS Lex.Equal (XIsMissingYAfterZFound "variable initialization" "'='" "variable name") `seqparser` \ eqsp ->
        parseExpr `seqparser` \ initializer ->
        return $ Just (eqsp, initializer)
    ) >>= \ minit ->
    lnend "variable statement" `seqparser` \ endlsp ->
    return $ Just $ Located (varsp `joinSpan` endlsp) $ AST.DStmt'Var ty (maybeToMutability mmut) name minit

retStmt =
    consumeTokS Lex.Return (XIsMissingYFound "return statement" "introductory 'return'") `seqparser` \ retsp ->
    parseExpr `seqparser` \ expr ->
    lnend "return statement" `seqparser` \ endlsp ->
    return $ Just $ Located (retsp `joinSpan` endlsp) $ AST.DStmt'Ret expr

exprStmt =
    parseExpr `seqparser` \ expr@(Located exprsp notLocatedExpr) ->
    let needendl = case notLocatedExpr of
            AST.DExpr'Block _ -> False
            AST.DExpr'If {} -> False
            AST.DExpr'While {} -> False
            _ -> True
    in
    (
        if needendl
        then lnend "expression statement without block" `seqparser` \ ln -> return $ Just $ Just ln
        else lnend "expression statement with block" >>= \ ln -> return $ Just ln
    ) `seqparser` \ msp ->
    let totalsp = case msp of
            Just sp -> exprsp `joinSpan` sp
            Nothing -> exprsp
    in return $ Just $ Located totalsp $ AST.DStmt'Expr expr

-- parse {{{1
parse :: [Located Lex.Token] -> Either ParseError AST.LDModule
parse toks =
    let (res, Parser _ _ errs) = runState grammar $ Parser 0 toks []
    in case res of
        Just x -> Right x
        Nothing -> Left $ ParseError errs
