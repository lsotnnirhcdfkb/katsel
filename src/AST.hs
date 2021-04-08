module AST where

import Location

import qualified Tokens
import qualified Message
import qualified Message.Underlines as MsgUnds

import Data.List(foldl', nub)
import Data.Data(toConstr, Data)
import Data.Maybe(fromMaybe)

import Control.Monad.State.Lazy(State, state, runState)

{-
    most asts are just a plain adt, but some asts are required in multiple asts
    for example, function declarations are needed in a standard Decl and also in an ImplMember
    to prevent duplication, these asts are separated into separate datatypes

    so, a Decl is a variant, but a FunDecl is a separated datatype (due to its multiple uses)

    so, there are 2 types of datatypes in this module:
        - variant datatypes
        - separated datatypes

    to prevent name collisions, variant datatypes are prefixed with 'D', and separated datatypes are prefixed with 'S'
    constructor names are prefixed with the datatype name and a '''
    so, the Decl/FunctionDecl hierarchy would be

        data DDecl
            = DDecl'Fun SFunDecl
            ...

        data SFunDecl
            = SFunDecl' DType ...

    all the datatypes (except for a few, like 'Mutability') when used also should be located, so there are type aliases that are used for located types
    the type alias names are prefixed with 'L', so 'DDecl' would have the type alias:

        type LDDecl = Located Decl

    'BinOp' would have the type alias:

        type LBinOp = Located BinOp

    and 'SFunDecl' would have the type alias:

        type DSFunDecl = Located SFunDecl
-}

type LocStr = Located String

data Mutability = Mutable | Immutable

type LBinOp = Located BinOp
data BinOp
    = Plus | Minus | Star | Slash | Percent
    | Greater | Less | GreaterEqual | LessEqual
    | Amper | Pipe | Caret
    | DoubleGreater | DoubleLess
    | DoubleEqual | BangEqual

type LShortOp = Located ShortOp
data ShortOp = DoubleAmper | DoublePipe
type LUnaryOp = Located UnaryOp
data UnaryOp = UnBang | UnTilde | UnMinus | UnStar
type LAssignOp = Located AssignOp
data AssignOp = Equal

type LSFunDecl = Located SFunDecl
data SFunDecl = SFunDecl' (Maybe LDType) LocStr [LDParam] LSBlockExpr

type LSBlockExpr = Located SBlockExpr
newtype SBlockExpr = SBlockExpr' [LDStmt]

type LDModule = Located DModule
newtype DModule = DModule' [LDDecl]

type LDDecl = Located DDecl
data DDecl
    = DDecl'Fun LSFunDecl
    | DDecl'Impl LDType [LDImplMember]

type LDImplMember = Located DImplMember
newtype DImplMember
    = DImplMember'Fun LSFunDecl

type LDStmt = Located DStmt
data DStmt
    = DStmt'Var LDType Mutability LocStr (Maybe (Span, LDExpr))
    | DStmt'Expr LDExpr
    | DStmt'Ret LDExpr

type LDExpr = Located DExpr
data DExpr
    = DExpr'Block LSBlockExpr
    | DExpr'If Span LDExpr LDExpr (Maybe (Span, LDExpr))
    | DExpr'While LDExpr LDExpr
    | DExpr'Assign LDExpr LAssignOp LDExpr
    | DExpr'ShortCircuit LDExpr LShortOp LDExpr
    | DExpr'Binary LDExpr LBinOp LDExpr
    | DExpr'Cast LDType LDExpr
    | DExpr'Unary LUnaryOp LDExpr
    | DExpr'Ref Span Mutability LDExpr
    | DExpr'Call LDExpr Span [LDExpr]
    | DExpr'Field LDExpr Span LocStr
    | DExpr'Method LDExpr Span LocStr Span [LDExpr]
    | DExpr'Bool Bool
    | DExpr'Float Double
    | DExpr'Int Integer
    | DExpr'Char Char
    | DExpr'String String
    | DExpr'This
    | DExpr'Path LDPath

type LDParam = Located DParam
data DParam
    = DParam'Normal Mutability LDType LocStr

type LDType = Located DType
data DType
    = DType'Path LDPath
    | DType'Pointer Mutability LDType
    | DType'This

type LDPath = Located DPath
newtype DPath = DPath' [LocStr]

-- TODO: boom tokens

-- errors {{{1
data ErrorCondition = ErrorCondition Int ErrorConditionVariant
    deriving Eq
data ErrorConditionVariant
    = XIsMissingYFound String String Span (Located Tokens.Token)
    | XIsMissingYAfterZFound String String String Span (Located Tokens.Token)
    | ExcessTokens Span (Located Tokens.Token)
    | InvalidToken String String [String] Span (Located Tokens.Token)
    | Unclosed String String Span Span (Located Tokens.Token)
    | IfContinuedNeeds String String String Span (Located Tokens.Token)
    deriving Eq

condSections :: ErrorConditionVariant -> ([MsgUnds.Message], [Message.Section])

condSections (XIsMissingYFound x y sp (Located _ tok)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " (found " ++ Tokens.fmtToken tok ++ " instead)"
        ],
    [])
condSections (XIsMissingYAfterZFound x y z sp (Located _ tok)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " after " ++ z ++ " (found " ++ Tokens.fmtToken tok ++ " instead)"
        ],
    [])
condSections (ExcessTokens sp (Located _ tok)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "extraneous tokens found in input (found " ++ Tokens.fmtToken tok ++ ")"
        ],
    [])
condSections (InvalidToken construct thing possibilities sp (Located _ found)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ "invalid " ++ thing ++ " for " ++ construct ++ "; must be " ++ fmtPossibilities possibilities ++ " (found " ++ Tokens.fmtToken found ++ " instead)"
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
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ construct ++ " has unclosed " ++ delimiterName ++ " (found " ++ Tokens.fmtToken found ++ " instead)"
        , MsgUnds.Message openSp MsgUnds.Note MsgUnds.Secondary $ "opening " ++ delimiterName ++ " is here"
        ],
    [])
condSections (IfContinuedNeeds listname delimiter item sp (Located _ found)) =
    (
        [ MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary $ listname ++ ", if continued, needs " ++ delimiter ++ " after " ++ item ++ " (found " ++ Tokens.fmtToken found ++ " instead)"
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
data Parser = Parser Int [Located Tokens.Token] [ErrorCondition]
type ParseFun = State Parser
type ParseFunM a = ParseFun (Maybe a)
-- utility functions {{{1
constrEq :: (Data a, Data b) => a -> b -> Bool
constrEq a b = toConstr a == toConstr b

isTT :: Tokens.Token -> Located Tokens.Token -> Bool
isTT a (Located _ b) = constrEq a b

isTTU :: Tokens.Token -> Located Tokens.Token -> Maybe ()
isTTU a b = if isTT a b then Just () else Nothing

isTTS :: Tokens.Token -> Located Tokens.Token -> Maybe Span
isTTS a b@(Located sp _) = if isTT a b then Just sp else Nothing
-- parser helpers {{{1
advance :: Int -> Parser -> Parser
advance 0 p = p
advance 1 (Parser ind toks errs) = Parser (ind + 1) toks errs
advance n p = advance 1 $ advance (n - 1) p

advanceS :: Int -> ParseFun ()
advanceS x = state $ \ parser -> ((), advance x parser)

peek :: Parser -> Located Tokens.Token
peek (Parser ind toks _) =
    case drop ind toks of
        x:_ -> x
        [] -> error "peek on empty token stream"

peekS :: ParseFun (Located Tokens.Token)
peekS = state $ \ parser -> (peek parser, parser)

newErr :: ErrorConditionVariant -> Parser -> Parser
newErr err (Parser ind toks errs) = Parser ind toks (errs ++ [ErrorCondition ind err])

newErrS :: ErrorConditionVariant -> ParseFun ()
newErrS err = state $ \ parser -> ((), newErr err parser)

getParser :: ParseFun Parser
getParser = state $ \ parser -> (parser, parser)

saveLocation :: ParseFun (Int, [Located Tokens.Token])
saveLocation = state $ \ parser@(Parser ind toks _) -> ((ind, toks), parser)

restoreLocation :: (Int, [Located Tokens.Token]) -> ParseFun ()
restoreLocation (ind, toks) = state $ \ (Parser _ _ errs) -> ((), Parser ind toks errs)

selectSpanFromParser :: Parser -> Span
selectSpanFromParser (Parser ind toks _) =
    let front =
            case drop ind toks of
                x:_ -> Just x
                [] -> Nothing
        back = toks !! (ind - 1)
        (Located backsp _) = back
    in
    case front of
        Just (Located _ Tokens.EOF) -> backsp
        Nothing -> backsp

        Just (Located sp _) -> sp
-- combinators {{{1
consume :: (Located Tokens.Token -> Maybe t) -> (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM t
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
consumeTokU :: Tokens.Token -> (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM ()
consumeTokU tok = consume (isTTU tok)

consumeTokS :: Tokens.Token -> (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM Span
consumeTokS tok = consume (isTTS tok)

consumeIden :: (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM (Located String)
consumeIden = consume $
    \ tok ->
    case tok of
        Located sp (Tokens.Identifier n) -> Just $ Located sp n
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
    consumeTokS Tokens.EOF ExcessTokens `seqparser` \ eofsp ->
    case mdl of
        Just dl -> return $ Just $ Located (spanFromList eofsp dl) (AST.DModule' dl)
        Nothing -> return $ Just $ Located eofsp $ AST.DModule' []
-- lists {{{2
declList :: ParseFunM [AST.LDDecl]
declList = onemore parseDecl

paramList :: ParseFunM [AST.LDParam]
paramList = onemoredelim parseParam (consumeTokU Tokens.Comma (IfContinuedNeeds "parameter list" "separator ','" "parameter"))

stmtList :: ParseFunM [AST.LDStmt]
stmtList = onemore parseStmt

argList :: ParseFunM [AST.LDExpr]
argList = onemoredelim parseExpr (consumeTokU Tokens.Comma (IfContinuedNeeds "argument list" "separator ','" "argument"))
-- line endings {{{2
lnend :: String -> ParseFunM Span
lnend what = consume
    (\ tok ->
    case tok of
        Located sp Tokens.Newline -> Just sp
        Located sp Tokens.Semicolon -> Just sp
        _ -> Nothing
    ) (XIsMissingYFound what "terminator (newline or ';')")
-- blocks {{{2
blocked, braced, indented :: String -> ParseFun a -> ParseFunM (Span, a)
blocked what ex = choice [braced what ex, indented what ex]

braced what ex =
    consumeTokS Tokens.OBrace (XIsMissingYFound what "opening '{'") `seqparser` \ obracesp ->
    ex >>= \ inside ->
    consumeTokS Tokens.CBrace (Unclosed what "'{'" obracesp) `seqparser` \ cbracesp ->
    return $ Just (obracesp `joinSpan` cbracesp, inside)

indented what ex =
    consumeTokS Tokens.Indent (XIsMissingYFound what "opening indent") `seqparser` \ indentsp ->
    ex >>= \ inside ->
    consumeTokS Tokens.Dedent (Unclosed what "indent" indentsp) `seqparser` \ dedentsp ->
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
    consumeTokS Tokens.Fun (XIsMissingYFound "function declaration" "introductory 'fun'") `seqparser` \ funsp ->
    consumeIden (XIsMissingYAfterZFound "function declaration" "function name" "'fun'") `seqparser` \ name ->
    consumeTokS Tokens.OParen (XIsMissingYAfterZFound "function declaration" "'('" "function name") `seqparser` \ oparensp ->
    paramList >>= \ mparamlist ->
    consumeTokS Tokens.CParen (Unclosed "function declaration parameter list" "'('" oparensp) `seqparser` \ cparensp ->
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
    consumeTokS Tokens.Impl (XIsMissingYFound "implementation block" "introductory 'impl'") `seqparser` \ implsp ->
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
    consumeTokU Tokens.Colon (XIsMissingYFound "type annotation" "introductory ':'") `seqparser` \ _ ->
    parseType

parseType, pointerType, thisType, pathType :: ParseFunM AST.LDType
parseType = choice [pointerType, thisType, pathType]

pointerType =
    consumeTokS Tokens.Star (XIsMissingYFound "pointer type" "introductory '*'") `seqparser` \ starsp ->
    consumeTokU Tokens.Mut (XIsMissingYAfterZFound "mutable pointer type" "'mut'" "'*'") >>= \ mmut ->
    parseType `seqparser` \ pointeeTy@(Located pointeesp _) ->
    return $ Just $ Located (joinSpan starsp pointeesp) $ AST.DType'Pointer (maybeToMutability mmut) pointeeTy

thisType =
    consumeTokS Tokens.This (XIsMissingYFound "'this' type" "'this'") `seqparser` \ thsp ->
    return $ Just $ Located thsp AST.DType'This

pathType =
    parsePath `seqparser` \ path@(Located pathsp _) ->
    return $ Just $ Located pathsp $ AST.DType'Path path
-- paths {{{2
parsePath :: ParseFunM AST.LDPath
parsePath =
    onemoredelim
        (consumeIden (XIsMissingYFound "path" "path segment (identifier)"))
        (consumeTokU Tokens.DoubleColon (IfContinuedNeeds "path" "segment separator ('::')" "segment (identifier)"))
        `seqparser` \ list ->
    let totalsp = spanFromList (error "path should always have at least one element") list
    in return $ Just $ Located totalsp $ AST.DPath' list
-- params {{{2
parseParam, normalParam, thisParam :: ParseFunM AST.LDParam
parseParam = choice [normalParam, thisParam]

normalParam =
    consumeTokS Tokens.Mut (XIsMissingYFound "mutable parameter" "'mut'") >>= \ mmut ->
    consumeIden (XIsMissingYFound "parameter" "parameter name") `seqparser` \ name@(Located namesp _) ->
    typeAnnotation `seqparser` \ ty@(Located tysp _) ->
    let startsp = fromMaybe namesp mmut
        endsp = tysp
    in return $ Just $ Located (joinSpan startsp endsp) $ AST.DParam'Normal (maybeToMutability mmut) ty name

thisParam =
    (
        consumeTokS Tokens.Star (XIsMissingYFound "'this' reference parameter" "'*'") `seqparser` \ starsp ->
        consumeTokU Tokens.Mut (XIsMissingYAfterZFound "'this' mutable reference parameter" "'mut'" "'*'") >>= \ mmut ->
        return $ Just $ Located starsp mmut
    ) >>= \ mstarmut ->
    consumeTokS Tokens.This (XIsMissingYFound "'this' parameter" "'this'") `seqparser` \ thissp ->
    let plainThisTy = Located thissp AST.DType'This
        (mstartsp, ty) = case mstarmut of
            Just (Located starsp mutable) -> (
                    Just starsp,
                    Located (starsp `joinSpan` endsp) $ AST.DType'Pointer (maybeToMutability mutable) plainThisTy
                )

            Nothing -> (Nothing, plainThisTy)

        ty :: AST.LDType

        startsp = fromMaybe thissp mstartsp
        endsp = thissp

    in return $ Just $ Located (startsp `joinSpan` endsp) $ AST.DParam'Normal AST.Immutable ty (Located thissp "this")
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
    consumeTokS Tokens.If (XIsMissingYFound "'if' expression" "'if'") `seqparser` \ ifsp ->
    parseExpr `seqparser` \ cond ->
    blockExpr `seqparser` \ trueb@(Located truebsp _) ->
    (
        consumeTokS Tokens.Else (XIsMissingYFound "'else' branch of 'if' expression" "'else'") `seqparser` \ elsesp ->
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
    consumeTokS Tokens.While (XIsMissingYFound "'while' expression" "'while'") `seqparser` \ whilesp ->
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
            Tokens.Equal -> Just $ Located sp AST.Equal
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
                    Tokens.DoublePipe -> Just AST.DoublePipe
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'||'"]))
        AST.DExpr'ShortCircuit
binAndExpr = mkBinExpr compEQExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.DoubleAmper -> Just AST.DoubleAmper
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'&&'"]))
        AST.DExpr'ShortCircuit
compEQExpr = mkBinExpr compLGTExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.BangEqual -> Just AST.BangEqual
                    Tokens.DoubleEqual -> Just AST.DoubleEqual
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'!='", "'=='"]))
        AST.DExpr'Binary
compLGTExpr = mkBinExpr bitXorExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Greater -> Just AST.Greater
                    Tokens.Less -> Just AST.Less
                    Tokens.GreaterEqual -> Just AST.GreaterEqual
                    Tokens.LessEqual -> Just AST.LessEqual
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'>'", "'<'", "'>='", "'<='"]))
        AST.DExpr'Binary
bitXorExpr = mkBinExpr bitOrExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Caret -> Just AST.Caret
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'^'"]))
        AST.DExpr'Binary
bitOrExpr = mkBinExpr bitAndExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Pipe -> Just AST.Pipe
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'|'"]))
        AST.DExpr'Binary
bitAndExpr = mkBinExpr bitShiftExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Amper -> Just AST.Amper
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'&'"]))
        AST.DExpr'Binary
bitShiftExpr = mkBinExpr additionExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.DoubleLess -> Just AST.DoubleLess
                    Tokens.DoubleGreater -> Just AST.DoubleGreater
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'<<'", "'>>'"]))
        AST.DExpr'Binary
additionExpr = mkBinExpr multExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Plus -> Just AST.Plus
                    Tokens.Minus -> Just AST.Minus
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'+'", "'-'"]))
        AST.DExpr'Binary
multExpr = mkBinExpr castExpr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Star -> Just AST.Star
                    Tokens.Slash -> Just AST.Slash
                    Tokens.Percent -> Just AST.Percent
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
                consumeTokU Tokens.RightArrow (XIsMissingYAfterZFound "cast expression" "'->'" "expression to cast") `seqparser` \ _ ->
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
            consumeTokS Tokens.Amper (XIsMissingYFound "reference expression" "operator '&'") `seqparser` \ ampersp ->
            consumeTokU Tokens.Mut (XIsMissingYFound "mutable reference expression" "'mut'") >>= \ mmut ->
            unaryExpr `seqparser` \ operand@(Located operandsp _) ->
            return $ Just $ Located (ampersp `joinSpan` operandsp) $ AST.DExpr'Ref ampersp (maybeToMutability mmut) operand

        punop =
            consume (\ (Located sp tok) ->
                let op = case tok of
                        Tokens.Tilde -> Just AST.UnTilde
                        Tokens.Minus -> Just AST.UnMinus
                        Tokens.Bang -> Just AST.UnBang
                        Tokens.Star -> Just AST.UnStar
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
            consumeTokS Tokens.Period (XIsMissingYAfterZFound exprName "'.'" operandName)

        field lhs@(Located lhssp _) =
            consumeDot "field access expression" "expression with fields" `seqparser` \ dot ->
            consumeIden (XIsMissingYAfterZFound "field access expression" "field name" "'.'") `seqparser` \ fieldname@(Located fieldsp _) ->
            return $ Just $ Located (lhssp `joinSpan` fieldsp) $ AST.DExpr'Field lhs dot fieldname

        method lhs@(Located lhssp _) =
            consumeDot "method call expression" "expression with methods" `seqparser` \ dot ->
            consumeIden (XIsMissingYAfterZFound "method call expression" "method name" "'.'") `seqparser` \ methodname ->
            consumeTokS Tokens.OParen (XIsMissingYAfterZFound "method call expression" "'('" "method name") `seqparser` \ oparensp ->
            argList >>= \ marglist ->
            consumeTokS Tokens.CParen (Unclosed "method call expression" "'('" oparensp) `seqparser` \ cparensp ->
            let arglist = fromMaybe [] marglist
            in return $ Just $ Located (lhssp `joinSpan` cparensp) $ AST.DExpr'Method lhs dot methodname oparensp arglist

        call lhs@(Located lhssp _) =
            consumeTokS Tokens.OParen (XIsMissingYAfterZFound "call expression" "'('" "callee") `seqparser` \ oparensp ->
            argList >>= \ marglist ->
            consumeTokS Tokens.CParen (Unclosed "call expression" "'('" oparensp) `seqparser` \ cparensp ->
            let arglist = fromMaybe [] marglist
            in return $ Just $ Located (lhssp `joinSpan` cparensp) $ AST.DExpr'Call lhs oparensp arglist

primaryExpr = choice [tokExpr, parenExpr, pathExpr]
    where
        tokExpr = consume (
                \ (Located sp tok) ->
                    let e = case tok of
                            Tokens.BoolLit b -> Just $ AST.DExpr'Bool b
                            Tokens.FloatLit f -> Just $ AST.DExpr'Float f
                            Tokens.IntLit _ i -> Just $ AST.DExpr'Int i
                            Tokens.CharLit c -> Just $ AST.DExpr'Char c
                            Tokens.StringLit s -> Just $ AST.DExpr'String s
                            Tokens.This -> Just AST.DExpr'This
                            _ -> Nothing
                    in Located sp <$> e
            ) (InvalidToken "primary expression" "token" ["a literal", "'this'"])
        parenExpr =
            consumeTokS Tokens.OParen (XIsMissingYFound "parenthesized expression" "introductory '('") `seqparser` \ oparensp ->
            parseExpr `seqparser` \ inside ->
            consumeTokU Tokens.CParen (Unclosed "parenthesized expression" "'('" oparensp) `seqparser` \ _ ->
            return $ Just inside

pathExpr =
    parsePath `seqparser` \ path@(Located pathsp _) ->
    return $ Just $ Located pathsp $ AST.DExpr'Path path
-- stmt {{{2
parseStmt, varStmt, retStmt, exprStmt :: ParseFunM AST.LDStmt
parseStmt = choice [exprStmt, varStmt, retStmt]

varStmt =
    consumeTokS Tokens.Var (XIsMissingYFound "variable statement" "introductory 'var'") `seqparser` \ varsp ->
    consumeTokU Tokens.Mut (XIsMissingYAfterZFound "variable statement" "'mut'" "'var'") >>= \ mmut ->
    consumeIden (XIsMissingYFound "variable statement" "variable name") `seqparser` \ name ->
    typeAnnotation `seqparser` \ ty ->
    (
        consumeTokS Tokens.Equal (XIsMissingYAfterZFound "variable initialization" "'='" "variable name") `seqparser` \ eqsp ->
        parseExpr `seqparser` \ initializer ->
        return $ Just (eqsp, initializer)
    ) >>= \ minit ->
    lnend "variable statement" `seqparser` \ endlsp ->
    return $ Just $ Located (varsp `joinSpan` endlsp) $ AST.DStmt'Var ty (maybeToMutability mmut) name minit

retStmt =
    consumeTokS Tokens.Return (XIsMissingYFound "return statement" "introductory 'return'") `seqparser` \ retsp ->
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
parse :: [Located Tokens.Token] -> Either ParseError AST.LDModule
parse toks =
    let (res, Parser _ _ errs) = runState grammar $ Parser 0 toks []
    in case res of
        Just x -> Right x
        Nothing -> Left $ ParseError errs
