module AST
    ( parse

    -- manipulation stuff
    , prec_of_bin_op
    , prec_of_short_op

    -- datatypes
    , ExprPrec(..)
    , BinOp(..)
    , ShortOp(..)
    , UnaryOp(..)
    , AssignOp(..)

    , SFunDecl(..)
    , SBlockExpr(..)

    , DModule(..)
    , DDecl(..)
    -- , DImplEntity(..)
    , DStmt(..)
    , DExpr(..)
    , DParam(..)
    , DType(..)
    , DPath(..)

    , LSFunDecl
    , LSBlockExpr

    , LDModule
    , LDDecl
    -- , LDImplEntity
    , LDStmt
    , LDExpr
    , LDParam
    , LDType
    , LDPath
    ) where

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
    for example, function declarations are needed in a standard Decl and also in an ImplEntity
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

data ExprPrec
    = PrecAssign | PrecBinOr | PrecBinAnd
    | PrecCompEQ | PrecCompLGT | PrecBitXor | PrecBitOr
    | PrecBitAnd | PrecBitShift | PrecAdd | PrecMult
    | PrecCast | PrecUnary | PrecCall | PrecPrimary
    deriving (Eq, Ord) -- Ord derives it as the later constructors are greater, i.e. PrecPrimary > PrecCall and PrecCall > PrecBlockLevel

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
data UnaryOp = UnBang | UnTilde | UnMinus
type LAssignOp = Located AssignOp
data AssignOp = Equal

prec_of_bin_op :: BinOp -> ExprPrec
prec_of_bin_op Plus = PrecAdd
prec_of_bin_op Minus = PrecAdd
prec_of_bin_op Star = PrecMult
prec_of_bin_op Slash = PrecMult
prec_of_bin_op Percent = PrecMult
prec_of_bin_op Greater = PrecCompLGT
prec_of_bin_op Less = PrecCompLGT
prec_of_bin_op GreaterEqual = PrecCompLGT
prec_of_bin_op LessEqual = PrecCompLGT
prec_of_bin_op Amper = PrecBitAnd
prec_of_bin_op Pipe = PrecBitOr
prec_of_bin_op Caret = PrecBitXor
prec_of_bin_op DoubleGreater = PrecBitShift
prec_of_bin_op DoubleLess = PrecBitShift
prec_of_bin_op DoubleEqual = PrecCompEQ
prec_of_bin_op BangEqual = PrecCompEQ

prec_of_short_op :: ShortOp -> ExprPrec
prec_of_short_op DoubleAmper = PrecBinAnd
prec_of_short_op DoublePipe = PrecBinOr

type LSFunDecl = Located SFunDecl
data SFunDecl = SFunDecl' (Maybe LDType) LocStr [LDParam] LSBlockExpr

type LSBlockExpr = Located SBlockExpr
data SBlockExpr = SBlockExpr' [LDStmt]

type LDModule = Located DModule
data DModule = DModule' [LDDecl]

type LDDecl = Located DDecl
data DDecl
    = DDecl'Fun LSFunDecl
    {- | DDecl'Impl LDType [LDImplEntity] -}

{-
type LDImplEntity = Located DImplEntity
data DImplEntity
    = DImplEntity'Fun LSFunDecl
-}

type LDStmt = Located DStmt
data DStmt
    = DStmt'Var LDType LocStr (Maybe LDExpr)
    | DStmt'Expr LDExpr

type LDExpr = Located DExpr
data DExpr
    = DExpr'Block LSBlockExpr
    | DExpr'If LDExpr LDExpr (Maybe LDExpr)
    | DExpr'While LDExpr LDExpr
    | DExpr'Assign LDExpr LAssignOp LDExpr
    | DExpr'ShortCircuit LDExpr LShortOp LDExpr
    | DExpr'Binary LDExpr LBinOp LDExpr
    | DExpr'Cast LDType LDExpr
    | DExpr'Unary LUnaryOp LDExpr
    -- | DExpr'Deref LDExpr
    -- | DExpr'Ref LDExpr
    | DExpr'Call LDExpr [LDExpr]
{-
    | DExpr'Field LDExpr LocStr
    | DExpr'Method LDExpr LocStr [LDExpr]
-}
    | DExpr'Bool Bool
    | DExpr'Float Double
    | DExpr'Int Integer
    | DExpr'Char Char
    | DExpr'String String
    {- | DExpr'This -}
    | DExpr'Path LDPath
    | DExpr'Ret LDExpr

type LDParam = Located DParam
data DParam
    = DParam'Normal LDType LocStr

type LDType = Located DType
data DType
    = DType'Path LDPath
    -- | DType'Pointer LDType
    {- | DType'This -}

type LDPath = Located DPath
data DPath = DPath' [LocStr]

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

data UncondError
    = ThisParamMustBeFirst Span

condition_to_sections :: ErrorConditionVariant -> ([MsgUnds.Underline], [Message.Section])

condition_to_sections (XIsMissingYFound x y sp (Located _ tok)) =
    (
        [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " (found " ++ Tokens.format_token tok ++ " instead)"]
        ],
    [])
condition_to_sections (XIsMissingYAfterZFound x y z sp (Located _ tok)) =
    (
        [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary $ x ++ " is missing " ++ y ++ " after " ++ z ++ " (found " ++ Tokens.format_token tok ++ " instead)"]
        ],
    [])
condition_to_sections (ExcessTokens sp (Located _ tok)) =
    (
        [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary $ "extraneous tokens found in input (found " ++ Tokens.format_token tok ++ ")"]
        ],
    [])
condition_to_sections (InvalidToken construct thing possibilities sp (Located _ found)) =
    (
        [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary $ "invalid " ++ thing ++ " for " ++ construct ++ "; must be " ++ format_possibilities possibilities ++ " (found " ++ Tokens.format_token found ++ " instead)"]
        ],
    [])
    where
        format_possibilities [] = error "no possibilities"
        format_possibilities [x] = x
        format_possibilities [x, y] = x ++ " or " ++ y
        format_possibilities [x, y, z] = x ++ ", " ++ y ++ ", or " ++ z
        format_possibilities (x:xs) = x ++ ", " ++ format_possibilities xs
condition_to_sections (Unclosed construct delimiter_name open_span sp (Located _ found)) =
    (
        [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary $ construct ++ " has unclosed " ++ delimiter_name ++ " (found " ++ Tokens.format_token found ++ " instead)"]
        , MsgUnds.Underline open_span [MsgUnds.Message MsgUnds.Note MsgUnds.Secondary $ "opening " ++ delimiter_name ++ " is here"]
        ],
    [])
condition_to_sections (IfContinuedNeeds listname delimiter item sp (Located _ found)) =
    (
        [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary $ listname ++ ", if continued, needs " ++ delimiter ++ " after " ++ item ++ " (found " ++ Tokens.format_token found ++ " instead)"]
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
    to_diagnostic (ParseError msgs) =
        Message.SimpleDiag Message.Error Nothing Nothing Nothing $
            Message.Underlines (MsgUnds.UnderlinesSection messages) : extra_sections
        where
            maxind = maximum $ map condloc msgs
            to_show = map condv $ combine_all $ nub $ filter ((maxind==) . condloc) msgs

            (messages, extra_sections) =
                (concatMap fst msgsecs, concatMap snd msgsecs)
                where
                    msgsecs = map condition_to_sections to_show

            condloc (ErrorCondition i _) = i
            condv (ErrorCondition _ v) = v

            combine_all = foldl' combine_once []
                where
                    combine_once acc current =
                        let combinations = [(i, j) | (i, Just j) <- zip [0..] (map (combine current) acc)]
                        in case combinations of
                            (ind, combined):_ ->
                                let (before, after) = splitAt ind acc
                                in before ++ [combined] ++ drop 1 after

                            [] -> acc ++ [current]
                                -- if current coult not combine with any other things

instance Message.ToDiagnostic UncondError where
    to_diagnostic (ThisParamMustBeFirst sp) =
        Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [ Message.Underlines $ MsgUnds.UnderlinesSection
                [ MsgUnds.Underline sp [MsgUnds.Message MsgUnds.Error MsgUnds.Primary "'this' parameters must be the first parameter"]
                ]
            ]
-- parser {{{1
data Parser = Parser Int [Located Tokens.Token] [ErrorCondition] [UncondError]
type ParseFun = State Parser
type ParseFunM a = ParseFun (Maybe a)
-- utility functions {{{1
constr_eq :: (Data a, Data b) => a -> b -> Bool
constr_eq a b = toConstr a == toConstr b

is_tt :: Tokens.Token -> Located Tokens.Token -> Bool
is_tt a (Located _ b) = constr_eq a b

is_tt_u :: Tokens.Token -> Located Tokens.Token -> Maybe ()
is_tt_u a b = if is_tt a b then Just () else Nothing

is_tt_s :: Tokens.Token -> Located Tokens.Token -> Maybe Span
is_tt_s a b@(Located sp _) = if is_tt a b then Just sp else Nothing
-- parser helpers {{{1
advance :: Int -> Parser -> Parser
advance 0 p = p
advance 1 (Parser ind toks errs uncond_errs) = Parser (ind + 1) toks errs uncond_errs
advance n p = advance 1 $ advance (n - 1) p

advance_s :: Int -> ParseFun ()
advance_s x = state $ \ parser -> ((), advance x parser)

peek :: Parser -> Located Tokens.Token
peek (Parser ind toks _ _) =
    case drop ind toks of
        x:_ -> x
        [] -> error "peek on empty token stream"

peek_s :: ParseFun (Located Tokens.Token)
peek_s = state $ \ parser -> (peek parser, parser)

new_err :: ErrorConditionVariant -> Parser -> Parser
new_err err (Parser ind toks errs uncond_errs) = Parser ind toks (errs ++ [ErrorCondition ind err]) uncond_errs

new_err_s :: ErrorConditionVariant -> ParseFun ()
new_err_s err = state $ \ parser -> ((), new_err err parser)

new_uncond_err_s :: UncondError -> ParseFun ()
new_uncond_err_s err = state $ \ (Parser i t e uncond_errs) -> ((), Parser i t e (uncond_errs ++ [err]))

get_parser :: ParseFun Parser
get_parser = state $ \ parser -> (parser, parser)

save_location :: ParseFun (Int, [Located Tokens.Token])
save_location = state $ \ parser@(Parser ind toks _ _) -> ((ind, toks), parser)

restore_location :: (Int, [Located Tokens.Token]) -> ParseFun ()
restore_location (ind, toks) = state $ \ (Parser _ _ errs uncond_errs) -> ((), Parser ind toks errs uncond_errs)

select_span_from_parser :: Parser -> Span
select_span_from_parser (Parser ind toks _ _) =
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
    peek_s >>= \ located_peeked ->
    case predicate located_peeked of
        Just x ->
            advance_s 1 >>
            return (Just x)
        Nothing ->
            get_parser >>= \ parser ->
            new_err_s (onerr (select_span_from_parser parser) located_peeked) >>
            return Nothing
-- consume combinators {{{
consume_tok_u :: Tokens.Token -> (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM ()
consume_tok_u tok = consume (is_tt_u tok)

consume_tok_s :: Tokens.Token -> (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM Span
consume_tok_s tok = consume (is_tt_s tok)

consume_iden :: (Span -> Located Tokens.Token -> ErrorConditionVariant) -> ParseFunM (Located String)
consume_iden = consume $
    \case
        Located sp (Tokens.Identifier n) -> Just $ Located sp n
        _ -> Nothing
-- }}}


seqparser :: ParseFunM a -> (a -> ParseFunM b) -> ParseFunM b
seqparser exa cont =
    save_location >>= \ saved ->
    exa >>= \case
        Just res ->
            cont res >>= \case
                jx@(Just _) -> return jx
                Nothing ->
                    restore_location saved >>
                    return Nothing
        Nothing ->
            restore_location saved >>
            return Nothing

-- TODO: allow error override, if used then silence all choice's errors, and if none of them match then emit that custom error
choice :: [ParseFunM a] -> ParseFunM a
choice choices =
    save_location >>= try_choices choices
    where
        try_choices (c:cs) original_location =
            restore_location original_location >>
            c >>= \case
                Just x -> return (Just x)
                Nothing -> try_choices cs original_location

        try_choices [] original_location =
            restore_location original_location >>
            return Nothing

zeromore :: ParseFunM a -> ParseFun [a]
zeromore ex = fun
    where
        fun =
            save_location >>= \ saved ->
            ex >>= \case
                Just res ->
                    fun >>= \ rest ->
                    return $ res:rest

                Nothing ->
                    restore_location saved >>
                    return []

onemore :: ParseFunM a -> ParseFunM [a]
onemore ex = fun []
    where
        fun acc =
            save_location >>= \ saved ->
            ex >>= \case
                Just thing ->
                    fun $ acc ++ [thing]

                Nothing ->
                    restore_location saved >>
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

must_match :: ParseFunM a -> ParseFunM ()
must_match ex =
    save_location >>= \ saved ->
    ex >>= \ res ->
    restore_location saved >>
    return (const () <$> res)

must_not_match :: ParseFunM a -> (Span -> ErrorConditionVariant) -> ParseFunM ()
must_not_match ex onerr =
    save_location >>= \ saved ->
    ex >>= \ res ->
    restore_location saved >>
    case res of
        Just _ ->
            get_parser >>= \ parser ->
            new_err_s (onerr $ select_span_from_parser parser) >>
            return Nothing

        Nothing ->
            return (Just ())
-}
-- grammar {{{1
-- span helpers {{{
span_from_list :: Span -> [Located a] -> Span
span_from_list fallback [] = fallback
span_from_list _ list =
    join_span a b
    where
        Located a _ = head list
        Located b _ = last list

-- }}}
grammar :: ParseFunM AST.LDModule
grammar =
    decl_list >>= \ mdl ->
    consume_tok_s Tokens.EOF ExcessTokens `seqparser` \ eofsp ->
    case mdl of
        Just dl -> return $ Just $ Located (span_from_list eofsp dl) (AST.DModule' dl)
        Nothing -> return $ Just $ Located eofsp $ AST.DModule' []
-- lists {{{2
decl_list :: ParseFunM [AST.LDDecl]
decl_list = onemore parse_decl

param_list :: ParseFunM [AST.LDParam]
param_list =
    onemoredelim parse_param (consume_tok_u Tokens.Comma (IfContinuedNeeds "parameter list" "separator ','" "parameter")) `seqparser` \ params ->
    mapM (
        \ (p@(Located param_sp param), idx) ->
        case param of
            DParam'Normal _ (Located _ "this")
                | idx /= 0 ->
                    new_uncond_err_s (ThisParamMustBeFirst param_sp) >>
                    return Nothing
            _ -> return $ Just p
    ) (zip params ([0..] :: [Integer])) >>= \ m_params ->
    return $ sequence m_params


stmt_list :: ParseFunM [AST.LDStmt]
stmt_list = onemore parse_stmt

arg_list :: ParseFunM [AST.LDExpr]
arg_list = onemoredelim parse_expr (consume_tok_u Tokens.Comma (IfContinuedNeeds "argument list" "separator ','" "argument"))
-- line endings {{{2
lnend :: String -> ParseFunM Span
lnend what = consume
    (\case
        Located sp Tokens.Newline -> Just sp
        Located sp Tokens.Semicolon -> Just sp
        _ -> Nothing
    ) (XIsMissingYFound what "terminator (newline or ';')")
-- blocks {{{2
blocked, braced, indented :: String -> ParseFun a -> ParseFunM (Span, a)
blocked what ex = choice [braced what ex, indented what ex]

braced what ex =
    consume_tok_s Tokens.OBrace (XIsMissingYFound what "opening '{'") `seqparser` \ obracesp ->
    ex >>= \ inside ->
    consume_tok_s Tokens.CBrace (Unclosed what "'{'" obracesp) `seqparser` \ cbracesp ->
    return $ Just (obracesp `join_span` cbracesp, inside)

indented what ex =
    consume_tok_s Tokens.Indent (XIsMissingYFound what "opening indent") `seqparser` \ indentsp ->
    ex >>= \ inside ->
    consume_tok_s Tokens.Dedent (Unclosed what "indent" indentsp) `seqparser` \ dedentsp ->
    return $ Just (indentsp `join_span` dedentsp, inside)
-- decl {{{2
parse_decl :: ParseFunM AST.LDDecl
parse_decl =
    choice
        [ {- impl_decl
        , -} function_decl `seqparser` \ lsfd@(Located sp _) ->
          return $ Just $ Located sp $ AST.DDecl'Fun lsfd
        ]

function_decl :: ParseFunM AST.LSFunDecl
function_decl =
    consume_tok_s Tokens.Fun (XIsMissingYFound "function declaration" "introductory 'fun'") `seqparser` \ funsp ->
    consume_iden (XIsMissingYAfterZFound "function declaration" "function name" "'fun'") `seqparser` \ name ->
    consume_tok_s Tokens.OParen (XIsMissingYAfterZFound "function declaration" "'('" "function name") `seqparser` \ oparensp ->
    param_list >>= \ mparamlist ->
    consume_tok_s Tokens.CParen (Unclosed "function declaration parameter list" "'('" oparensp) `seqparser` \ cparensp ->
    type_annotation >>= \ retty ->
    block_expr `seqparser` \ body ->
    lnend "function declaration" >>
    (let params = fromMaybe [] mparamlist
         fdsp = join_span funsp $
            case retty of
                Just (Located rettysp _) -> rettysp
                Nothing -> cparensp

    in return $ Just $ Located fdsp $ AST.SFunDecl' retty name params body)

{-
impl_decl :: ParseFunM AST.LDDecl
impl_decl =
    consume_tok_s Tokens.Impl (XIsMissingYFound "implementation block" "introductory 'impl'") `seqparser` \ implsp ->
    parse_type `seqparser` \ impl_for@(Located tysp _)->
    impl_body `seqparser` \ (_, body) ->
    return $ Just $ Located (join_span implsp tysp) $ AST.DDecl'Impl impl_for body
    where
        impl_body = blocked "implementation body" impl_list
        impl_list = zeromore $
            choice
                [ function_decl `seqparser` \ lsfd@(Located sp _) ->
                  return $ Just $ Located sp $ AST.DImplEntity'Fun lsfd
                ]
-}
-- types {{{2
type_annotation :: ParseFunM AST.LDType
type_annotation =
    consume_tok_u Tokens.Colon (XIsMissingYFound "type annotation" "introductory ':'") `seqparser` \ _ ->
    parse_type

parse_type, {- pointer_type, this_type, -} path_type :: ParseFunM AST.LDType
parse_type = choice [{- pointer_type, this_type, -} path_type]

{-
pointer_type =
    consume_tok_s Tokens.Star (XIsMissingYFound "pointer type" "introductory '*'") `seqparser` \ starsp ->
    -- consume_tok_u Tokens.Mut (XIsMissingYAfterZFound "mutable pointer type" "'mut'" "'*'") >>= \ mmut ->
    parse_type `seqparser` \ pointee_ty@(Located pointeesp _) ->
    return $ Just $ Located (join_span starsp pointeesp) $ AST.DType'Pointer {- (maybe_to_mutability mmut) -} pointee_ty
-}

{-
this_type =
    consume_tok_s Tokens.This (XIsMissingYFound "'this' type" "'this'") `seqparser` \ thsp ->
    return $ Just $ Located thsp AST.DType'This
-}

path_type =
    parse_path `seqparser` \ path@(Located pathsp _) ->
    return $ Just $ Located pathsp $ AST.DType'Path path
-- paths {{{2
parse_path :: ParseFunM AST.LDPath
parse_path =
    onemoredelim
        (consume_iden (XIsMissingYFound "path" "path segment (identifier)"))
        (consume_tok_u Tokens.DoubleColon (IfContinuedNeeds "path" "segment separator ('::')" "segment (identifier)"))
        `seqparser` \ list ->
    let totalsp = span_from_list (error "path should always have at least one element") list
    in return $ Just $ Located totalsp $ AST.DPath' list
-- params {{{2
parse_param, normal_param {-, this_param -} :: ParseFunM AST.LDParam
parse_param = choice [normal_param {-, this_param -}]

normal_param =
    -- consume_tok_s Tokens.Mut (XIsMissingYFound "mutable parameter" "'mut'") >>= \ mmut ->
    consume_iden (XIsMissingYFound "parameter" "parameter name") `seqparser` \ name@(Located namesp _) ->
    type_annotation `seqparser` \ ty@(Located tysp _) ->
    return $ Just $ Located (namesp `join_span` tysp) $ AST.DParam'Normal {- (maybe_to_mutability mmut) -} ty name

{-
this_param =
    (
        consume_tok_s Tokens.Star (XIsMissingYFound "'this' reference parameter" "'*'") `seqparser` \ starsp ->
        consume_tok_u Tokens.Mut (XIsMissingYAfterZFound "'this' mutable reference parameter" "'mut'" "'*'") >>= \ mmut ->
        return $ Just $ Located starsp mmut
    ) >>= \ mstarmut ->
    consume_tok_s Tokens.This (XIsMissingYFound "'this' parameter" "'this'") `seqparser` \ thissp ->
    let plain_this_ty = Located thissp AST.DType'This
        (mstartsp, ty) = case mstarmut of
            Just (Located starsp mutable) -> (
                    Just starsp,
                    Located (starsp `join_span` endsp) $ AST.DType'Pointer (maybe_to_mutability mutable) plain_this_ty
                )

            Nothing -> (Nothing, plain_this_ty)

        ty :: AST.LDType

        startsp = fromMaybe thissp mstartsp
        endsp = thissp

    in return $ Just $ Located (startsp `join_span` endsp) $ AST.DParam'Normal AST.Immutable ty (Located thissp "this")
-}
-- expr {{{2
parse_expr :: ParseFunM AST.LDExpr
parse_expr = assign_expr

mk_bin_expr :: ParseFunM AST.LDExpr -> ParseFunM a -> (AST.LDExpr -> a -> AST.LDExpr -> AST.DExpr) -> ParseFunM AST.LDExpr
mk_bin_expr next operators constructor =
    next `seqparser` \ lhs ->
    maybe_next_rep lhs
    where
        maybe_next_rep lhs@(Located lhssp _) =
            (
                operators `seqparser` \ op ->
                next `seqparser` \ rhs ->
                return $ Just (op, rhs)
            ) >>= \case
                Just (op, rhs@(Located rhssp _)) -> maybe_next_rep $ Located (lhssp `join_span` rhssp) $ constructor lhs op rhs
                Nothing -> return $ Just lhs

assign_expr, bin_or_expr, bin_and_expr, comp_eq_expr, comp_lgt_expr, bit_xor_expr, bit_or_expr, bit_and_expr, bit_shift_expr, addition_expr, mult_expr, cast_expr, unary_expr, call_expr, primary_expr, path_expr :: ParseFunM AST.LDExpr

assign_expr =
    bin_or_expr `seqparser` \ lhs@(Located lhssp _) ->
    (
        consume (\ (Located sp tok) ->
        case tok of
            Tokens.Equal -> Just $ Located sp AST.Equal
            _ -> Nothing
        ) (InvalidToken "expression" "operator" ["'='"]) `seqparser` \ op ->
        assign_expr `seqparser` \ rhs ->
        return $ Just (op, rhs)
    ) >>= \ mrhs ->
    return $ Just (case mrhs of
        Just (op, rhs@(Located rhssp _)) -> Located (lhssp `join_span` rhssp) $ AST.DExpr'Assign lhs op rhs
        Nothing -> lhs
    )

bin_or_expr = mk_bin_expr bin_and_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.DoublePipe -> Just AST.DoublePipe
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'||'"]))
        AST.DExpr'ShortCircuit
bin_and_expr = mk_bin_expr comp_eq_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.DoubleAmper -> Just AST.DoubleAmper
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'&&'"]))
        AST.DExpr'ShortCircuit
comp_eq_expr = mk_bin_expr comp_lgt_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.BangEqual -> Just AST.BangEqual
                    Tokens.DoubleEqual -> Just AST.DoubleEqual
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'!='", "'=='"]))
        AST.DExpr'Binary
comp_lgt_expr = mk_bin_expr bit_xor_expr
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
bit_xor_expr = mk_bin_expr bit_or_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Caret -> Just AST.Caret
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'^'"]))
        AST.DExpr'Binary
bit_or_expr = mk_bin_expr bit_and_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Pipe -> Just AST.Pipe
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'|'"]))
        AST.DExpr'Binary
bit_and_expr = mk_bin_expr bit_shift_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Amper -> Just AST.Amper
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'&'"]))
        AST.DExpr'Binary
bit_shift_expr = mk_bin_expr addition_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.DoubleLess -> Just AST.DoubleLess
                    Tokens.DoubleGreater -> Just AST.DoubleGreater
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'<<'", "'>>'"]))
        AST.DExpr'Binary
addition_expr = mk_bin_expr mult_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Plus -> Just AST.Plus
                    Tokens.Minus -> Just AST.Minus
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'+'", "'-'"]))
        AST.DExpr'Binary
mult_expr = mk_bin_expr cast_expr
        (consume (\ (Located sp tok) ->
            let op = case tok of
                    Tokens.Star -> Just AST.Star
                    Tokens.Slash -> Just AST.Slash
                    Tokens.Percent -> Just AST.Percent
                    _ -> Nothing
            in Located sp <$> op
        ) (InvalidToken "expression" "operator" ["'*'", "'/'", "'%'"]))
        AST.DExpr'Binary

cast_expr =
    unary_expr `seqparser` \ lhs ->
    parse_more lhs
    where
        parse_more lhs@(Located lhssp _) =
            (
                consume_tok_u Tokens.RightArrow (XIsMissingYAfterZFound "cast expression" "'->'" "expression to cast") `seqparser` \ _ ->
                parse_type `seqparser` \ ty ->
                return $ Just ty
            ) >>= \case
                Just ty@(Located tysp _) -> parse_more $ Located (lhssp `join_span` tysp) $ AST.DExpr'Cast ty lhs
                Nothing -> return $ Just lhs

unary_expr =
    choice [punop, call_expr]
    where
        {-
        amper_expr =
            consume_tok_s Tokens.Amper (XIsMissingYFound "reference expression" "operator '&'") `seqparser` \ ampersp ->
            -- consume_tok_u Tokens.Mut (XIsMissingYFound "mutable reference expression" "'mut'") >>= \ mmut ->
            unary_expr `seqparser` \ operand@(Located operandsp _) ->
            return $ Just $ Located (ampersp `join_span` operandsp) $ AST.DExpr'Ref {- (maybe_to_mutability mmut) -} operand

        deref_expr =
            consume_tok_s Tokens.Star (XIsMissingYFound "dereference expression" "operator '*'") `seqparser` \ starsp ->
            unary_expr `seqparser` \ operand@(Located operandsp _) ->
            return $ Just $ Located (starsp `join_span` operandsp) $ AST.DExpr'Deref operand
        -}

        punop =
            consume (\ (Located sp tok) ->
                let op = case tok of
                        Tokens.Tilde -> Just AST.UnTilde
                        Tokens.Minus -> Just AST.UnMinus
                        Tokens.Bang -> Just AST.UnBang
                        _ -> Nothing
                in Located sp <$> op
            ) (InvalidToken "unary expression" "operator" ["'~'", "'-'", "'!'", "'*'"]) `seqparser` \ op@(Located opsp _) ->
            unary_expr `seqparser` \ operand@(Located operandsp _) ->
            return $ Just $ Located (opsp `join_span` operandsp) $ AST.DExpr'Unary op operand

call_expr =
    primary_expr `seqparser` \ lhs ->
    parse_more lhs
    where
        parse_more lhs =
            choice [{- method lhs, field lhs, -} call lhs] >>= \case
                Just newlhs -> parse_more newlhs
                Nothing -> return $ Just lhs

        {-
        consume_dot expr_name operand_name =
            consume_tok_s Tokens.Period (XIsMissingYAfterZFound expr_name "'.'" operand_name)

        field lhs@(Located lhssp _) =
            consume_dot "field access expression" "expression with fields" `seqparser` \ _ ->
            consume_iden (XIsMissingYAfterZFound "field access expression" "field name" "'.'") `seqparser` \ fieldname@(Located fieldsp _) ->
            return $ Just $ Located (lhssp `join_span` fieldsp) $ AST.DExpr'Field lhs fieldname

        method lhs@(Located lhssp _) =
            consume_dot "method call expression" "expression with methods" `seqparser` \ _ ->
            consume_iden (XIsMissingYAfterZFound "method call expression" "method name" "'.'") `seqparser` \ methodname ->
            consume_tok_s Tokens.OParen (XIsMissingYAfterZFound "method call expression" "'('" "method name") `seqparser` \ oparensp ->
            arg_list >>= \ marglist ->
            consume_tok_s Tokens.CParen (Unclosed "method call expression" "'('" oparensp) `seqparser` \ cparensp ->
            let arglist = fromMaybe [] marglist
            in return $ Just $ Located (lhssp `join_span` cparensp) $ AST.DExpr'Method lhs methodname arglist
        -}

        call lhs@(Located lhssp _) =
            consume_tok_s Tokens.OParen (XIsMissingYAfterZFound "call expression" "'('" "callee") `seqparser` \ oparensp ->
            arg_list >>= \ marglist ->
            consume_tok_s Tokens.CParen (Unclosed "call expression" "'('" oparensp) `seqparser` \ cparensp ->
            let arglist = fromMaybe [] marglist
            in return $ Just $ Located (lhssp `join_span` cparensp) $ AST.DExpr'Call lhs arglist

primary_expr =
    choice
    [ tok_expr
    , paren_expr
    , path_expr
    , if_expr
    , while_expr
    , block_expr `seqparser` \ bl@(Located blsp _) ->
      return $ Just $ Located blsp $ AST.DExpr'Block bl
    , ret_expr
    ]
    where
        tok_expr = consume (
                \ (Located sp tok) ->
                    let e = case tok of
                            Tokens.BoolLit b -> Just $ AST.DExpr'Bool b
                            Tokens.FloatLit f -> Just $ AST.DExpr'Float f
                            Tokens.IntLit _ i -> Just $ AST.DExpr'Int i
                            Tokens.CharLit c -> Just $ AST.DExpr'Char c
                            Tokens.StringLit s -> Just $ AST.DExpr'String s
                            {- Tokens.This -> Just AST.DExpr'This -}
                            _ -> Nothing
                    in Located sp <$> e
            ) (InvalidToken "primary expression" "token" ["a literal", "'this'"])
        paren_expr =
            consume_tok_s Tokens.OParen (XIsMissingYFound "parenthesized expression" "introductory '('") `seqparser` \ oparensp ->
            parse_expr `seqparser` \ inside ->
            consume_tok_u Tokens.CParen (Unclosed "parenthesized expression" "'('" oparensp) `seqparser` \ _ ->
            return $ Just inside

block_stmt_list :: ParseFunM (Span, [AST.LDStmt])
block_stmt_list =
    blocked "code block" (
        stmt_list >>= \ sl ->
        return $ fromMaybe [] sl
    )

block_expr :: ParseFunM AST.LSBlockExpr
block_expr =
    block_stmt_list `seqparser` \ (slsp, sl) ->
    return $ Just $ Located slsp $ AST.SBlockExpr' sl

if_expr, while_expr, ret_expr :: ParseFunM AST.LDExpr

if_expr =
    consume_tok_s Tokens.If (XIsMissingYFound "'if' expression" "'if'") `seqparser` \ ifsp ->
    parse_expr `seqparser` \ cond ->
    block_expr `seqparser` \ trueb@(Located truebsp _) ->
    (
        consume_tok_s Tokens.Else (XIsMissingYFound "'else' branch of 'if' expression" "'else'") `seqparser` \ _ ->
        choice
            [ block_expr `seqparser` \ block@(Located blocksp _) ->
              return $ Just $ Located blocksp $ AST.DExpr'Block block
            , if_expr
            ] `seqparser` \ falseb@(Located falsebsp _) ->
        return $ Just (falsebsp, falseb)
    ) >>= \ melsebandspan ->
    let startsp = ifsp
        endsp = maybe truebsp fst melsebandspan
        melseb = snd <$> melsebandspan

        loctrueb = Located truebsp $ AST.DExpr'Block trueb
    in return $ Just $ Located (startsp `join_span` endsp) $ AST.DExpr'If cond loctrueb melseb

while_expr =
    consume_tok_s Tokens.While (XIsMissingYFound "'while' expression" "'while'") `seqparser` \ whilesp ->
    parse_expr `seqparser` \ cond ->
    block_expr `seqparser` \ block@(Located blocksp _) ->
    return $ Just $ Located (whilesp `join_span` blocksp) $ AST.DExpr'While cond (Located blocksp $ AST.DExpr'Block block)

ret_expr =
    consume_tok_s Tokens.Return (XIsMissingYFound "return expression" "introductory 'return'") `seqparser` \ retsp ->
    parse_expr `seqparser` \ expr@(Located exprsp _) ->
    return $ Just $ Located (retsp `join_span` exprsp) $ AST.DExpr'Ret expr

path_expr =
    parse_path `seqparser` \ path@(Located pathsp _) ->
    return $ Just $ Located pathsp $ AST.DExpr'Path path
-- stmt {{{2
parse_stmt, var_stmt, expr_stmt :: ParseFunM AST.LDStmt
parse_stmt = choice [expr_stmt, var_stmt]

var_stmt =
    consume_tok_s Tokens.Let (XIsMissingYFound "variable statement" "introductory 'let'") `seqparser` \ varsp ->
    -- consume_tok_u Tokens.Mut (XIsMissingYAfterZFound "variable statement" "'mut'" "'let'") >>= \ mmut ->
    consume_iden (XIsMissingYFound "variable statement" "variable name") `seqparser` \ name ->
    type_annotation `seqparser` \ ty ->
    (consume_tok_s Tokens.Equal (XIsMissingYAfterZFound "variable initialization" "'='" "variable name") `seqparser` \ _ -> parse_expr) >>= \ initializer ->
    lnend "variable statement" `seqparser` \ endlsp ->
    return $ Just $ Located (varsp `join_span` endlsp) $ AST.DStmt'Var ty {- (maybe_to_mutability mmut) -} name initializer

expr_stmt =
    parse_expr `seqparser` \ expr@(Located exprsp not_located_expr) ->
    let needendl = case not_located_expr of
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
            Just sp -> exprsp `join_span` sp
            Nothing -> exprsp
    in return $ Just $ Located totalsp $ AST.DStmt'Expr expr

-- parse {{{1
parse :: [Located Tokens.Token] -> Either [Message.SimpleDiag] AST.LDModule
parse toks =
    let (res, Parser _ _ errs uncond_errs) = runState grammar $ Parser 0 toks [] []
        uncond_errs_simple = map Message.to_diagnostic uncond_errs
    in case (res, uncond_errs) of
        (Just x, []) -> Right x
        (Nothing, []) -> Left [Message.to_diagnostic (ParseError errs)]
        (_, _:_) -> Left uncond_errs_simple
