module AST.Parsing
    ( parse_from_toks
    , ParseError

    , tests
    ) where

import Test

import File
import Location
import MonadUtils
import MaybeWithError

import AST.Datatypes

import qualified Tokens
import qualified Message
import qualified Message.Underlines as MsgUnds

import Data.Data (toConstr, Data)
import Data.List (find)

import Control.Monad.State.Lazy (State, state, runState)

-- ParseError {{{1
data ParseError
    = Expected String Span
    | TestError
    | TestErrorWithSpan Span

type ParseErrorSpanPending = Span -> ParseError

instance Message.ToDiagnostic ParseError where
    to_diagnostic (Expected thing sp) =
        Message.SimpleDiag Message.Error (Just sp) Nothing Nothing
            [ Message.Underlines
                [ MsgUnds.Underline sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error $ "expected " ++ thing]
                ]
            ]

    to_diagnostic TestError = error "cannot convert TestError to SimpleDiag"
    to_diagnostic (TestErrorWithSpan _) = error "cannot convert TestErrorWithSpan to SimpleDiag"

-- Parser {{{1
data Parser
    = Parser
      { get_parse_errors :: [ParseError]
      , get_token_stream :: [Located Tokens.Token]
      , last_token :: Maybe (Located Tokens.Token)
      }
-- type synonyms {{{1
type ParseFun = State Parser
type ParseFunM a = ParseFun (Maybe a)
type ParseFunMWE a = ParseFun (MaybeWithError a ParseError)

type ParserPredicate = Parser -> Bool
type SynchronizationPredicate = ParserPredicate
type StopPredicate = ParserPredicate

type TokenPredicate = Located Tokens.Token -> Bool
-- run_parse_fun {{{1
run_parse_fun :: ParseFun a -> [Located Tokens.Token] -> ([ParseError], a)
run_parse_fun pf tokens =
    let parser = Parser [] tokens Nothing
        (res, Parser errs _ _) = runState pf parser
    in (reverse errs, res)
-- error helpers {{{1
add_error :: ParseError -> ParseFun (ErrorLoggedPromise ParseError)
add_error err = state $ \ parser -> (error_logged_promise err, parser { get_parse_errors = err : get_parse_errors parser })

add_error_and_nothing :: ParseError -> ParseFunMWE r
add_error_and_nothing err = NothingWithError <$> add_error err
-- span_from_list {{{1
span_from_list :: [Located a] -> Span -> Span
span_from_list [] s = s
span_from_list items _ = get_span (head items) `join_span` get_span (last items)

-- predicates {{{1
constr_eq :: (Data a, Data b) => a -> b -> Bool
constr_eq a b = toConstr a == toConstr b

is_tt :: Tokens.Token -> TokenPredicate
is_tt a (Located _ b) = constr_eq a b

is_at_end :: ParserPredicate
is_at_end = peek_matches_pred $ is_tt Tokens.EOF
-- predicate combinators {{{1
predicate_or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
predicate_or p1 p2 a = p1 a || p2 a

predicate_and :: (a -> Bool) -> (a -> Bool) -> a -> Bool
predicate_and p1 p2 a = p1 a && p2 a

peek_matches_pred :: TokenPredicate -> ParserPredicate
peek_matches_pred predicate = predicate . head . get_token_stream
-- basic functions {{{1
peek :: ParseFun (Located Tokens.Token)
peek = state $ \ parser -> (head $ get_token_stream parser, parser)

peek_span :: ParseFun Span
peek_span = get_span <$> peek

peek_matches :: TokenPredicate -> ParseFun Bool
peek_matches predicate = predicate <$> peek

peek_match :: [(TokenPredicate, ParseFun a)] -> ParseFun a -> ParseFun a
peek_match predicates fallback =
    peek >>= \ tok ->
    let results = map (\ (predicate, act) -> (predicate tok, act)) predicates
    in case find fst results of
        Just (_, act) -> act
        Nothing -> fallback


parser_matches :: ParserPredicate -> ParseFun Bool
parser_matches predicate = state $ \ parser -> (predicate parser, parser)

advance :: ParseFun (Located Tokens.Token)
advance = state $ \ parser ->
    let l:ts = get_token_stream parser
    in if null ts
        then error "advance past last token of the token stream"
        else
            ( l
            , parser
             { get_token_stream = ts
             , last_token = Just l
             }
            )

advance_match :: [(TokenPredicate, Located Tokens.Token -> ParseFun a)] -> ParseFun a -> ParseFun a
advance_match predicates fallback =
    peek >>= \ tok ->
    let results = map (\ (predicate, act) -> (predicate tok, act)) predicates
    in case find fst results of
        Just (_, act) -> advance >>= act
        Nothing -> fallback

consume :: TokenPredicate -> ParseFunM (Located Tokens.Token)
consume predicate =
    peek_matches predicate >>= \case
        True -> Just <$> advance
        False -> return Nothing

consume_or_error :: ParseErrorSpanPending -> TokenPredicate -> ParseFunMWE (Located Tokens.Token)
consume_or_error err predicate =
    consume predicate >>= \case
        Just tok -> return $ JustWithError tok
        Nothing -> peek_span >>= add_error_and_nothing . err

synchronize :: SynchronizationPredicate -> ParseFun ()
synchronize sync_predicate = advance >> go
    where
        go =
            parser_matches (sync_predicate `predicate_or` is_at_end) >>= \case
                True -> return ()
                False -> advance >> go
-- combinators {{{1
braced, indented, blocked :: ParseFunMWE a -> ParseFunMWE (Span, a)
(braced, indented) = (surround_with Tokens.OBrace Tokens.CBrace, surround_with Tokens.Indent Tokens.Dedent)
    where
        surround_with :: Tokens.Token -> Tokens.Token -> ParseFunMWE a -> ParseFunMWE (Span, a)
        surround_with open close pf =
            consume_or_error (Expected $ Tokens.format_token open) (is_tt open) >>=??> \ (Located open_sp _) ->
            pf >>=??> \ inside ->
            consume_or_error (Expected $ Tokens.format_token close) (is_tt close) >>=??> \ (Located close_sp _) ->
            return (JustWithError (open_sp `join_span` close_sp, inside))

blocked pf =
    peek_span >>= \ next_span ->
    peek_match
        [ (is_tt Tokens.OBrace, braced pf)
        , (is_tt Tokens.Indent, indented pf)
        ]
        (add_error_and_nothing (Expected "block" next_span))

thing_list_no_separator :: StopPredicate -> SynchronizationPredicate -> ParseFunMWE a -> ParseFun [a]
thing_list_no_separator stop_predicate sync_predicate pf = go []
    where
        go things =
            parser_matches (stop_predicate `predicate_or` is_at_end) >>= \case
                True -> return things

                False ->
                    pf >>= \case
                        JustWithError thing ->
                            go $ things ++ [thing]
                        NothingWithError _ ->
                            synchronize sync_predicate >>
                            go things

thing_list_with_separator :: TokenPredicate -> ParseFunMWE a -> ParseFun [a]
thing_list_with_separator delim_predicate pf = go []
    where
        -- TODO: maybe synchronize to delimiter on error
        go things =
            peek_matches delim_predicate >>= \case
                True ->
                    pf >>= \case
                        JustWithError thing -> go (things ++ [thing])
                        NothingWithError _ -> go things

                False -> return things
-- line endings {{{1
line_ending :: ParseFunMWE Span
line_ending =
    peek_span >>= \ next_span ->
    advance_match
        [ (is_tt Tokens.Newline, return . JustWithError . get_span)
        , (is_tt Tokens.Semicolon, return . JustWithError . get_span)
        ]
        (add_error_and_nothing (Expected "line ending" next_span))

optional_line_ending :: ParseFunM Span
optional_line_ending =
    advance_match
        [ (is_tt Tokens.Newline, return . Just . get_span)
        , (is_tt Tokens.Semicolon, return . Just . get_span)
        ]
        (return Nothing)
-- parse_type {{{1
parse_type :: ParseFunMWE LDType
parse_type = undefined
-- parse_type_annotation {{{1
parse_type_annotation :: ParseFunMWE LDType
parse_type_annotation = consume_or_error (Expected "':'") (is_tt Tokens.Colon) >>=??> \ _ -> parse_type
-- parse_param {{{1
parse_param :: ParseFunMWE LDParam
parse_param =
    consume_or_error (Expected "parameter name") (is_tt $ Tokens.Identifier "") >>=??> \ (Located name_sp (Tokens.Identifier name)) ->
    parse_type_annotation >>=??> \ ty ->
    return (JustWithError $ Located (name_sp `join_span` get_span ty) (DParam'Normal ty (Located name_sp name)))
-- parse_expr {{{1
-- parse_block {{{2
parse_block :: ParseFunMWE LSBlockExpr
parse_block = undefined
-- parse_decl {{{1
parse_decl :: ParseFunMWE LDDecl
parse_decl =
    peek_span >>= \ next_span ->
    advance_match
        [ (is_tt Tokens.Fun,
            \ fun_tok ->
                parse_fun fun_tok >>=??> \ fun@(Located fun_sp _) ->
                return (JustWithError $ Located fun_sp (DDecl'Fun fun))
          )
        ]
        (add_error_and_nothing (Expected "a declaration" next_span))
-- parse_fun {{{2
parse_fun :: Located Tokens.Token -> ParseFunMWE LSFunDecl
parse_fun (Located fun_sp _) =
    consume_or_error (Expected "function name") (is_tt (Tokens.Identifier "")) >>=??> \ (Located name_sp (Tokens.Identifier name)) ->
    consume_or_error (Expected "'('") (is_tt Tokens.OParen) >>=??> \ _ ->

    thing_list_with_separator (is_tt Tokens.Comma) parse_param >>= \ params ->

    consume_or_error (Expected "')'") (is_tt Tokens.CParen) >>=??> \ (Located cparen_sp _) ->

    (peek_matches (is_tt Tokens.Colon) >>= \case
        True -> parse_type_annotation >>=?? return Nothing $ return . Just
        False -> return Nothing
    ) >>= \ return_type ->

    parse_block >>=??> \ body ->
    optional_line_ending >>
    return (JustWithError $ Located (fun_sp `join_span` cparen_sp) (SFunDecl' return_type (Located name_sp name) params body))
-- parse_module {{{1
parse_module :: ParseFun LDModule
parse_module =
    thing_list_no_separator
        (const False) -- stop predicate: will never stop until EOF
        (peek_matches_pred $ is_tt Tokens.Fun) -- synchronization predicate: will stop before 'fun'
        parse_decl
     >>= \ decl_list ->
    peek >>= \ (Located eof_span _) ->
    return (Located (span_from_list decl_list eof_span) (DModule' decl_list))
-- parsing entry point{{{1
parse_from_toks :: [Located Tokens.Token] -> ([ParseError], LDModule)
parse_from_toks = run_parse_fun parse_module
-- tests {{{1
tests :: Test
tests =
    DescribeModule "AST.Parsing"
        [ DescribeFunction "add_error"
            [ ItCan "add an error and return a promise" $
                let (errs, promise) = run_parse_fun (add_error TestError) []
                in case errs of
                    [TestError] -> seq promise pass_test -- if promise does not throw, it is not undefined and is an actual promise
                    _ -> fail_test
            ]

        , DescribeFunction "add_error_and_nothing"
            [ ItCan "add an error and return NothingWithError" $
                let res = run_parse_fun (add_error_and_nothing TestError) []
                in case res of
                    ([TestError], NothingWithError _) -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "span_from_list"
            [ ItCan "join the span of the first and last elements of a list of located items" $
                let fake_file = make_file "fake_file" "a b c d e f"

                    span1 = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 3
                            end = make_location_from_ind fake_file 4
                    span2 = Span start before end
                        where
                            start = make_location_from_ind fake_file 5
                            before = make_location_from_ind fake_file 6
                            end = make_location_from_ind fake_file 7

                    things = [Located span1 (), undefined, Located span2 ()]
                -- TODO: use pass_if
                in if span_from_list things undefined == (span1 `join_span` span2)
                    then pass_test
                    else fail_test

            , ItCan "use the fallback span if there is an empty list" $
                let fake_file = make_file "fake_file" "a b c d e f"
                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 1
                            end = make_location_from_ind fake_file 2
                in if span_from_list [] sp == sp
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "constr_eq"
            [ ItCan "return true if the two arguments are of the same constructor" $
                if constr_eq (Just ()) (Just 2 :: Maybe Int)
                    then pass_test
                    else fail_test

            , ItCan "return false if the two arguments are not of the same constructor" $
                if not $ constr_eq (Just ()) (Nothing :: Maybe ())
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "is_tt"
            [ ItCan "tell whether a token is a certain token type" $
                if is_tt (Tokens.Identifier "") (Located undefined $ Tokens.Identifier "abc")
                    then pass_test
                    else fail_test

            , ItCan "tell whether a token is not a certain token type" $
                if not $ is_tt Tokens.OParen (Located undefined Tokens.CParen)
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "is_at_end"
            [ ItCan "tell if the next token is an EOF token" $
                let (_, pred_result) = run_parse_fun (parser_matches is_at_end) [Located undefined Tokens.EOF]
                in if pred_result
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "predicate_or"
            [ ItCan "combine predicates with an or" $
                let combined = predicate_or odd even
                in if combined (2 :: Int) && combined 3
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "predicate_and"
            [ ItCan "combine predicates with an and" $
                let combined = predicate_and odd even
                in if not (combined (2 :: Int)) && not (combined 3)
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "peek_matches_pred"
            [ ItCan "convert a token predicate into a parser predicate" $
                let (_, pred_result) = run_parse_fun (parser_matches $ peek_matches_pred (is_tt Tokens.OParen)) [Located undefined Tokens.OParen]
                in if pred_result
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "peek"
            [ ItCan "return the next token" $
                let fake_file = make_file "fake_file" "a"

                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 0
                            end = make_location_from_ind fake_file 1

                    tok = Located sp Tokens.OParen
                    (_, res) = run_parse_fun peek [tok]
                in if res == tok
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "peek_span"
            [ ItCan "return the next token's span" $
                let fake_file = make_file "fake_file" "a"

                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 0
                            end = make_location_from_ind fake_file 1

                    (_, res) = run_parse_fun peek_span [Located sp undefined]
                in if res == sp
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "peek_matches"
            [ ItCan "check if the next token matches a predicate" $
                let (_, pred_result) = run_parse_fun (peek_matches (is_tt Tokens.OParen)) [Located undefined Tokens.OParen]
                in if pred_result
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "peek_match"
            [ ItCan "check which predicate is matched by the next token, and then run the appropriate action" $
                let predicates =
                        [ (is_tt Tokens.OParen, return 0)
                        , (is_tt Tokens.CParen, return 1)
                        ]
                    fallback = return 2

                    (_, res) = run_parse_fun (peek_match predicates fallback) [Located undefined Tokens.OParen]
                in if res == (0 :: Int)
                    then pass_test
                    else fail_test


            , ItCan "use the fallback action if none of the predicates match" $
                let (_, res) = run_parse_fun (peek_match [] (return 1)) [Located undefined undefined]
                in if res == (1 :: Int)
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "parser_matches"
            [ ItCan "check that a parser matches a predicate" $
                let (_, res) = run_parse_fun (parser_matches is_at_end) [Located undefined Tokens.EOF]
                in if res
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "advance"
            [ ItCan "advance the parser to the next token" $
                let (_, res) = run_parse_fun (advance >> parser_matches is_at_end) [Located undefined undefined, Located undefined Tokens.EOF]
                in if res
                    then pass_test
                    else fail_test
            -- TODO: it can return an error when advancing past the EOF token
            ]

        , DescribeFunction "advance_match"
            [ ItCan "run the action that is associated with the predicate that matched" $
                let predicates =
                        [ (is_tt Tokens.OParen, undefined)
                        , (is_tt Tokens.CParen, \ (Located _ Tokens.CParen) -> return 1)
                        ]
                    fallback = undefined

                    (_, res) = run_parse_fun (advance_match predicates fallback) [Located undefined Tokens.CParen, Located undefined Tokens.EOF]

                in if res == (1 :: Int)
                    then pass_test
                    else fail_test

            , ItCan "advance the parser" $
                let predicates =
                        [ (is_tt Tokens.OParen, \ _ -> return ())
                        ]
                    fallback = undefined

                    (_, res) = run_parse_fun (advance_match predicates fallback >> parser_matches is_at_end) [Located undefined Tokens.OParen, Located undefined Tokens.EOF]

                in if res
                    then pass_test
                    else fail_test

            , ItCan "keep the parser as is when no predicdate matches, and run the fallback action" $
                let action = advance_match [] (return 2) >>= \ r -> parser_matches is_at_end >>= \ b -> return (r, b)
                    (_, (match_res, end_res)) = run_parse_fun action [Located undefined Tokens.OParen, Located undefined Tokens.EOF]
                in if match_res == (2 :: Int) && not end_res
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "consume"
            [ ItCan "consume the next token if it matches the predicate" $
                let fake_file = make_file "fake_file" "a"
                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 0
                            end = make_location_from_ind fake_file 1

                    action =
                        consume (is_tt Tokens.OParen) >>= \ t ->
                        parser_matches is_at_end >>= \ a ->
                        return (t, a)

                    (_, (tok, at_end)) = run_parse_fun action [Located sp Tokens.OParen, Located undefined Tokens.EOF]
                in if tok == Just (Located sp Tokens.OParen) && at_end
                    then pass_test
                    else fail_test

            , ItCan "do nothing if the next token does not match the predicate" $
                let fake_file = make_file "fake_file" "a"
                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 0
                            end = make_location_from_ind fake_file 1

                    action =
                        consume (is_tt Tokens.OParen) >>= \ t ->
                        peek >>= \ nt ->
                        return (t, nt)

                    (_, (tok, next_token)) = run_parse_fun action [Located sp Tokens.CParen]

                in if tok == Nothing && next_token == Located sp Tokens.CParen
                    then pass_test
                    else fail_test
            ]

        , DescribeFunction "consume_or_error"
            [ ItCan "consume the next token if it matches the predicate" $
                let fake_file = make_file "fake_file" "a"
                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 0
                            end = make_location_from_ind fake_file 1

                    action =
                        consume_or_error TestErrorWithSpan (is_tt Tokens.OParen) >>= \ t ->
                        parser_matches is_at_end >>= \ a ->
                        return (t, a)

                    (_, (tok, at_end)) = run_parse_fun action [Located sp Tokens.OParen, Located undefined Tokens.EOF]
                in case tok of
                    JustWithError (Located sp' Tokens.OParen)
                        | sp == sp' && at_end -> pass_test
                    _ -> fail_test

            , ItCan "do add an error if the next token does not match the predicate" $
                let fake_file = make_file "fake_file" "a"
                    sp = Span start before end
                        where
                            start = make_location_from_ind fake_file 0
                            before = make_location_from_ind fake_file 0
                            end = make_location_from_ind fake_file 1

                    action =
                        consume_or_error TestErrorWithSpan (is_tt Tokens.OParen) >>= \ t ->
                        peek >>= \ nt ->
                        return (t, nt)

                    (errs, (tok, next_token)) = run_parse_fun action [Located sp Tokens.CParen]

                in case (errs, tok) of
                    ([TestErrorWithSpan _], NothingWithError _)
                        | next_token == Located sp Tokens.CParen -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "synchronize"
            [ ItCan "skip tokens until the parser matches a predicate" $
                let action =
                        synchronize (peek_matches_pred $ is_tt Tokens.CParen) >>
                        peek

                    toks =
                        [ Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.CParen
                        ]

                    (_, next_token) = run_parse_fun action toks
                in case next_token of
                    Located _ Tokens.CParen -> pass_test
                    _ -> fail_test

            , ItCan "stop at an EOF token" $
                let action =
                        synchronize (const False) >>
                        parser_matches is_at_end

                    toks =
                        [ Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.OParen
                        , Located undefined Tokens.CParen
                        , Located undefined Tokens.EOF
                        ]

                    (_, at_end) = run_parse_fun action toks
                in if at_end
                    then pass_test
                    else fail_test
            ]

        -- TODO: reduce duplicate copy-pasted code in tests for braced, indented, and blocked
        , DescribeFunction "braced"
            [ ItCan "parse a thing enclosed in '{' and '}'" $
                let action = braced (consume_or_error TestErrorWithSpan (is_tt $ Tokens.IntLit Tokens.Dec 0))
                    tokens = [Located undefined Tokens.OBrace, Located undefined (Tokens.IntLit Tokens.Dec 2), Located undefined Tokens.CBrace, Located undefined Tokens.EOF]

                    res = run_parse_fun action tokens
                in case res of
                    ([], JustWithError (_, Located _ (Tokens.IntLit Tokens.Dec 2))) -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "indented"
            [ ItCan "parse a thing enclosed in indent and dedent tokens" $
                let action = indented (consume_or_error TestErrorWithSpan (is_tt $ Tokens.IntLit Tokens.Dec 0))
                    tokens = [Located undefined Tokens.Indent, Located undefined (Tokens.IntLit Tokens.Dec 2), Located undefined Tokens.Dedent, Located undefined Tokens.EOF]

                    res = run_parse_fun action tokens
                in case res of
                    ([], JustWithError (_, Located _ (Tokens.IntLit Tokens.Dec 2))) -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "blocked"
            [ ItCan "parse a thing enclosed in '{' and '}'" $
                let action = blocked (consume_or_error TestErrorWithSpan (is_tt $ Tokens.IntLit Tokens.Dec 0))
                    tokens = [Located undefined Tokens.OBrace, Located undefined (Tokens.IntLit Tokens.Dec 2), Located undefined Tokens.CBrace, Located undefined Tokens.EOF]

                    res = run_parse_fun action tokens
                in case res of
                    ([], JustWithError (_, Located _ (Tokens.IntLit Tokens.Dec 2))) -> pass_test
                    _ -> fail_test

            , ItCan "parse a thing enclosed in indent and dedent tokens" $
                let action = blocked (consume_or_error TestErrorWithSpan (is_tt $ Tokens.IntLit Tokens.Dec 0))
                    tokens = [Located undefined Tokens.Indent, Located undefined (Tokens.IntLit Tokens.Dec 2), Located undefined Tokens.Dedent, Located undefined Tokens.EOF]

                    res = run_parse_fun action tokens
                in case res of
                    ([], JustWithError (_, Located _ (Tokens.IntLit Tokens.Dec 2))) -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "thing_list_no_separator"
            [ ItCan "repeatedly parse a thing until the stop predicate holds" $
                let action = thing_list_no_separator
                        (peek_matches_pred $ is_tt Tokens.Semicolon) -- stop predicate
                        undefined -- synchronization predicate
                        (consume_or_error TestErrorWithSpan $ is_tt Tokens.Colon) -- thing
                    tokens = [Located undefined Tokens.Colon, Located undefined Tokens.Colon, Located undefined Tokens.Semicolon]

                    res = run_parse_fun action tokens
                in case res of
                    ([], [Located _ Tokens.Colon, Located _ Tokens.Colon]) -> pass_test
                    _ -> fail_test

            , ItCan "stop at an eof token" $
                let action =
                        thing_list_no_separator
                            (const False) -- stop predicate
                            (peek_matches_pred $ is_tt Tokens.Colon) -- synchronization predicate
                            (consume_or_error TestErrorWithSpan $ is_tt Tokens.Colon) -- thing
                        >>
                        peek
                    tokens = [Located undefined Tokens.Colon, Located undefined Tokens.Colon, Located undefined Tokens.EOF]

                    (_, res) = run_parse_fun action tokens
                in case res of
                    Located _ Tokens.EOF -> pass_test
                    _ -> fail_test

            , ItCan "synchronize when it encounters a malformed thing" $
                let action = thing_list_no_separator
                        (peek_matches_pred $ is_tt Tokens.Semicolon) -- stop predicate
                        (peek_matches_pred $ is_tt Tokens.Colon) -- synchronization predicate
                        (consume_or_error TestErrorWithSpan $ is_tt Tokens.Colon) -- thing

                    tokens = [Located undefined Tokens.Colon, Located undefined Tokens.OParen, Located undefined Tokens.Colon, Located undefined Tokens.Semicolon]

                    res = run_parse_fun action tokens
                in case res of
                    ([_], [Located _ Tokens.Colon, Located _ Tokens.Colon]) -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "thing_list_with_separator"
            [ ItCan "repeatedly parse a thing while it finds a delimiter token" $
                let action = thing_list_with_separator
                        (is_tt Tokens.Comma) -- delimiter predicate
                        (consume_or_error TestErrorWithSpan $ is_tt Tokens.Colon) -- thing
                    tokens = [Located undefined Tokens.Colon, Located undefined Tokens.Comma, Located undefined Tokens.Colon]

                    res = run_parse_fun action tokens
                in case res of
                    ([_], [Located _ Tokens.Colon, Located _ Tokens.Colon]) -> pass_test
                    _ -> fail_test
            ]

        , DescribeFunction "line_ending" []

        , DescribeFunction "optional_line_ending" []

        , DescribeFunction "parse_type" []

        , DescribeFunction "parse_type_annotation" []

        , DescribeFunction "parse_param" []

        , DescribeFunction "parse_block" []

        , DescribeFunction "parse_decl" []

        , DescribeFunction "parse_fun" []

        , DescribeFunction "parse_module" []

        , DescribeFunction "parse_from_toks" []
        ]
