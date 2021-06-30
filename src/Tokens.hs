{-# LANGUAGE DeriveDataTypeable #-}

module Tokens
    ( Token(..)
    , Tokens.lex
    , format_token
    ) where

import File
import Location

import Data.Char(isDigit, isAlpha, isHexDigit, isOctDigit, digitToInt, isSpace)
import Data.List(foldl', elemIndex, elemIndices)
import Data.Maybe(catMaybes, fromMaybe)

import qualified Message
import qualified Message.Underlines as MsgUnds

import Data.Data(Data)

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving (Data, Eq)

data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | Comma
    | Period
    | Question
    | Colon
    | Bang
    | Plus
    | Minus
    | Star
    | Slash
    | Percent
    | Equal
    | Greater
    | Less
    | Tilde
    | Amper
    | Pipe
    | Caret
    | Dollar
    | Hash
    | RightArrow
    | LeftArrow
    | DoublePlus
    | DoubleMinus
    | DoubleGreater
    | DoubleLess
    | DoubleAmper
    | DoublePipe
    | DoubleEqual
    | DoubleColon
    | PlusEqual
    | MinusEqual
    | StarEqual
    | SlashEqual
    | BangEqual
    | GreaterEqual
    | LessEqual
    | PercentEqual
    | DoubleLessEqual
    | DoubleGreaterEqual
    | AmperEqual
    | PipeEqual
    | CaretEqual
    | Identifier String
    | CharLit Char
    | StringLit String
    | IntLit IntLitBase Integer
    | FloatLit Double
    | BoolLit Bool
    | This
    | Fun
    | Root
    | Let
    | Mut
    | Data
    | Impl
    | Return
    | While
    | For
    | If
    | Else
    | Case
    | Break
    | Continue
    | Boom
    | OBrace
    | CBrace
    | Semicolon
    | Indent
    | Dedent
    | Newline
    | EOF
    deriving (Data, Eq)

format_token :: Token -> String
format_token OParen = "'('"
format_token CParen = "')'"
format_token OBrack = "'{'"
format_token CBrack = "'}'"
format_token Comma = "','"
format_token Period = "'.'"
format_token Question = "'?'"
format_token Colon = "':'"
format_token Bang = "'!'"
format_token Plus = "'+'"
format_token Minus = "'-'"
format_token Star = "'*'"
format_token Slash = "'/'"
format_token Percent = "'%'"
format_token Equal = "'='"
format_token Greater = "'>'"
format_token Less = "'<'"
format_token Tilde = "'~'"
format_token Amper = "'&'"
format_token Pipe = "'|'"
format_token Caret = "'^'"
format_token Dollar = "'$'"
format_token Hash = "'#'"
format_token RightArrow = "'->'"
format_token LeftArrow = "'<-'"
format_token DoublePlus = "'++'"
format_token DoubleMinus = "'--'"
format_token DoubleGreater = "'>>'"
format_token DoubleLess = "'<<'"
format_token DoubleAmper = "'&&'"
format_token DoublePipe = "'||'"
format_token DoubleEqual = "'=='"
format_token DoubleColon = "'::'"
format_token PlusEqual = "'+='"
format_token MinusEqual = "'-='"
format_token StarEqual = "'*='"
format_token SlashEqual = "'/='"
format_token BangEqual = "'!='"
format_token GreaterEqual = "'>='"
format_token LessEqual = "'<='"
format_token PercentEqual = "'%="
format_token DoubleLessEqual = "'<<='"
format_token DoubleGreaterEqual = "'>>='"
format_token AmperEqual = "'&='"
format_token PipeEqual = "'|='"
format_token CaretEqual = "'^='"
format_token (Identifier n) = "identifier '" ++ n ++ "'"
format_token (CharLit ch) = "character literal '" ++ [ch] ++ "'"
format_token (StringLit s) = "string literal " ++ show shortened
    where
        shortened = if length s < 20
            then s
            else take 16 s ++ "..."
format_token (IntLit _ i) = "integer literal " ++ show i
format_token (FloatLit d) = "floating point literal " ++ show d
format_token (BoolLit b) = "bool literal " ++ if b then "true" else "false"
format_token This = "keyword 'this'"
format_token Fun = "keyword 'fun'"
format_token Root = "keyword 'root'"
format_token Let = "keyword 'let'"
format_token Mut = "keyword 'mut'"
format_token Data = "keyword 'data'"
format_token Impl = "keyword 'impl'"
format_token Return = "keyword 'return'"
format_token While = "keyword 'while'"
format_token For = "keyword 'for'"
format_token If = "keyword 'if'"
format_token Else = "keyword 'else'"
format_token Case = "keyword 'case'"
format_token Break = "keyword 'break'"
format_token Continue = "keyword 'continue'"
format_token Boom = "keyword 'boom'"
format_token OBrace = "'{'"
format_token CBrace = "'}'"
format_token Semicolon = "';'"
format_token Indent = "indent"
format_token Dedent = "dedent"
format_token Newline = "newline"
format_token EOF = "end of file"

data IndentFrame
    = IndentationSensitive Int
    | IndentationInsensitive

data Lexer
    = Lexer
     { sourcefile :: File
     , source_location :: Int
     , remaining :: String
     , rev_str_before_lexer :: String
     , linen :: Int
     , coln :: Int
     , indent_stack :: [IndentFrame]
     }

data LexError
    = BadChar Char Span
    | UntermMultilineComment Span
    | UntermStr Span
    | UntermChar Span
    | MulticharChar Span
    | InvalidBase Char Span Span
    | InvalidDigit Char Span Span
    | NonDecimalFloat Span
    | MissingDigits Span
    -- TODO: add 4 fields: new indent level, before indent level, two closest indentation levels
    | BadDedent Span

instance Message.ToDiagnostic LexError where
    to_diagnostic err =
        case err of
            BadChar ch sp -> simple sp "E0001" "bad-char" $ "bad character '" ++ [ch] ++ "'"
            UntermMultilineComment sp -> simple sp "E0002" "unterm-multiln-cmt" "unterminated multiline comment"
            UntermStr sp -> simple sp "E0003" "unterm-strlit" "string literal missing closing quote ('\"')"
            UntermChar sp -> simple sp "E0004" "unterm-chrlit" "character literal missing closing quote (''')"
            MulticharChar sp -> simple sp "E0005" "multichr-chrlit" "character literal must contain exactly one character"

            InvalidBase basechr basechrsp _ ->
                Message.SimpleDiag Message.Error (Just basechrsp) (Message.make_code "E0006") (Just "invalid-intlit-base")
                    [ Message.Underlines
                        [ MsgUnds.Underline basechrsp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error $ "invalid integer literal base '" ++ [basechr] ++ "' (must be 'x', 'o', or 'b' or omitted)"]
                        ]
                    ]

            InvalidDigit digitchr digitsp litsp ->
                Message.SimpleDiag Message.Error (Just digitsp) (Message.make_code "E0007") (Just "invalid-digit")
                    [ Message.Underlines
                        [ MsgUnds.Underline digitsp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error $ "invalid digit '" ++ [digitchr] ++ "'"]
                        , MsgUnds.Underline litsp MsgUnds.Secondary [MsgUnds.Message MsgUnds.Note "in this integer literal"]
                        ]
                    ]

            NonDecimalFloat sp -> simple sp "E0008" "nondecimal-floatlit" "non-decimal floating point literals are not supported"
            MissingDigits sp -> simple sp "E0009" "no-digits" "integer literal must have digits"

            BadDedent sp -> simple sp "E0010" "bad-dedent" "dedent to level that does not match any other indentation level"

        where
            simple sp code nm msg = Message.SimpleDiag Message.Error (Just sp) (Message.make_code code) (Just nm)
                    [Message.Underlines [MsgUnds.Underline sp MsgUnds.Primary [MsgUnds.Message MsgUnds.Error msg]]]

lex :: File -> ([LexError], [Located Token])
lex f =
    let start_lexer =
            ( Just $ Lexer
                     { sourcefile = f
                     , source_location = 0
                     , remaining = source f
                     , rev_str_before_lexer = ""
                     , linen = 1
                     , coln = 1
                     , indent_stack = [IndentationSensitive 0]
                     }
            , []
            , []
            )

        should_end (Nothing, _, _) = True
        should_end (Just _, _, _) = False

        feed (Nothing, _, _) = error "unreachable"
        feed (Just l, errs, toks) =
            let safe_last [] = Nothing
                safe_last x = Just $ last x

                (l', next_errs, next_toks) = lex' l (safe_last toks)

            in (l', errs ++ next_errs, toks ++ next_toks)

        (_, final_errs, final_toks) = until should_end feed start_lexer
    in (final_errs, final_toks)

lex' :: Lexer -> Maybe (Located Token) -> (Maybe Lexer, [LexError], [Located Token])
lex' lexer last_tok =
    let lex_choices =
            [ lex_eof
            , lex_comment
            , lex_punc_token
            , lex_braces_semi
            , lex_str_or_char_lit
            , lex_iden
            , lex_number
            , lex_space
            , make_bad_char
            ]

        (include_indent, lexer', errs, toks) = head $ catMaybes $ map ($ lexer) lex_choices
        (next_indent_stack, indent_errs, indent_toks) = lex_indent lexer last_tok

    in if include_indent
        then ( case lexer' of
                   Just l -> Just $ l { indent_stack = next_indent_stack }
                   Nothing -> Nothing
             , indent_errs ++ errs
             , indent_toks ++ toks
             )
        else (lexer', errs, toks)

lex_eof, lex_comment, lex_punc_token, lex_braces_semi, lex_str_or_char_lit, lex_iden, lex_number, lex_space, make_bad_char :: Lexer -> Maybe (Bool, Maybe Lexer, [LexError], [Located Token])

lex_eof lexer =
    case remaining lexer of
        [] -> Just (False, Nothing, [], all_dedents ++ [Located (make_span_from_lexer lexer 0 1) EOF])
            where
                -- TODO: use the same span as other dedent tokens
                all_dedents = case concatMap make_dedent (init $ indent_stack lexer) of
                    [] -> []
                    dedents -> Located (make_span_from_lexer lexer 0 1) Newline : dedents

                make_dedent (IndentationSensitive _) = [Located (make_span_from_lexer lexer 0 1) Dedent]
                make_dedent IndentationInsensitive = [] -- the parser will handle these when it finds a dedent token instead of a matching '}'

        _ -> Nothing

lex_comment lexer =
    case remaining lexer of
        '/':'/':next ->
            let comment = takeWhile (/='\n') next
                advance_amt = length comment + 3 -- +2 for '//' and +1 for the newline
            in Just (False, Just $ seek_lexer lexer advance_amt, [], [])

        '/':'*':next ->
            case until_comment_end next of
                Just comment_length -> Just (False, Just $ seek_lexer lexer (comment_length + 4), [], [])
                Nothing -> single_err lexer (length next + 2) UntermMultilineComment False
            where
                until_comment_end ('*':'/':_) = Just 0
                until_comment_end ('/':'*':more) =
                    until_comment_end more >>= \ subcomment_len ->
                    until_comment_end (drop (subcomment_len + 2) more) >>= \ more_after ->
                    Just (2 + subcomment_len + 2 + more_after)
                until_comment_end (_:more) = (1 +) <$> until_comment_end more
                until_comment_end [] = Nothing

        _ -> Nothing

lex_punc_token lexer =
    let res len tok = single_tok lexer len tok True
    in case remaining lexer of
        '<':'<':'=':_ -> res 3 DoubleLessEqual
        '>':'>':'=':_ -> res 3 DoubleGreaterEqual

        '+':'=':_ -> res 2 PlusEqual
        '-':'=':_ -> res 2 MinusEqual
        '*':'=':_ -> res 2 StarEqual
        '/':'=':_ -> res 2 SlashEqual
        '%':'=':_ -> res 2 PercentEqual
        '<':'=':_ -> res 2 LessEqual
        '>':'=':_ -> res 2 GreaterEqual
        '!':'=':_ -> res 2 BangEqual
        '&':'=':_ -> res 2 AmperEqual
        '|':'=':_ -> res 2 PipeEqual
        '^':'=':_ -> res 2 CaretEqual

        '=':'=':_ -> res 2 DoubleEqual
        '+':'+':_ -> res 2 DoublePlus
        '-':'-':_ -> res 2 DoubleMinus
        '&':'&':_ -> res 2 DoubleAmper
        '|':'|':_ -> res 2 DoublePipe
        '<':'<':_ -> res 2 DoubleLess
        '>':'>':_ -> res 2 DoubleGreater
        ':':':':_ -> res 2 DoubleColon

        '-':'>':_ -> res 2 RightArrow
        '<':'-':_ -> res 2 LeftArrow

        '(':_ -> res 1 OParen
        ')':_ -> res 1 CParen
        '[':_ -> res 1 OBrack
        ']':_ -> res 1 CBrack
        ',':_ -> res 1 Comma
        '.':_ -> res 1 Period
        '?':_ -> res 1 Question
        '~':_ -> res 1 Tilde
        '#':_ -> res 1 Hash
        '$':_ -> res 1 Dollar
        '!':_ -> res 1 Bang
        '=':_ -> res 1 Equal
        ':':_ -> res 1 Colon
        '+':_ -> res 1 Plus
        '-':_ -> res 1 Minus
        '*':_ -> res 1 Star
        '/':_ -> res 1 Slash
        '%':_ -> res 1 Percent
        '<':_ -> res 1 Less
        '>':_ -> res 1 Greater
        '^':_ -> res 1 Caret
        '&':_ -> res 1 Amper
        '|':_ -> res 1 Pipe

        _ -> Nothing

lex_braces_semi lexer =
    let res = Just (True, Just $ seek_lexer lexer 1, [], [])
    in case remaining lexer of
        '{':_ -> res
        '}':_ -> res
        ';':_ -> res
        _ -> Nothing

lex_str_or_char_lit lexer =
    case remaining lexer of
        '\'':more ->
            case f '\'' more of
                Right (seek_amt_minus_1, contents)
                    | length contents == 1 -> single_tok lexer (seek_amt_minus_1 + 1) (CharLit $ head contents) True
                    | otherwise -> single_err lexer (seek_amt_minus_1 + 1) MulticharChar True

                Left err_len_minus_1 -> single_err lexer (err_len_minus_1 + 1) UntermChar True

        '"':more ->
            case f '"' more of
                Right (seek_amt_minus_1, contents) -> single_tok lexer (seek_amt_minus_1 + 1) (StringLit contents) True

                Left err_len_minus_1 -> single_err lexer (err_len_minus_1 + 1) UntermStr True

        _ -> Nothing
    where
        f start_delim more =
            let nl_ind = fromMaybe (length more) $ elemIndex '\n' more
                end_delim_ind = fromMaybe (length more) $ elemIndex start_delim more

                (cur_contents, after_lit) = splitAt (min nl_ind end_delim_ind) more

            in if end_delim_ind < nl_ind
                then Right (length cur_contents + 1, cur_contents)
                else
                    let (whitespace, after_whitespace) = span isSpace after_lit
                    in case after_whitespace of
                        x:after_next_start_delim | x == start_delim ->
                            let total_len = ((length cur_contents + length whitespace + 1) +)
                            in case f start_delim after_next_start_delim of
                                Right (next_tok_len, next_contents) -> Right (total_len next_tok_len, cur_contents ++ "\n" ++ next_contents)
                                Left err_len -> Left $ total_len err_len

                        _ -> Left $ length cur_contents

lex_iden lexer =
    case remaining lexer of
        entire@(x:_) | isAlpha x || x == '_' ->
            let is_iden_char ch = isAlpha ch || isDigit ch || ch == '_' || ch == '\''
                contents = takeWhile is_iden_char entire
                iden_len = length contents
            in single_tok lexer iden_len (
                case contents of
                    "data" -> Data
                    "impl" -> Impl
                    "fun" -> Fun
                    "root" -> Root
                    "mut" -> Mut
                    "let" -> Let
                    "this" -> This
                    "return" -> Return
                    "while" -> While
                    "for" -> For
                    "if" -> If
                    "else" -> Else
                    "case" -> Case
                    "break" -> Break
                    "continue" -> Continue
                    "true" -> BoolLit True
                    "false" -> BoolLit False
                    "boom" -> Boom
                    _ -> Identifier contents
            ) True

        _ -> Nothing

-- TOOD: clean this up
lex_number lexer =
    case remaining lexer of
        entire@(x:_) | isDigit x ->
        {- {{{ how the algorithm works
            {{{ first scan
            a number literal has three components:

                 0x   234abc   .342353
                |__| |______| |_______|
                 |    |        |
                 |    digits   floating point digits (optional)
                 base (optional)


            to lex digits, this lexer first consumes all possible characters, then goes back to check that each component is correct

            the first scan happens like:
                a base is a '0' followed by an alphabetical character
                    - the base being correct (i.e. one of 'o', 'b', or 'x') is checked in the second pass
                    - if there is no 0, then assume it is decimal
                    - if there is a 0 followed by a non-alphabetical character, then there is no base
                the digits are one or more hexadecimal digits
                    - the digits being correct for the base are checked in the second pass
                    - if there are no digits, that's an error, but that is checked in the second pass
                the floating point digits are a dot ('.') followed by one or more decimal digits
                    - hexadecimal digits are accepted in the first pass, the second pass checks that they are all decimal
                    - there can only be digits if the literal is decimal, but that is also checked in the second pass
                    - if there are no floating point digits, this is not a floating point literal, do not consume the '.'

            the first scan can be summed up with the following regex

                (0\w)?([0-9a-f_a-F]+)(\.[0-9a-f_a-F]+)?
            }}}
            {{{ classification
            the integer literal is then classified into the following categories
                - valid hex
                    * the base is '0x'
                    * all the digits are 0-9 or a-f or A-F
                    * there are no decimal places
                - valid binary
                    * the base is '0b'
                    * all the digits are 0 or 1
                    * there are no decimal places
                - valid octal
                    * the base is '0o'
                    * all the digits are 0-7
                    * there are no decimal places
                - valid decimal
                    * there is no base
                    * all the digits are 0-9
                    * there are no decimal places
                - floating point literal
                    * there is no base (i.e. the base is decimal)
                    * all the digits are 0-9
                    * there is at least one decimal place

                - invalid base (the base character is not 'o', 'b', or 'x')
                - invalid digit for base (the digits have a character that is not valid for the given base)
                - non-decimal float literal
                - missing digits (there are no integral digits)
            }}}
        }}} -}
            let (base, base_len, after_base) =
                    case entire of
                        '0':ch:after | isAlpha ch -> (Just ch, 2, after)
                        after -> (Nothing, 0, after)

                (digits, digits_len, after_digits) = (d, length d, after)
                    where
                        (d, after) = span isHexDigit after_base

                (decimal_digits, decimal_len, _) =
                    case after_digits of
                        '.':rest@(first_digit:_)
                            | isHexDigit first_digit ->
                                (Just f, length f + 1, drop 1 more)
                                where (f, more) = span isHexDigit rest

                        after -> (Nothing, 0, after)

                total_len = base_len + digits_len + decimal_len

                read_digits :: (Num a) => [a] -> a -> String -> (a -> a -> a) -> a
                read_digits places read_base str power_fn = foldl' acc_fn 0 $ zip places str
                    where
                        acc_fn acc (place, char) = acc + ((read_base `power_fn` place) * fromIntegral (digitToInt char))

                read_lit_digits read_base power_fn = read_digits (reverse [0..toInteger digits_len - 1]) read_base digits power_fn

                get_invalid_digits chk = filter (not . chk . fst) $ zip digits [base_len..]
                make_errors_from_invalid_digits = map (\ (dig, ind) -> InvalidDigit dig (make_span_from_lexer lexer ind 1) (make_span_from_lexer lexer 0 total_len))

                check_int_no_float digit_chk tok =
                    if not $ null digits
                    then
                        let invalid_digits = get_invalid_digits digit_chk
                        in if null invalid_digits
                            then
                                case decimal_digits of
                                    Nothing -> single_tok lexer total_len tok True
                                    Just _ -> single_err lexer total_len NonDecimalFloat True
                            else Just (True, Just $ seek_lexer lexer total_len, make_errors_from_invalid_digits invalid_digits, [])
                    else single_err lexer total_len MissingDigits True

            in case base of
                Just 'x' ->
                    check_int_no_float (const True) $ IntLit Hex $ read_lit_digits 16 (^)
                Just 'b' ->
                    check_int_no_float (\ d -> d == '0' || d == '1') $ IntLit Bin $ read_lit_digits 2 (^)
                Just 'o' ->
                    check_int_no_float isOctDigit $ IntLit Oct $ read_lit_digits 8 (^)
                Nothing ->
                    if not $ null digits
                    then
                        let invalid_digits = get_invalid_digits isDigit
                        in if null invalid_digits
                            then
                                case decimal_digits of
                                    Nothing -> single_tok lexer total_len (IntLit Dec $ read_lit_digits 10 (^)) True
                                    Just dd -> single_tok lexer total_len (FloatLit $ fromIntegral (read_lit_digits 10 (^)) + read_digits (map (fromIntegral . negate) [1..decimal_len]) 10 dd (**)) True
                            else Just (True, Just $ seek_lexer lexer total_len, make_errors_from_invalid_digits invalid_digits, [])
                    else single_err lexer total_len MissingDigits True

                Just b -> single_err lexer total_len (InvalidBase b $ make_span_from_lexer lexer 1 1) True

        _ -> Nothing

lex_space lexer =
    case remaining lexer of
        x:_ | isSpace x -> Just (False, Just $ seek_lexer lexer 1, [], [])
        _ -> Nothing

make_bad_char lexer =
    case remaining lexer of
        x:_ -> single_err lexer 1 (BadChar x) True
        [] -> Nothing

-- TODO: clean this up
lex_indent :: Lexer -> Maybe (Located Token) -> ([IndentFrame], [LexError], [Located Token])
lex_indent lexer last_tok =
    case last_tok of
        Just _ ->
            let remain = remaining lexer
            in
                process_cbrace remain .
                process_semi   remain .
                process_obrace remain .
                process_dedent cur_indent last_indent .
                process_nl     cur_indent last_indent .
                process_indent cur_indent last_indent $ (orig_indent_stack, [], [])
        Nothing -> (orig_indent_stack, [], [])
    where
        orig_indent_stack = indent_stack lexer

        cur_indent = foldl' count_indent (Just 0) str_before_lexer'
            where
                str_before_lexer' = takeWhile (/='\n') $ rev_str_before_lexer lexer

                count_indent (Just acc) ' ' = Just $ acc + 1
                count_indent (Just acc) '\t' = Just $ (acc `div` 8 + 1) * 8
                count_indent _ _ = Nothing

        last_indent = case head orig_indent_stack of
            IndentationInsensitive -> Nothing
            IndentationSensitive x -> Just x

        last_is_semi =
            case last_tok of
                Just (Located _ Semicolon) -> True
                _ -> False

        -- TODO: support \ at the end of a line to prevent indent tokens
        -- TODO: support ~ at the end of a line to start a indentation sensitive frame
        process_indent (Just curlvl) (Just lastlvl) (stack, errs, toks)
            | curlvl > lastlvl =
                (new_frame:stack, errs, toks ++ [make_tok_at_cur Indent])
                where
                    new_frame = IndentationSensitive curlvl
        process_indent _ _ st = st

        process_obrace ('{':_) (stack, errs, toks) = (IndentationInsensitive : stack, errs, toks ++ [make_tok_at_cur OBrace])
        process_obrace _ st = st

        process_nl (Just curlvl) (Just lastlvl) (stack, errs, toks)
            -- TODO: do not emit newline if first character is '{'
            | curlvl == lastlvl =
                if last_is_semi
                    then (stack, errs, toks)
                    else (stack, errs, toks ++ [make_tok_at_nl_before Newline])

        process_nl _ _ st = st

        process_semi (';':_) (stack, errs, toks) = (stack, errs, toks ++ [make_tok_at_cur Semicolon])
        process_semi _ st = st

        process_dedent (Just curlvl) (Just lastlvl) (stack, errs, toks)
            | curlvl < lastlvl =
                let canpop (IndentationSensitive ind)
                        | curlvl < ind = True
                        | otherwise = False
                    canpop IndentationInsensitive = False

                    (popped, after_pop) = span canpop stack

                    is_valid_level = case head after_pop of
                        IndentationSensitive lvl
                            | curlvl > lvl -> False

                        _ -> True

                    num_pop = length popped
                in
                    ( after_pop
                    , errs ++
                      (if is_valid_level
                          then []
                          else [BadDedent (make_span_from_lexer lexer 0 1)])
                    , toks ++
                      (if last_is_semi
                          then []
                          else [make_tok_at_nl_before Newline]) ++
                      replicate num_pop (make_tok_at_cur Dedent) -- TODO: change dedent span to something better so that it doesnt include beginning of the next token
                    )
        process_dedent _ _ st = st

        process_cbrace ('}':_) (stack, errs, toks) =
            case head stack of
                IndentationInsensitive -> (tail stack, errs, newtoks)
                IndentationSensitive _ -> (stack, errs, newtoks) -- do not pop on the stack, but the parser will handle the error message when there is a random '}' that appears
            where
                newtoks = toks ++ [make_tok_at_cur CBrace]
        process_cbrace _ st = st

        make_tok_at_cur tok = Located (make_span_from_lexer lexer 0 1) tok

        make_tok_relative_to_nl_before off len tok = Located (make_span_from_lexer lexer (off_to_nl + off) len) tok
            where
                off_to_nl =
                    case from_last_tok of
                        Just x -> x :: Int
                        Nothing ->
                            case from_cur_pos of
                                Just x -> x :: Int
                                Nothing -> error "no newlines to make token at"
                    where
                        from_last_tok =
                            last_tok >>= \ (Located (Span _ _ endloc) _) ->
                            let endind = ind_of_loc endloc
                            in elemIndex '\n' (drop endind $ source $ sourcefile lexer) >>= \ from_tok_ind ->
                            Just $ from_tok_ind + endind - source_location lexer

                        from_cur_pos =
                            elemIndex '\n' (rev_str_before_lexer lexer) >>= \ x ->
                            Just $ -x - 1

        make_tok_at_nl_before = make_tok_relative_to_nl_before 0 1

make_span_from_lexer :: Lexer -> Int -> Int -> Span
make_span_from_lexer lexer start len = Span (make_loc_from_lexer start_lexer) (make_loc_from_lexer before_lexer) (make_loc_from_lexer end_lexer)
    where
        start_lexer = lexer `seek_lexer` start
        before_lexer = start_lexer `seek_lexer` (len - 1)
        end_lexer = before_lexer `seek_lexer` 1

make_loc_from_lexer :: Lexer -> Location
make_loc_from_lexer l = make_location (sourcefile l) (source_location l) (linen l) (coln l)

single_tok :: Lexer -> Int -> Token -> Bool -> Maybe (Bool, Maybe Lexer, [LexError], [Located Token])
single_tok lexer len tok b = Just (b, Just $ seek_lexer lexer len, [], [Located (make_span_from_lexer lexer 0 len) tok])

single_err :: Lexer -> Int -> (Span -> LexError) -> Bool -> Maybe (Bool, Maybe Lexer, [LexError], [Located Token])
single_err lexer len err b = Just (b, Just $ seek_lexer lexer len, [err (make_span_from_lexer lexer 0 len)], [])

seek_lexer :: Lexer -> Int -> Lexer
seek_lexer l 0 = l
seek_lexer lexer@(Lexer sf loc remain before l c _) n =
    let (before', changed, remain')
            | n > 0 =
                let (into_before, r) = splitAt n remain
                in (reverse into_before ++ before, into_before, r)

            | otherwise =
                let (into_remain, b) = splitAt (-n) before
                in (b, into_remain, reverse into_remain ++ remain)

        numnl = length $ elemIndices '\n' changed

        new_linen
            | n > 0 = l + numnl
            | otherwise = l - numnl

        new_coln
            | numnl == 0 = c + n
            | otherwise = 1 + length (takeWhile (/='\n') before')

    in lexer
       { sourcefile = sf
       , source_location = loc + n
       , remaining = remain'
       , rev_str_before_lexer = before'
       , linen = new_linen
       , coln = new_coln
       }
