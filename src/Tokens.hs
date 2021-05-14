{-# LANGUAGE DeriveDataTypeable #-}

module Tokens
    ( Token(..)
    , Tokens.lex
    , format_token
    ) where

import File
import Location

import Data.Char(isDigit, isAlpha, isHexDigit, isOctDigit, digitToInt, isSpace)
import Data.List(foldl', find, elemIndex)
import Data.Either(isRight)

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

data Lexer = Lexer
             { sourcefile :: File
             , source_location :: Int
             , remaining :: String
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
                    [ Message.Underlines $ MsgUnds.UnderlinesSection
                        [ MsgUnds.Message basechrsp MsgUnds.Error MsgUnds.Primary $ "invalid integer literal base '" ++ [basechr] ++ "' (must be 'x', 'o', or 'b' or omitted)"
                        ]
                    ]

            InvalidDigit digitchr digitsp litsp ->
                Message.SimpleDiag Message.Error (Just digitsp) (Message.make_code "E0007") (Just "invalid-digit")
                    [ Message.Underlines $ MsgUnds.UnderlinesSection
                        [ MsgUnds.Message digitsp MsgUnds.Error MsgUnds.Primary $ "invalid digit '" ++ [digitchr] ++ "'"
                        , MsgUnds.Message litsp MsgUnds.Note MsgUnds.Secondary "in this integer literal"
                        ]
                    ]

            NonDecimalFloat sp -> simple sp "E0008" "nondecimal-floatlit" "non-decimal floating point literals are not supported"
            MissingDigits sp -> simple sp "E0009" "no-digits" "integer literal must have digits"

            BadDedent sp -> simple sp "E0010" "bad-dedent" "dedent to level that does not match any other indentation level"

        where
            simple sp code nm msg = Message.SimpleDiag Message.Error (Just sp) (Message.make_code code) (Just nm)
                    [ Message.Underlines $ MsgUnds.UnderlinesSection [MsgUnds.Message sp MsgUnds.Error MsgUnds.Primary msg]
                    ]

lex :: File -> [Either LexError (Located Token)]
lex f = lex' [] [] $ Lexer
           { sourcefile = f
           , source_location = 0
           , remaining = source f
           }

lex' :: [Either LexError (Located Token)] -> [IndentFrame] -> Lexer -> [Either LexError (Located Token)]
lex' prevtoks indent_stack lexer =
    case remaining lexer of
        -- comments {{{
        '/':'/':next ->
            let comment = takeWhile (/='\n') next
            in continue_lex_with_nothing $ length comment + 3

        '/':'*':next ->
            case comment_length next of
                Right cl -> continue_lex_with_nothing cl
                Left chars_to_end -> continue_lex_with [Left $ make_error 0 chars_to_end UntermMultilineComment] chars_to_end
            where
                comment_length after_slash_star =
                    case chars_until_comment_end 0 after_slash_star of
                        Right cl -> Right $ 4 + cl
                        Left chars_to_end -> Left $ 2 + chars_to_end

                chars_until_comment_end acc entire@('/':'*':rest) =
                    case comment_length rest of
                        Right inner_comment_length ->
                            chars_until_comment_end (acc + inner_comment_length) after_inner_comment
                            where
                                after_inner_comment = drop inner_comment_length entire
                        Left unterminated_inner_comment_length -> Left $ acc + unterminated_inner_comment_length

                chars_until_comment_end acc ('*':'/':_) = Right acc
                chars_until_comment_end acc [] = Left acc
                chars_until_comment_end acc (_:rest) = chars_until_comment_end (acc + 1) rest

                -- TODO: check for '* /' and put a note there
        -- }}}

        '<':'<':'=':_ -> continue_lex_with_single_tok 3 DoubleLessEqual
        '>':'>':'=':_ -> continue_lex_with_single_tok 3 DoubleGreaterEqual

        '+':'=':_ -> continue_lex_with_single_tok 2 PlusEqual
        '-':'=':_ -> continue_lex_with_single_tok 2 MinusEqual
        '*':'=':_ -> continue_lex_with_single_tok 2 StarEqual
        '/':'=':_ -> continue_lex_with_single_tok 2 SlashEqual
        '%':'=':_ -> continue_lex_with_single_tok 2 PercentEqual
        '<':'=':_ -> continue_lex_with_single_tok 2 LessEqual
        '>':'=':_ -> continue_lex_with_single_tok 2 GreaterEqual
        '!':'=':_ -> continue_lex_with_single_tok 2 BangEqual
        '&':'=':_ -> continue_lex_with_single_tok 2 AmperEqual
        '|':'=':_ -> continue_lex_with_single_tok 2 PipeEqual
        '^':'=':_ -> continue_lex_with_single_tok 2 CaretEqual

        '=':'=':_ -> continue_lex_with_single_tok 2 DoubleEqual
        '+':'+':_ -> continue_lex_with_single_tok 2 DoublePlus
        '-':'-':_ -> continue_lex_with_single_tok 2 DoubleMinus
        '&':'&':_ -> continue_lex_with_single_tok 2 DoubleAmper
        '|':'|':_ -> continue_lex_with_single_tok 2 DoublePipe
        '<':'<':_ -> continue_lex_with_single_tok 2 DoubleLess
        '>':'>':_ -> continue_lex_with_single_tok 2 DoubleGreater
        ':':':':_ -> continue_lex_with_single_tok 2 DoubleColon

        '-':'>':_ -> continue_lex_with_single_tok 2 RightArrow
        '<':'-':_ -> continue_lex_with_single_tok 2 LeftArrow

        '(':_ -> continue_lex_with_single_tok 1 OParen
        ')':_ -> continue_lex_with_single_tok 1 CParen
        '[':_ -> continue_lex_with_single_tok 1 OBrack
        ']':_ -> continue_lex_with_single_tok 1 CBrack
        ',':_ -> continue_lex_with_single_tok 1 Comma
        '.':_ -> continue_lex_with_single_tok 1 Period
        '?':_ -> continue_lex_with_single_tok 1 Question
        '~':_ -> continue_lex_with_single_tok 1 Tilde
        '#':_ -> continue_lex_with_single_tok 1 Hash
        '$':_ -> continue_lex_with_single_tok 1 Dollar
        '!':_ -> continue_lex_with_single_tok 1 Bang
        '=':_ -> continue_lex_with_single_tok 1 Equal
        ':':_ -> continue_lex_with_single_tok 1 Colon
        '+':_ -> continue_lex_with_single_tok 1 Plus
        '-':_ -> continue_lex_with_single_tok 1 Minus
        '*':_ -> continue_lex_with_single_tok 1 Star
        '/':_ -> continue_lex_with_single_tok 1 Slash
        '%':_ -> continue_lex_with_single_tok 1 Percent
        '<':_ -> continue_lex_with_single_tok 1 Less
        '>':_ -> continue_lex_with_single_tok 1 Greater
        '^':_ -> continue_lex_with_single_tok 1 Caret
        '&':_ -> continue_lex_with_single_tok 1 Amper
        '|':_ -> continue_lex_with_single_tok 1 Pipe

        -- braces and semicolons are handled by indentation tokens
        -- cannot use continue_lex_with_nothing because that function does not include indent tokens
        '{':_ -> continue_lex_with [] 1
        '}':_ -> continue_lex_with [] 1
        ';':_ -> continue_lex_with [] 1

        '"':strlit -> lex_str_lit strlit
        '\'':chrlit -> lex_char_lit chrlit

        [] -> prevtoks ++ alldedents ++ [Right $ make_token 0 1 EOF]
            where
                -- TODO: use the same span as other dedent tokens
                alldedents =
                    Right (make_token 0 1 Newline) :
                    concatMap make_dedent (init indent_stack)

                make_dedent (IndentationSensitive _) = [Right $ make_token 0 1 Dedent]
                make_dedent IndentationInsensitive = [] -- the parser will handle these when it finds a dedent token instead of a matching '}'

        entire@(other:_)
            | isAlpha other || other == '_' -> lex_iden entire
            | isDigit other -> lex_nr entire
            | isSpace other -> continue_lex_with_nothing 1
            | otherwise -> continue_lex_with_single_err 1 $ BadChar other

    where
        -- helpers {{{
        continue_lex_with things advanceamt = lex' (prevtoks ++ indentation_tokens ++ things) new_indent_stack $ lexer `advance` advanceamt
        continue_lex_with_nothing advanceamt = lex' prevtoks indent_stack $ lexer `advance` advanceamt

        continue_lex_with_single_tok len tok = continue_lex_with [Right $ make_token 0 len tok] len
        continue_lex_with_single_err len err = continue_lex_with [Left $ make_error 0 len err] len

        make_token start len tok = Located (make_span_from_lexer start len) tok

        make_error start len err = err $ make_span_from_lexer start len

        make_span_from_lexer start len =
            Span (make_location file $ ind + start) (make_location file $ ind + start + len)
            where
                file = sourcefile lexer
                ind = source_location lexer

        (new_indent_stack, indentation_tokens) =
            if null prevtoks
                then ([IndentationSensitive 0], [])
                else
                    let remain = remaining lexer
                    in
                        process_cbrace remain .
                        process_semi   remain .
                        process_obrace remain .
                        process_dedent cur_indent last_indent .
                        process_nl     cur_indent last_indent .
                        process_indent cur_indent last_indent $ (indent_stack, [])

            where
                cur_indent = foldl' count_indent (Just 0) str_before_lexer
                    where
                        str_before_lexer = takeWhile (/='\n') $ reverse $ take (source_location lexer) $ source $ sourcefile lexer

                        count_indent (Just acc) ' ' = Just $ acc + 1
                        count_indent (Just acc) '\t' = Just $ (acc `div` 8 + 1) * 8
                        count_indent _ _ = Nothing

                last_indent = case head indent_stack of
                    IndentationInsensitive -> Nothing
                    IndentationSensitive x -> Just x

                last_is_semi =
                    case find isRight $ reverse prevtoks of
                        Just (Right (Located _ Semicolon)) -> True
                        _ -> False

                -- TODO: support \ at the end of a line to prevent indent tokens
                -- TODO: support ~ at the end of a line to start a indentation sensitive frame
                process_indent (Just curlvl) (Just lastlvl) (stack, toks)
                    | curlvl > lastlvl =
                        (new_frame:stack, toks ++ [Right $ make_tok_at_cur Indent])
                        where
                            new_frame = IndentationSensitive curlvl
                process_indent _ _ st = st

                process_obrace ('{':_) (stack, toks) = (IndentationInsensitive : stack, toks ++ [Right $ make_tok_at_cur OBrace])
                process_obrace _ st = st

                process_nl (Just curlvl) (Just lastlvl) (stack, toks)
                    -- TODO: do not emit newline if first character is '{'
                    | curlvl == lastlvl =
                        if last_is_semi
                            then (stack, toks)
                            else (stack, toks ++ [Right $ make_tok_at_nl_before Newline])

                process_nl _ _ st = st

                process_semi (';':_) (stack, toks) = (stack, toks ++ [Right $ make_tok_at_cur Semicolon])
                process_semi _ st = st

                process_dedent (Just curlvl) (Just lastlvl) (stack, toks)
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
                            (
                                after_pop,
                                toks ++
                                (if last_is_semi
                                    then []
                                    else [Right $ make_tok_at_nl_before Newline]) ++
                                replicate num_pop (Right $ make_tok_at_cur Dedent) ++ -- TODO: change dedent span to something better so that it doesnt include beginning of the next token
                                (if is_valid_level
                                    then []
                                    else [Left $ make_error 0 1 BadDedent])
                            )
                process_dedent _ _ st = st

                process_cbrace ('}':_) (stack, toks) =
                    case head stack of
                        IndentationInsensitive -> (tail stack, newtoks)
                        IndentationSensitive _ -> (stack, newtoks) -- do not pop on the stack, but the parser will handle the error message when there is a random '}' that appears
                    where
                        newtoks = toks ++ [Right $ make_tok_at_cur CBrace]
                process_cbrace _ st = st

                make_tok_at_cur = make_token 0 1

                make_tok_relative_to_nl_before off = make_token $ off_to_nl + off
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
                                    find isRight (reverse prevtoks) >>= \ (Right (Located (Span _ endloc) _)) ->
                                    let endind = ind_of_loc endloc
                                    in elemIndex '\n' (drop endind $ source $ sourcefile lexer) >>= \ from_tok_ind ->
                                    Just $ from_tok_ind + endind - source_location lexer

                                from_cur_pos =
                                    elemIndex '\n' (reverse $ take (source_location lexer) (source $ sourcefile lexer)) >>= \ x ->
                                    Just $ -x - 1

                make_tok_at_nl_before = make_tok_relative_to_nl_before 0 1
        -- }}}
        -- {{{ str and char literals
        lex_str_or_char_lit is_char_lit lex_from =
            case lex_str_or_char_lit' lex_from of
                Right (contents, tok_len_minus1) ->
                    let tok_len = tok_len_minus1 + 1 -- + 1 for starting quote
                    in if is_char_lit
                    then if length contents == 1
                        then continue_lex_with_single_tok tok_len $ CharLit $ head contents
                        else continue_lex_with_single_err tok_len $ MulticharChar
                    else continue_lex_with_single_tok tok_len $ StringLit contents

                Left err_len ->
                    if is_char_lit
                        -- + 1 for starting quote
                    then continue_lex_with_single_err (err_len + 1) UntermChar
                    else continue_lex_with_single_err (err_len + 1) UntermStr
            where
                lex_str_or_char_lit' rest =
                    let (cur_contents, after_lit) = break end_pred rest
                    in case after_lit of
                        x:_ | x == start_delim ->
                            Right (cur_contents, length cur_contents + 1) -- + 1 for terminating quote
                        _ ->
                            let (whsp, after_wh) = span isSpace after_lit
                            in case after_wh of
                                x:after_continuing_start_delim | x == start_delim ->
                                    let total_len next_len = length cur_contents + length whsp + 1 + next_len -- + 1 for starting quote of next segment
                                    in case lex_str_or_char_lit' after_continuing_start_delim of
                                        Right (next_contents, next_tok_len) -> Right (cur_contents ++ "\n" ++ next_contents, total_len next_tok_len)
                                        Left err_len -> Left $ total_len err_len

                                _ ->
                                    Left $ length cur_contents

                start_delim = if is_char_lit then '\'' else '"'
                end_pred ch = ch == '\n' || ch == start_delim

        lex_str_lit = lex_str_or_char_lit False
        lex_char_lit = lex_str_or_char_lit True
        -- }}}
        -- lex_iden {{{
        lex_iden entire =
            continue_lex_with_single_tok iden_len (
                case iden_contents of
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
                    _ -> Identifier iden_contents
            )
            where
                iden_contents = alphanum ++ apostrophes
                    where
                        iden_pred ch = isAlpha ch || isDigit ch || ch == '_'
                        (alphanum, rest) = span iden_pred entire
                        apostrophes = takeWhile (=='\'') rest
                iden_len = length iden_contents
        -- }}}
        -- {{{ lex_nr
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
        lex_nr entire =
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
                make_errors_from_invalid_digits = map (\ (dig, ind) -> Left $ InvalidDigit dig (make_span_from_lexer ind 1) (make_span_from_lexer 0 total_len))

                check_int_no_float digit_chk tok =
                    if not $ null digits
                    then
                        let invalid_digits = get_invalid_digits digit_chk
                        in if null invalid_digits
                            then
                                case decimal_digits of
                                    Nothing -> continue_lex_with_single_tok total_len tok
                                    Just _ -> continue_lex_with_single_err total_len NonDecimalFloat
                            else continue_lex_with (make_errors_from_invalid_digits invalid_digits) total_len
                    else continue_lex_with_single_err total_len MissingDigits

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
                                    Nothing -> continue_lex_with_single_tok total_len $ IntLit Dec $ read_lit_digits 10 (^)
                                    Just dd -> continue_lex_with_single_tok total_len $ FloatLit $ fromIntegral (read_lit_digits 10 (^)) + read_digits (map (fromIntegral . negate) [1..decimal_len]) 10 dd (**)
                            else continue_lex_with (make_errors_from_invalid_digits invalid_digits) total_len
                    else continue_lex_with_single_err total_len MissingDigits

                Just b ->
                    continue_lex_with_single_err total_len $ InvalidBase b $ make_span_from_lexer 1 1
        -- }}}

advance :: Lexer -> Int -> Lexer
advance lexer n = lexer
                  { source_location = source_location lexer + n
                  , remaining = drop n $ remaining lexer
                  }
