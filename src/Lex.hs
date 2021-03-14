module Lex where

import File
import Location

import Data.Char(isDigit, isAlpha, isHexDigit, isOctDigit, digitToInt)
import Data.List(foldl')

import qualified Message

data IntLitBase = Dec
                | Oct
                | Hex
                | Bin
                deriving Show

data Token = OParen
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
           | Var
           | Fun
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
           deriving Show

data Lexer = Lexer
             { sourcefile :: File
             , sourceLocation :: Int
             , remaining :: String
             , lnn :: Int
             , coln :: Int
             }

data LexError = BadChar Char Span
              | UntermMultilineComment Span
              | UntermStr Span
              | UntermChar Span
              | MulticharChar Span
              | InvalidBase Char Span Span
              | InvalidDigit Char Span Span
              | NonDecimalFloat Span
              | MissingDigits Span

instance Message.ToDiagnostic LexError where
    toDiagnostic err =
        case err of
            BadChar ch sp -> simple sp "E0001" "bad-char" $ "bad character '" ++ [ch] ++ "'"
            UntermMultilineComment sp -> simple sp "E0002" "unterm-multiln-cmt" "unterminated multiline comment"
            UntermStr sp -> simple sp "E0003" "unterm-strlit" "string literal missing closing quote ('\"')"
            UntermChar sp -> simple sp "E0004" "unterm-chrlit" "character literal missing closing quote (''')"
            MulticharChar sp -> simple sp "E0005" "multichr-chrlit" "character literal must contain only one character"

            InvalidBase basechr basechrsp litsp ->
                Message.SimpleDiag Message.Error (Just basechrsp) (Message.makeCode "E0006") (Just "invalid-intlit-base") [
                    Message.makeUnderlinesSection [
                        Message.UnderlineMessage basechrsp Message.ErrorUnderline Message.Primary $ "invalid integer literal base '" ++ [basechr] ++ "' (must be one of 'x', 'o', or 'b')",
                        Message.UnderlineMessage litsp Message.NoteUnderline Message.Secondary "in this integer literal"
                    ]
                ]

            InvalidDigit digitchr digitsp litsp ->
                Message.SimpleDiag Message.Error (Just digitsp) (Message.makeCode "E0007") (Just "invalid-digit") [
                    Message.makeUnderlinesSection [
                        Message.UnderlineMessage digitsp Message.ErrorUnderline Message.Primary $ "invalid digit '" ++ [digitchr] ++ "'",
                        Message.UnderlineMessage litsp Message.NoteUnderline Message.Secondary "in this integer literal"
                    ]
                ]

            NonDecimalFloat sp -> simple sp "E0008" "nondecimal-floatlit" "non-decimal floating point literals are not supported"
            MissingDigits sp -> simple sp "E0009" "no-digits" "integer literal must have digits"

        where
            simple sp code nm msg = Message.SimpleDiag Message.Error (Just sp) (Message.makeCode code) (Just nm) [
                    Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary msg]
                ]

lex :: File -> [Either LexError (Located Token)]
lex f = lex' $ Lexer
           { sourcefile = f
           , sourceLocation = 0
           , remaining = source f
           , lnn = 1
           , coln = 1
           }

lex' :: Lexer -> [Either LexError (Located Token)]
lex' lexer =
    case remaining lexer of
        -- TODO: replace this with Data.Char.isSpace
        '\r':_ -> skipChar
        '\n':_ -> skipChar
        ' ' :_ -> skipChar
        '\t':_ -> skipChar

        -- comments {{{
        '/':'/':next ->
            let comment = takeWhile (/='\n') next
            in continueLex $ length comment + 3

        '/':'*':next ->
            case commentLength of
                Right cl -> continueLex cl
                Left charsToEnd -> [Left $ makeError 0 charsToEnd UntermMultilineComment]
            where
                commentLength = case charsUntilCommentEnd next of
                    Right cl -> Right $ 4 + cl
                    Left charsToEnd -> Left $ 2 + charsToEnd

                charsUntilCommentEnd ('/':'*':rest) = charsUntilCommentEnd rest
                charsUntilCommentEnd ('*':'/':_) = Right 0
                charsUntilCommentEnd (_:rest) =
                    case charsUntilCommentEnd rest of
                        Right l -> Right $ 1 + l
                        Left l -> Left $ 1 + l
                charsUntilCommentEnd [] = Left 0
                -- TODO: check for '* /' and put a note there
                -- TODO: nesting multiline comments
        -- }}}

        -- TODO: indentation

        '<':'<':'=':_ -> continueLexWithTok 3 DoubleLessEqual
        '>':'>':'=':_ -> continueLexWithTok 3 DoubleGreaterEqual

        '+':'=':_ -> continueLexWithTok 2 PlusEqual
        '-':'=':_ -> continueLexWithTok 2 MinusEqual
        '*':'=':_ -> continueLexWithTok 2 StarEqual
        '/':'=':_ -> continueLexWithTok 2 SlashEqual
        '%':'=':_ -> continueLexWithTok 2 PercentEqual
        '<':'=':_ -> continueLexWithTok 2 LessEqual
        '>':'=':_ -> continueLexWithTok 2 GreaterEqual
        '!':'=':_ -> continueLexWithTok 2 BangEqual
        '&':'=':_ -> continueLexWithTok 2 AmperEqual
        '|':'=':_ -> continueLexWithTok 2 PipeEqual
        '^':'=':_ -> continueLexWithTok 2 CaretEqual

        '=':'=':_ -> continueLexWithTok 2 DoubleEqual
        '+':'+':_ -> continueLexWithTok 2 DoublePlus
        '-':'-':_ -> continueLexWithTok 2 DoubleMinus
        '&':'&':_ -> continueLexWithTok 2 DoubleAmper
        '|':'|':_ -> continueLexWithTok 2 DoublePipe
        '<':'<':_ -> continueLexWithTok 2 DoubleLess
        '>':'>':_ -> continueLexWithTok 2 DoubleGreater
        ':':':':_ -> continueLexWithTok 2 DoubleColon

        '-':'>':_ -> continueLexWithTok 2 RightArrow
        '<':'-':_ -> continueLexWithTok 2 LeftArrow

        '(':_ -> singleCharTok OParen
        ')':_ -> singleCharTok CParen
        '[':_ -> singleCharTok OBrack
        ']':_ -> singleCharTok CBrack
        '{':_ -> singleCharTok OBrace
        '}':_ -> singleCharTok CBrace
        ';':_ -> singleCharTok Semicolon
        ',':_ -> singleCharTok Comma
        '.':_ -> singleCharTok Period
        '?':_ -> singleCharTok Question
        '~':_ -> singleCharTok Tilde
        '#':_ -> singleCharTok Hash
        '$':_ -> singleCharTok Dollar
        '!':_ -> singleCharTok Bang
        '=':_ -> singleCharTok Equal
        ':':_ -> singleCharTok Colon
        '+':_ -> singleCharTok Plus
        '-':_ -> singleCharTok Minus
        '*':_ -> singleCharTok Star
        '/':_ -> singleCharTok Slash
        '%':_ -> singleCharTok Percent
        '<':_ -> singleCharTok Less
        '>':_ -> singleCharTok Greater
        '^':_ -> singleCharTok Caret
        '&':_ -> singleCharTok Amper
        '|':_ -> singleCharTok Pipe

        '"':strlit -> lexStrLit strlit
        '\'':chrlit -> lexCharLit chrlit

        [] -> []

        entire@(other:_)
            | isAlpha other -> lexIden entire
            | isDigit other -> lexNr entire
            | otherwise -> continueLexWithErr 1 $ BadChar other

    where
        -- helpers {{{
        continueLex advanceamt = lex' $ lexer `advance` advanceamt
        skipChar = continueLex 1

        continueLexWithTok len tok = (Right $ makeToken 0 len tok) : continueLex len
        continueLexWithErr len err = (Left $ makeError 0 len err) : continueLex len

        makeToken start len tok = Located (makeSpanFromLexer start len) tok
        makeError start len err = err $ makeSpanFromLexer start len

        singleCharTok = continueLexWithTok 1

        makeSpanFromLexer start len =
            makeSpan file (sourceLocation startlexer) (lnn startlexer) (coln startlexer) len (lnn endlexer) (coln endlexer)
            where
                file = sourcefile lexer
                startlexer = lexer `advance` start
                endlexer = startlexer `advance` len
        -- }}}
        -- {{{ str and char literals
        lexStrOrCharLit isCharLit startingDelim rest =
            case litLength of
                Right len
                    | isCharLit && len /= 1 -> continueLexWithErr (2 + len) MulticharChar
                    | isCharLit -> continueLexWithTok (2 + len) $ CharLit $ rest !! 1
                    | otherwise -> continueLexWithTok (2 + len) $ StringLit $ take len rest

                Left len
                    | isCharLit -> continueLexWithErr (1 + len) UntermChar
                    | otherwise -> continueLexWithErr (1 + len) UntermStr

            where
                litLength = charsUntilClosingDelim rest

                charsUntilClosingDelim [] = Left 0
                charsUntilClosingDelim (chr:more)
                    | chr == '\n' = Left 0
                    | chr == startingDelim = Right 0
                    | otherwise =
                        case charsUntilClosingDelim more of
                            Right l -> Right $ 1 + l
                            Left l -> Left $ 1 + l

        lexStrLit = lexStrOrCharLit False '"'
        lexCharLit = lexStrOrCharLit True '\''
        -- }}}
        -- lexIden {{{
        lexIden entire =
            continueLexWithTok idenLen $ Identifier $ idenContents ++ aposes
            where (idenContents, rest) = break (not . idenPred) entire
                  aposes = takeWhile (=='\'') rest
                  idenLen = length idenContents + length aposes
                  idenPred ch = isAlpha ch || isDigit ch
        -- }}}
        -- {{{ lexNr
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

                (0\w)?([0-9a-fA-F]+)(\.[0-9a-fA-F]+)?
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
            }}}
        }}} -}
        lexNr entire =
            let (base, baseLen, afterBase) =
                    case entire of
                        '0':ch:after | isAlpha ch -> (Just ch, 2, after)
                        after -> (Nothing, 0, after)

                (digits, digitsLen, afterDigits) =
                    (d, length d, after)
                    where (d, after) = break (not . isHexDigit) afterBase

                (decimalDigits, decimalLen, _) =
                    case afterDigits of
                        '.':(rest@(firstDigit:_))
                            | isHexDigit firstDigit ->
                                (Just f, length f + 1, drop 1 more)
                                where (f, more) = break (not . isHexDigit) rest

                        after -> (Nothing, 0, after)

                totalLen = baseLen + digitsLen + decimalLen

                readDigits :: (Num a) => [a] -> a -> String -> (a -> a -> a) -> a
                readDigits places readBase str powerFn = foldl' accFn 0 $ zip places str
                    where
                        accFn acc (place, char) = acc + ((readBase `powerFn` place) * fromIntegral (digitToInt char))

                readLitDigits readBase powerFn = readDigits (reverse ([0..toInteger digitsLen - 1])) readBase digits powerFn

                getInvalidDigits chk = filter (not . chk . fst) $ zip digits [baseLen..]
                makeErrorsFromInvalidDigits = map (\ (dig, ind) -> Left $ InvalidDigit dig (makeSpanFromLexer ind 1) (makeSpanFromLexer 0 totalLen))

                checkIntNoFloat digitChk tok =
                    if length digits > 0
                    then
                        let invalidDigits = getInvalidDigits digitChk
                        in if length invalidDigits == 0
                            then
                                case decimalDigits of
                                    Nothing -> continueLexWithTok totalLen tok
                                    Just _ -> continueLexWithErr totalLen NonDecimalFloat
                            else makeErrorsFromInvalidDigits invalidDigits ++ continueLex totalLen
                    else continueLexWithErr totalLen MissingDigits

            in case base of
                Just 'x' ->
                    checkIntNoFloat (const True) $ IntLit Hex $ readLitDigits 16 (^)
                Just 'b' ->
                    checkIntNoFloat (\ d -> d == '0' || d == '1') $ IntLit Bin $ readLitDigits 2 (^)
                Just 'o' ->
                    checkIntNoFloat isOctDigit $ IntLit Oct $ readLitDigits 8 (^)
                Nothing ->
                    if length digits > 0
                    then
                        let invalidDigits = getInvalidDigits isDigit
                        in if length invalidDigits == 0
                            then
                                case decimalDigits of
                                    Nothing -> continueLexWithTok totalLen $ IntLit Dec $ readLitDigits 10 (^)
                                    Just dd -> continueLexWithTok totalLen $ FloatLit $ (fromIntegral $ readLitDigits 10 (^)) + (readDigits (map (fromIntegral . negate) [1..decimalLen]) 10 dd (**))
                            else makeErrorsFromInvalidDigits invalidDigits ++ continueLex totalLen
                    else continueLexWithErr totalLen MissingDigits

                Just b ->
                    continueLexWithErr totalLen $ InvalidBase b $ makeSpanFromLexer 1 1
        -- }}}

advance :: Lexer -> Int -> Lexer
advance lexer 0 = lexer
advance lexer 1 = lexer
                  { sourceLocation = sourceLocation lexer + 1
                  , remaining = drop 1 $ remaining lexer
                  , lnn = nextlnn
                  , coln = nextcoln
                  }
    where
        overNL = case remaining lexer of
            '\n':_ -> True
            _ -> False

        nextlnn = if overNL
            then lnn lexer + 1
            else lnn lexer
        nextcoln = if overNL
            then 1
            else coln lexer + 1

advance lexer n = advance (advance lexer 1) $ n - 1
