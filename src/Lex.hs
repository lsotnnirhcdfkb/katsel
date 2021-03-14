module Lex where

import File
import Location

import Data.Char(isDigit, isAlpha, isHexDigit, isOctDigit, digitToInt, isSpace)
import Data.List(foldl')

import qualified Message

data IntLitBase
    = Dec
    | Oct
    | Hex
    | Bin
    deriving Show

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

data IndentFrame
    = IndentationSensitive Int
    | IndentationInsensitive
    deriving Show

data Lexer = Lexer
             { sourcefile :: File
             , sourceLocation :: Int
             , remaining :: String
             , lnn :: Int
             , coln :: Int
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
    deriving Show

instance Message.ToDiagnostic LexError where
    toDiagnostic err =
        case err of
            BadChar ch sp -> simple sp "E0001" "bad-char" $ "bad character '" ++ [ch] ++ "'"
            UntermMultilineComment sp -> simple sp "E0002" "unterm-multiln-cmt" "unterminated multiline comment"
            UntermStr sp -> simple sp "E0003" "unterm-strlit" "string literal missing closing quote ('\"')"
            UntermChar sp -> simple sp "E0004" "unterm-chrlit" "character literal missing closing quote (''')"
            MulticharChar sp -> simple sp "E0005" "multichr-chrlit" "character literal must contain exactly one character"

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

            BadDedent sp -> simple sp "E0010" "bad-dedent" "dedent to level that does not match any other indentation level"

        where
            simple sp code nm msg = Message.SimpleDiag Message.Error (Just sp) (Message.makeCode code) (Just nm) [
                    Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary msg]
                ]

lex :: File -> [Either LexError (Located Token)]
lex f = lex' [] [IndentationSensitive 0] $ Lexer
           { sourcefile = f
           , sourceLocation = 0
           , remaining = source f
           , lnn = 1
           , coln = 1
           }

lex' :: [Either LexError (Located Token)] -> [IndentFrame] -> Lexer -> [Either LexError (Located Token)]
lex' prevtoks indentStack lexer =
    case remaining lexer of
        -- comments {{{
        '/':'/':next ->
            let comment = takeWhile (/='\n') next
            in continueLexWithNothing $ length comment + 3

        '/':'*':next ->
            case commentLength next of
                Right cl -> continueLexWithNothing cl
                Left charsToEnd -> [Left $ makeError 0 charsToEnd UntermMultilineComment]
            where
                commentLength afterSlashStar =
                    case charsUntilCommentEnd 0 afterSlashStar of
                        Right cl -> Right $ 4 + cl
                        Left charsToEnd -> Left $ 2 + charsToEnd

                charsUntilCommentEnd acc entire@('/':'*':rest) =
                    case commentLength rest of
                        Right innerCommentLength ->
                            charsUntilCommentEnd (acc + innerCommentLength) afterInnerComment
                            where
                                afterInnerComment = drop innerCommentLength entire
                        Left unterminatedInnerCommentLength -> Left $ acc + unterminatedInnerCommentLength

                charsUntilCommentEnd acc ('*':'/':_) = Right acc
                charsUntilCommentEnd acc [] = Left acc
                charsUntilCommentEnd acc (_:rest) = charsUntilCommentEnd (acc + 1) rest

                -- TODO: check for '* /' and put a note there
        -- }}}

        '<':'<':'=':_ -> continueLexWithSingleTok 3 DoubleLessEqual
        '>':'>':'=':_ -> continueLexWithSingleTok 3 DoubleGreaterEqual

        '+':'=':_ -> continueLexWithSingleTok 2 PlusEqual
        '-':'=':_ -> continueLexWithSingleTok 2 MinusEqual
        '*':'=':_ -> continueLexWithSingleTok 2 StarEqual
        '/':'=':_ -> continueLexWithSingleTok 2 SlashEqual
        '%':'=':_ -> continueLexWithSingleTok 2 PercentEqual
        '<':'=':_ -> continueLexWithSingleTok 2 LessEqual
        '>':'=':_ -> continueLexWithSingleTok 2 GreaterEqual
        '!':'=':_ -> continueLexWithSingleTok 2 BangEqual
        '&':'=':_ -> continueLexWithSingleTok 2 AmperEqual
        '|':'=':_ -> continueLexWithSingleTok 2 PipeEqual
        '^':'=':_ -> continueLexWithSingleTok 2 CaretEqual

        '=':'=':_ -> continueLexWithSingleTok 2 DoubleEqual
        '+':'+':_ -> continueLexWithSingleTok 2 DoublePlus
        '-':'-':_ -> continueLexWithSingleTok 2 DoubleMinus
        '&':'&':_ -> continueLexWithSingleTok 2 DoubleAmper
        '|':'|':_ -> continueLexWithSingleTok 2 DoublePipe
        '<':'<':_ -> continueLexWithSingleTok 2 DoubleLess
        '>':'>':_ -> continueLexWithSingleTok 2 DoubleGreater
        ':':':':_ -> continueLexWithSingleTok 2 DoubleColon

        '-':'>':_ -> continueLexWithSingleTok 2 RightArrow
        '<':'-':_ -> continueLexWithSingleTok 2 LeftArrow

        '(':_ -> continueLexWithSingleTok 1 OParen
        ')':_ -> continueLexWithSingleTok 1 CParen
        '[':_ -> continueLexWithSingleTok 1 OBrack
        ']':_ -> continueLexWithSingleTok 1 CBrack
        '{':_ -> continueLexWithSingleTok 1 OBrace
        '}':_ -> continueLexWithSingleTok 1 CBrace
        ';':_ -> continueLexWithSingleTok 1 Semicolon
        ',':_ -> continueLexWithSingleTok 1 Comma
        '.':_ -> continueLexWithSingleTok 1 Period
        '?':_ -> continueLexWithSingleTok 1 Question
        '~':_ -> continueLexWithSingleTok 1 Tilde
        '#':_ -> continueLexWithSingleTok 1 Hash
        '$':_ -> continueLexWithSingleTok 1 Dollar
        '!':_ -> continueLexWithSingleTok 1 Bang
        '=':_ -> continueLexWithSingleTok 1 Equal
        ':':_ -> continueLexWithSingleTok 1 Colon
        '+':_ -> continueLexWithSingleTok 1 Plus
        '-':_ -> continueLexWithSingleTok 1 Minus
        '*':_ -> continueLexWithSingleTok 1 Star
        '/':_ -> continueLexWithSingleTok 1 Slash
        '%':_ -> continueLexWithSingleTok 1 Percent
        '<':_ -> continueLexWithSingleTok 1 Less
        '>':_ -> continueLexWithSingleTok 1 Greater
        '^':_ -> continueLexWithSingleTok 1 Caret
        '&':_ -> continueLexWithSingleTok 1 Amper
        '|':_ -> continueLexWithSingleTok 1 Pipe

        '"':strlit -> lexStrLit strlit
        '\'':chrlit -> lexCharLit chrlit

        [] -> prevtoks

        entire@(other:_)
            | isAlpha other -> lexIden entire
            | isDigit other -> lexNr entire
            | isSpace other -> continueLexWithNothing 1
            | otherwise -> continueLexWithSingleErr 1 $ BadChar other

    where
        -- helpers {{{
        continueLexWith things advanceamt = lex' (prevtoks ++ indentTokens ++ things) newIndentStack $ lexer `advance` advanceamt
        continueLexWithNothing advanceamt = lex' prevtoks indentStack $ lexer `advance` advanceamt

        (newIndentStack, indentTokens) =
            case prevtoks of
                [] -> ([IndentationSensitive 0], [])
                _ ->
                    processCurIndent

            where
                curIndent = countIndent (Just 0) indentStr
                    where
                        strBeforeLexer = reverse $ take (sourceLocation lexer) $ source $ sourcefile lexer
                        indentStr = takeWhile (/='\n') strBeforeLexer

                        countIndent res [] = res
                        countIndent (Just acc) (' ':more) = countIndent (Just $ acc + 1) more
                        countIndent (Just acc) ('\t':more) = countIndent (Just $ (acc `div` 8 + 1) * 8) more
                        countIndent Nothing _ = Nothing
                        countIndent _ _ = Nothing

                processCurIndent =
                    case head indentStack of
                        IndentationInsensitive -> (indentStack, [])

                        IndentationSensitive lastlvl ->
                            -- TODO: to not insert any tokens if the current character is '{'
                            -- TODO: support \ at the end of a line to prevent indent tokens
                            -- TODO: support ~ at the end of a line to start a indentation sensitive frame
                            case curIndent of
                                Just curlvl
                                    | curlvl < lastlvl -> doDedent curlvl
                                    | curlvl > lastlvl -> doIndent curlvl
                                    | curlvl == lastlvl -> doNewline curlvl

                                _ -> (indentStack, [])
                doDedent curlvl =
                    -- TODO: this nl token's span should also be at the previous newline
                    let getAmountToPop acc (cur:rest)
                            | canPop cur = getAmountToPop (acc + 1) rest
                            | otherwise = acc
                        getAmountToPop _ [] = error "indent stack should never be empty"
                        canPop (IndentationSensitive ind)
                            | curlvl < ind = True
                            | otherwise = False
                        canPop IndentationInsensitive = False

                        popAmt = getAmountToPop 0 indentStack
                        poppedStack = drop popAmt indentStack

                        isValidLevel =
                            case head poppedStack of
                                IndentationSensitive lvl
                                    | curlvl > lvl -> False

                                _ -> True
                    in (
                        poppedStack,
                        (Right $ makeTokAtCur Newline) : (replicate popAmt $ Right $ makeTokAtCur Dedent) ++
                        if isValidLevel
                        then []
                        else [Left $ makeError 0 1 BadDedent]
                    )
                doIndent curlvl =
                    let newFrame = IndentationSensitive curlvl
                    in (newFrame:indentStack, [Right $ makeTokAtCur Indent])
                doNewline _ =
                    -- TODO: do not insert if last token is ';'
                    -- TODO: this span should be at the newline before
                    (indentStack, [Right $ makeTokAtCur Newline])

                makeTokAtCur = makeToken 0 1

        continueLexWithSingleTok len tok = continueLexWith [Right $ makeToken 0 len tok] len
        continueLexWithSingleErr len err = continueLexWith [Left $ makeError 0 len err] len

        makeToken start len tok = Located (makeSpanFromLexer start len) tok
        makeError start len err = err $ makeSpanFromLexer start len

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
                    | isCharLit && len /= 1 -> continueLexWithSingleErr (2 + len) MulticharChar
                    | isCharLit -> continueLexWithSingleTok (2 + len) $ CharLit $ rest !! 1
                    | otherwise -> continueLexWithSingleTok (2 + len) $ StringLit $ take len rest

                Left len
                    | isCharLit -> continueLexWithSingleErr (1 + len) UntermChar
                    | otherwise -> continueLexWithSingleErr (1 + len) UntermStr

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
            continueLexWithSingleTok idenLen (
                case idenContents of
                    "data" -> Data
                    "impl" -> Impl
                    "fun" -> Fun
                    "var" -> Var
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
                    _ -> Identifier idenContents
            )
            where
                idenContents = alphanum ++ apostrophes
                    where
                        idenPred ch = isAlpha ch || isDigit ch
                        (alphanum, rest) = break (not . idenPred) entire
                        apostrophes = takeWhile (=='\'') rest
                idenLen = length idenContents
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
                - missing digits (there are no integral digits)
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
                                    Nothing -> continueLexWithSingleTok totalLen tok
                                    Just _ -> continueLexWithSingleErr totalLen NonDecimalFloat
                            else continueLexWith (makeErrorsFromInvalidDigits invalidDigits) totalLen
                    else continueLexWithSingleErr totalLen MissingDigits

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
                                    Nothing -> continueLexWithSingleTok totalLen $ IntLit Dec $ readLitDigits 10 (^)
                                    Just dd -> continueLexWithSingleTok totalLen $ FloatLit $ (fromIntegral $ readLitDigits 10 (^)) + (readDigits (map (fromIntegral . negate) [1..decimalLen]) 10 dd (**))
                            else continueLexWith (makeErrorsFromInvalidDigits invalidDigits) totalLen
                    else continueLexWithSingleErr totalLen MissingDigits

                Just b ->
                    continueLexWithSingleErr totalLen $ InvalidBase b $ makeSpanFromLexer 1 1
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
