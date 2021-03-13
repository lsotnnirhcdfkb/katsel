module Lex where

import File
import Location

import qualified Message

data IntLitBase = Decimal
                | Octal
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
           | Floatlit Double
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
              | UntermMultiline Span

instance Message.ToDiagnostic LexError where
    toDiagnostic (BadChar ch sp) =
        Message.SimpleDiag Message.Error (Just sp) (Message.makeCode "E0001") (Just "bad-char") [
            Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary $ "bad character '" ++ [ch] ++ "'"]
        ]
    toDiagnostic (UntermMultiline sp) =
        Message.SimpleDiag Message.Error (Just sp) (Message.makeCode "E0002") (Just "unterm-multiline-cmt") [
            Message.makeUnderlinesSection [Message.UnderlineMessage sp Message.ErrorUnderline Message.Primary $ "unterminated multiline comment"]
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
        '\r':_ -> skipChar
        '\n':_ -> skipChar
        ' ' :_ -> skipChar
        '\t':_ -> skipChar

        '/':'/':next ->
            let comment = takeWhile (/='\n') next
            in continueLex $ length comment + 3

        '/':'*':next ->
            case commentLength of
                Right cl -> continueLex cl
                Left charsToEnd -> [Left $ makeError charsToEnd UntermMultiline]
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

        [] -> []
        bad:_ -> continueLexWithErr 1 $ BadChar bad

    where
        continueLex advanceamt = lex' $ lexer `advance` advanceamt
        skipChar = continueLex 1

        continueLexWithTok len tok = (Right $ makeToken len tok) : continueLex len
        continueLexWithErr len err = (Left $ makeError len err) : continueLex len

        makeToken len tok = Located (makeSpanFromLexer len) tok
        makeError len err = err $ makeSpanFromLexer len

        singleCharTok = continueLexWithTok 1

        makeSpanFromLexer len =
            makeSpan file srci startln startcoln len endln endcoln
            where
                file = sourcefile lexer
                srci = sourceLocation lexer
                startln = lnn lexer
                startcoln = coln lexer

                advlexer = lexer `advance` len
                endln = lnn advlexer
                endcoln = coln advlexer

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
