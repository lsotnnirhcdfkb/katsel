module Lex where

import File
import Location

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
           | EOF
           | Error String
           deriving Show

data Lexer = Lexer
             { sourcefile :: File
             , sourceLocation :: Int
             , remaining :: String
             , lnn :: Int
             , coln :: Int
             }

lex :: File -> [Located Token]
lex f = lex' $ Lexer
           { sourcefile = f
           , sourceLocation = 0
           , remaining = source f
           , lnn = 1
           , coln = 1
           }

lex' :: Lexer -> [Located Token]
lex' lexer =
    case remaining lexer of
        '\r':_ -> skipChar
        '\n':_ -> skipChar
        ' ' :_ -> skipChar
        '\t':_ -> skipChar

        -- '/':'/':_ -> error "comment"
        -- '/':'*':_ -> error "multiline comment"

        [] ->
            [makeToken 1 EOF]
        _ ->
            continueLexWithTok 1 $ Error "No"

    where
        skipChar = lex' $ lexer `advance` 1

        continueLexWithTok len tok =
            (makeToken len tok) : lex' (lexer `advance` len)

        makeToken len tok =
            Located (makeSpan file srci l c len) tok
            where
                file = sourcefile lexer
                srci = sourceLocation lexer
                l = lnn lexer
                c = coln lexer

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
