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
           | Error String -- TODO: error reporting
           deriving Show

data Lexer = Lexer
             { sourcefile :: File
             , sourceLocation :: Int
             , remaining :: String
             }

lex :: File -> [Located Token]
lex f = lex' $ Lexer
           { sourcefile = f
           , sourceLocation = 0
           , remaining = source f
           }

lex' :: Lexer -> [Located Token]
lex' _ = error "todo"
