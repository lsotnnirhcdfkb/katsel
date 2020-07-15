#ifndef LEXERH_INCLUDED
#define LEXERH_INCLUDED

#include <string>

enum TokenType {
    OPAREN,
    CPAREN,
    COMMA,
    PERIOD,
    MINUS,
    PLUS,
    SEMICOLON,
    DIVIDE,
    MULTIPLY,

    NOT,
    NOTEQUAL,
    EQUAL,
    DOUBLEEQUAL,
    GREATER,
    GREATEREQUAL,
    LESS,
    LESSEQUAL,

    AND,
    OR,

    IDENTIFIER,

    STRING,
    INTEGER,
    FLOAT,
    TRUE,
    FALSE,
    NULL_,

    PRINT,

    VOID,
    NAMESPACE,
    CLASS,

    RETURN,

    THIS,

    WHILE,
    FOR,
    IF,
    ELSE,

    EOF_
};

struct Token {
    TokenType type;
    size_t start;
    size_t length;

    int line;
    int column;
};

namespace LexerNS {
    void startLexer();
    Token nextToken();
}

#endif
