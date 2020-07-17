#ifndef LEXERH_INCLUDED
#define LEXERH_INCLUDED

#include <string>

enum TokenType {
    OPAREN,
    CPAREN,
    COMMA,
    PERIOD,
    SEMICOLON,

    PLUS,
    MINUS,
    MULT,
    DIV,

    DOUBLEPLUS,
    DOUBLEMINUS,

    PLUSEQUAL,
    MINUSEQUAL,
    MULTEQUAL,
    DIVEQUAL,

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

    EOF_,
    ERROR
};

struct Token {
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    std::string message;

    int line;
    int column;
};

class Lexer {
public:
    std::string::iterator start;
    std::string::iterator end;
    int line;
    int column;

    std::string::iterator srcend;

    Lexer(std::string &source);
    
    Token nextToken();

private:
    bool atEnd();
    bool match(char c);
    char advance();
    char peek();
    char peekpeek();

    Token makeErrorToken(std::string message);
    Token makeToken(TokenType type);
};

#endif
