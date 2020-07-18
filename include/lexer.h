#ifndef LEXERH_INCLUDED
#define LEXERH_INCLUDED

#include <string>
#include <iostream>

enum TokenType {
    OPAREN,
    CPAREN,
    COMMA,
    PERIOD,
    SEMICOLON,
    QUESTION,
    COLON,

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

    NOTEQUAL,
    EQUAL,
    DOUBLEEQUAL,
    GREATER,
    GREATEREQUAL,
    LESS,
    LESSEQUAL,

    NOT,
    AND,
    OR,
    BITNOT,
    BITAND,
    BITOR,
    BITXOR,

    IDENTIFIER,

    CHAR,
    STRING,

    DECINT,
    OCTINT,
    BININT,
    HEXINT,

    FLOAT,
    TRUE,
    FALSE,
    NULL_,

    PRINT,

    VOID,
    NAMESPACE,
    CLASS,
    ENUM,

    RETURN,

    THIS,

    WHILE,
    FOR,
    IF,
    ELSE,
    SWITCH,

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
    int nextline;
    int nextcolumn;

    std::string::iterator srcend;

    Lexer(std::string &source);
    
    Token nextToken();

private:
    TokenType checkKeyword(int start, std::string compareTo, TokenType type);
    bool atEnd();
    bool match(char c);
    char advance();
    char peek();
    char peekpeek();
    char consumed();

    Token makeErrorToken(std::string message);
    Token makeToken(TokenType type);

    void nextLine();
};

#endif
