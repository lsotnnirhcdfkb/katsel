#ifndef LEXERH_INCLUDED
#define LEXERH_INCLUDED

#include <string>
#include <iostream>

enum TokenType {
    OPARN, CPARN,
    OSQUB, CSQUB,
    OCURB, CCURB,
    COMMA,
    PERIOD,
    SEMICOLON,
    QUESTION,
    COLON,
    BANG,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    EQUAL,
    GREATER,
    LESS,
    TILDE,
    AMPER,
    PIPE,
    CARET,

    DOUBLEPLUS,
    DOUBLEMINUS,
    DOUBLEGREATER,
    DOUBLELESS,
    DOUBLEAMPER,
    DOUBLEPIPE,
    DOUBLEEQUAL,
    DOUBLECOLON,

    PLUSEQUAL,
    MINUSEQUAL,
    STAREQUAL,
    SLASHEQUAL,
    BANGEQUAL,
    GREATEREQUAL,
    LESSEQUAL,
    PERCENTEQUAL,
    DOUBLELESSEQUAL,
    DOUBLEGREATEREQUAL,
    AMPEREQUAL,
    PIPEEQUAL,
    CARETEQUAL,

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
    CASE,
    DEFAULT,
    BREAK,
    BREAKALL,
    BREAKTO,
    CONST,
    CONTINUE,
    INLINE,
    VOLATILE,

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
    TokenType getIdentifierType();
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
