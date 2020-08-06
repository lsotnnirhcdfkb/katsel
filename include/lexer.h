#pragma once

#include <string>

#include "token.h"
#include "tokentype.h"

class Lexer
{
public:
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

    std::string::iterator start;
    std::string::iterator end;

    int line;
    int column;
    int nextline;
    int nextcolumn;

    std::string::iterator srcend;
};

