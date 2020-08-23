#pragma once

#include <string>

#include "file.h"
#include "token.h"
#include "tokentype.h"

class Lexer
{
public:
    Lexer(File &sourcefile);

    Token nextToken();

private:
    TokenType getIdentifierType();
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

