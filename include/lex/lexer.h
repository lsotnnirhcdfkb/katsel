
#pragma once

#include <string>

#include "utils/file.h"
#include "lex/token.h"
#include "lex/tokentype.h"

class Lexer
{
public:
    Lexer(File &sourcefile);
    Lexer(Token const &t);

    Token nextToken();

    void resetToTok(Token const &t);

private:
    bool atEnd();

    char advance();
    bool match(char c);

    char peek();
    char peekpeek();
    char consumed();

    Token makeErrorToken(void (*errf)(Token const &));
    Token makeToken(TokenType type);

    std::string::iterator start;
    std::string::iterator end;

    int startline;
    int startcolumn;
    int endline;
    int endcolumn;

    bool atLineStart;

    std::string::iterator srcend;

    TokenType getIdentifierType();

    File &sourcefile;
};

