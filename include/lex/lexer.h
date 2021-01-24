#pragma once

#include <string>
#include <vector>

#include "utils/file.h"
#include "lex/token.h"
#include "lex/tokentype.h"

class Lexer {
public:
    Lexer(File &sourcefile);

    Token nextToken();

private:
    bool atEnd();

    char advance();
    bool match(char c);

    char peek();
    char peekpeek();
    char consumed();

    void startToEnd();

    Token lexDigit(char current);
    Token lexIdentifier(bool apostrophesAllowed);

    Token makeErrorToken(NNPtr<void (Token const &)> errf);
    Token makeToken(TokenType type);

    std::string::iterator start;
    std::string::iterator end;

    int startline;
    int startcolumn;
    int endline;
    int endcolumn;

    int indent;
    bool dedenting;
    std::vector<int> indentstack;

    std::string::iterator srcstart;
    std::string::iterator srcend;

    TokenType getIdentifierType();

    File &sourcefile;
};

