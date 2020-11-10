
#pragma once

#include <string>

#include "utils/file.h"
#include "lex/token.h"
#include "lex/tokentype.h"

class Lexer
{
public:
    Lexer(File &sourcefile);

    Token nextToken();

    inline Token makeSOF()
    {
        return makeToken(TokenType::SOF);
    }

private:
    TokenType getIdentifierType();
    inline bool atEnd()
    {
        return end >= srcend;
    }
    inline bool match(char c)
    {
        if (atEnd())
            return false;

        if (peek() == c)
        {
            advance();
            return true;
        }

        return false;
    }
    inline char advance()
    {
        ++nextcolumn;

        return *(end++);
    }
    inline char peek()
    {
    return *(end);
    }
    inline char peekpeek()
    {
    return *(end + 1);
    }
    inline char consumed()
    {
        return *(end - 1);
    }

    Token makeErrorToken(std::string message);
    inline Token makeToken(TokenType type)
    {
        return Token {type, start, end, "", line, column - 1, &sourcefile};
    }

    inline void nextLine()
    {
        ++nextline;
        nextcolumn = 1;
    }

    std::string::iterator start;
    std::string::iterator end;

    int line;
    int column;
    int nextline;
    int nextcolumn;

    std::string::iterator srcend;

    File &sourcefile;
};

