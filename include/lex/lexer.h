
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

    inline void resetToTok(Token const &t)
    {
        start = end = t.start;
        line = nextline = t.line;
        column = nextcolumn = t.column;
    }

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

    Token makeErrorToken(void (*errf)());
    inline Token makeToken(TokenType type)
    {
        return Token {type, start, end, nullptr, line, column - 1, &sourcefile};
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

