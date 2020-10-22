
#pragma once

#include "lex/tokentype.h"
#include "utils/file.h"
#include <string>

struct Token
{
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    std::string message;

    int line;
    int column;

    File *sourcefile;
};

inline std::string tokenToStr(Token t)
{
    return std::string(t.start, t.end);
}
