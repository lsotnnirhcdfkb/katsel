
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

    inline std::string stringify()
    {
        return std::string(start, end);
    }
};
