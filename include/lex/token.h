
#pragma once

#include "lex/tokentype.h"
#include "utils/file.h"
#include <string>

struct Token
{
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    void (*errf)(Token const &);

    int line;
    int column;

    File *sourcefile;

    inline std::string stringify() const
    {
        return std::string(start, end);
    }
};
