#pragma once

#include "tokentype.h"

struct Token
{
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    std::string message;

    int line;
    int column;
};

