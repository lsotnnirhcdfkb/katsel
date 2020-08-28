#pragma once

#include "tokentype.h"

/// A Token emitted from the lexer lexing the source file
struct Token
{
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    std::string message;

    int line;
    int column;
};

