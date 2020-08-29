/// @file token.h
/// A file containing the Token struct

#pragma once

#include "tokentype.h"

/// A Token emitted from the lexer lexing the source file
struct Token
{
    /// The type of this token
    TokenType type;
    /// An iterator to where this token starts in the source
    std::string::iterator start;
    /// An iterator to where this token ends in the source
    std::string::iterator end;

    /// The error message if this token is an error token
    std::string message;

    /// The token's line number
    int line;
    /// The token's column number
    int column;
};

