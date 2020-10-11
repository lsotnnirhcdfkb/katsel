/// @file lexer.h
/// Lexer class declaration

#pragma once

#include <string>

#include "utils/file.h"
#include "lex/token.h"
#include "lex/tokentype.h"

/// Lexer class to tokenize a sourcefile
class Lexer
{
public:
    /// The constructor of the lexer
    /// @param sourcefile The sourcefile to lex
    Lexer(File &sourcefile);

    /// Return the next token from the source file
    Token nextToken();

    inline Token makeSOF()
    {
        return makeToken(TokenType::SOF);
    }

private:
    /// Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER
    TokenType getIdentifierType();
    /// Return if the lexer is at the end of the sourcefil
    inline bool atEnd()
    {
        return end >= srcend;
    }
    /// Check if the next character in the lexer matches a certain character and consume it
    /// @param c The character to check against
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
    /// Go to the next character, incrementing the column as necessary
    inline char advance()
    {
        ++nextcolumn;

        return *(end++);
    }
    /// Return the next character of the lexer
    inline char peek()
    {
    return *(end);
    }
    /// Return the next next character of the lexer
    inline char peekpeek()
    {
    return *(end + 1);
    }
    /// Return the previous character of the lexer
    inline char consumed()
    {
        return *(end - 1);
    }

    /// Return an error token with a certain message and with the current lexer location
    /// @param message The error message that the error token should have
    Token makeErrorToken(std::string message);
    /// Create a token at the current lexer location with a certain type
    /// @param type The type that the token should be
    inline Token makeToken(TokenType type)
    {
        Token token;

        token.type = type;
        token.start = start;
        token.end = end;
        token.line = line;
        token.column = column - 1;
        token.sourcefile = sourcefile;

        return token;
    }

    /// Increment the line number and reset the column to 1
    inline void nextLine()
    {
        ++nextline;
        nextcolumn = 1;
    }

    /// The start location of the token currently being lexed
    std::string::iterator start;
    /// The end location of the token currently being lexed
    std::string::iterator end;

    /// The line number of the start of the current token
    int line;
    /// The column number of the start of the current token
    int column;
    /// The line number of the end of the current token
    int nextline;
    /// The column number of the end of the current token
    int nextcolumn;

    /// The very end of the sourcefile (for Lexer::atEnd() to work)
    std::string::iterator srcend;

    File &sourcefile;
};

