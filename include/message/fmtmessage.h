#pragma once

#include <string>
#include <sstream>

#include "lex/tokentype.h"

namespace msg
{
    inline std::string unterminatedCharLit()
    {
        return "Unterminated character literal";
    }
    inline std::string unterminatedStrLit()
    {
        return "Unterminated string literal";
    }
    inline std::string invalidNumLiteralBase()
    {
        return "Invalid base for integer literal (must be one of 0o, 0b, or 0x)";
    }
    inline std::string nonDecimalFloatingPoint()
    {
        return "Non-decimal floating point literals are not supported";
    }
    inline std::string unexpectedCharacter()
    {
        return "Unexpected character";
    }
    inline std::string expectedTokGotTok(TokenType got, TokenType expected)
    {
        std::stringstream ss;
        ss << "Unexpected token " << stringifyTokenType(got) << ", expected " << stringifyTokenType(expected);
        return ss.str();
    }

    inline std::string expectedPrimaryOrUnary()
    {
        return "Expected primary token or unary operator";
    }
    inline std::string expectedType()
    {
        return "Expected type";
    }
    inline std::string expectedEOFTok()
    {
        return "Expected EOF Token - This is a bug";
    }
    inline std::string expectedDecl()
    {
        return "Expected delcaration";
    }
}
