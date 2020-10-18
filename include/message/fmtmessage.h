#pragma once

#include <string>
#include <sstream>

#include "lex/tokentype.h"
#include "lex/token.h"
#include "codegen/codegen.h"

namespace msg
{
    const inline std::string unterminatedCharLit()
    {
        return "Unterminated character literal";
    }
    const inline std::string unterminatedStrLit()
    {
        return "Unterminated string literal";
    }
    const inline std::string invalidNumLiteralBase()
    {
        return "Invalid base for integer literal (must be one of 0o, 0b, or 0x)";
    }
    const inline std::string nonDecimalFloatingPoint()
    {
        return "Non-decimal floating point literals are not supported";
    }
    const inline std::string unexpectedCharacter()
    {
        return "Unexpected character";
    }
    inline std::string expectedTokGotTok(TokenType got, TokenType expected)
    {
        std::stringstream ss;
        ss << "Unexpected token " << stringifyTokenType(got) << ", expected " << stringifyTokenType(expected);
        return ss.str();
    }

    const inline std::string expectedPrimaryOrUnary()
    {
        return "Expected primary token or unary operator";
    }
    const inline std::string expectedType()
    {
        return "Expected type";
    }
    const inline std::string expectedEOFTok()
    {
        return "Expected EOF Token - This is a bug";
    }
    const inline std::string expectedDecl()
    {
        return "Expected delcaration";
    }

    const inline std::string duplicateFunction()
    {
        return "Duplicate function";
    }
    const inline std::string cannotRedefineVariable()
    {
        return "Cannot redefine variable";
    }
    inline std::string typeNoOp(Type *ty, Token op)
    {
        std::stringstream ss;
        ss << "Type \"" << ty->stringify() << "\" does not support operator \"" << tokenToStr(op) << "\"";
        return ss.str();
    }
}
