#pragma once

#include <string>

enum TokenType
{
    // single symbols
    OPARN, CPARN,
    OSQUB, CSQUB,
    OCURB, CCURB,
    COMMA,
    PERIOD,
    SEMICOLON,
    QUESTION,
    COLON,
    BANG,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    EQUAL,
    GREATER,
    LESS,
    TILDE,
    AMPER,
    PIPE,
    CARET,

    // double symbols
    DOUBLEPLUS,
    DOUBLEMINUS,
    DOUBLEGREATER,
    DOUBLELESS,
    DOUBLEAMPER,
    DOUBLEPIPE,
    DOUBLEEQUAL,
    DOUBLECOLON,

    // equal symbols
    PLUSEQUAL,
    MINUSEQUAL,
    STAREQUAL,
    SLASHEQUAL,
    BANGEQUAL,
    GREATEREQUAL,
    LESSEQUAL,
    PERCENTEQUAL,
    DOUBLELESSEQUAL,
    DOUBLEGREATEREQUAL,
    AMPEREQUAL,
    PIPEEQUAL,
    CARETEQUAL,

    IDENTIFIER,

    // quote literals
    CHARLIT,
    STRINGLIT,

    // integer literals
    DECINTLIT,
    OCTINTLIT,
    BININTLIT,
    HEXINTLIT,

    // other literals
    FLOATLIT,
    TRUELIT,
    FALSELIT,
    NULLLIT,

    // integer types
    UINT8,
    UINT16,
    UINT32,
    UINT64,
    SINT8,
    SINT16,
    SINT32,
    SINT64,

    // other types
    FLOAT,
    BOOL,
    DOUBLE,
    CHAR,

    // keywords
    VAR,
    FUN,
    VOID,
    NAMESPACE,
    CLASS,
    ENUM,
    RETURN,
    THIS,
    WHILE,
    FOR,
    IF,
    ELSE,
    SWITCH,
    CASE,
    DEFAULT,
    BREAK,
    BREAKALL,
    BREAKTO,
    CONST,
    CONTINUE,
    INLINE,
    VOLATILE,
    ASSERT,

    EOF_,
    ERROR,
    SOF // start of file to make sure parser does not report error on invalid token with garbage values/zeroes
};

inline std::string stringifyTokenType(TokenType ty)
{
#define STOKTY(type) case TokenType::type: return #type;
    switch (ty)
    {

        STOKTY(OPARN)
        STOKTY(CPARN)
        STOKTY(OSQUB)
        STOKTY(CSQUB)
        STOKTY(OCURB)
        STOKTY(CCURB)
        STOKTY(COMMA)
        STOKTY(PERIOD)
        STOKTY(SEMICOLON)
        STOKTY(QUESTION)
        STOKTY(COLON)
        STOKTY(BANG)
        STOKTY(PLUS)
        STOKTY(MINUS)
        STOKTY(STAR)
        STOKTY(SLASH)
        STOKTY(PERCENT)
        STOKTY(EQUAL)
        STOKTY(GREATER)
        STOKTY(LESS)
        STOKTY(TILDE)
        STOKTY(AMPER)
        STOKTY(PIPE)
        STOKTY(CARET)
        STOKTY(DOUBLEPLUS)
        STOKTY(DOUBLEMINUS)
        STOKTY(DOUBLEGREATER)
        STOKTY(DOUBLELESS)
        STOKTY(DOUBLEAMPER)
        STOKTY(DOUBLEPIPE)
        STOKTY(DOUBLEEQUAL)
        STOKTY(DOUBLECOLON)
        STOKTY(PLUSEQUAL)
        STOKTY(MINUSEQUAL)
        STOKTY(STAREQUAL)
        STOKTY(SLASHEQUAL)
        STOKTY(BANGEQUAL)
        STOKTY(GREATEREQUAL)
        STOKTY(LESSEQUAL)
        STOKTY(PERCENTEQUAL)
        STOKTY(DOUBLELESSEQUAL)
        STOKTY(DOUBLEGREATEREQUAL)
        STOKTY(AMPEREQUAL)
        STOKTY(PIPEEQUAL)
        STOKTY(CARETEQUAL)
        STOKTY(IDENTIFIER)
        STOKTY(CHARLIT)
        STOKTY(STRINGLIT)
        STOKTY(DECINTLIT)
        STOKTY(OCTINTLIT)
        STOKTY(BININTLIT)
        STOKTY(HEXINTLIT)
        STOKTY(FLOATLIT)
        STOKTY(TRUELIT)
        STOKTY(FALSELIT)
        STOKTY(NULLLIT)
        STOKTY(UINT8)
        STOKTY(UINT16)
        STOKTY(UINT32)
        STOKTY(UINT64)
        STOKTY(SINT8)
        STOKTY(SINT16)
        STOKTY(SINT32)
        STOKTY(SINT64)
        STOKTY(FLOAT)
        STOKTY(BOOL)
        STOKTY(DOUBLE)
        STOKTY(CHAR)
        STOKTY(VAR)
        STOKTY(FUN)
        STOKTY(VOID)
        STOKTY(NAMESPACE)
        STOKTY(CLASS)
        STOKTY(ENUM)
        STOKTY(RETURN)
        STOKTY(THIS)
        STOKTY(WHILE)
        STOKTY(FOR)
        STOKTY(IF)
        STOKTY(ELSE)
        STOKTY(SWITCH)
        STOKTY(CASE)
        STOKTY(DEFAULT)
        STOKTY(BREAK)
        STOKTY(BREAKALL)
        STOKTY(BREAKTO)
        STOKTY(CONST)
        STOKTY(CONTINUE)
        STOKTY(INLINE)
        STOKTY(VOLATILE)
        STOKTY(ASSERT)
        STOKTY(EOF_)
        STOKTY(ERROR)
        STOKTY(SOF)

        default:
            return "Unknown token type";
    }
#undef STOKTY
}
