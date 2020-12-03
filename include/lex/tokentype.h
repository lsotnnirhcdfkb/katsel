#pragma once

#include <string>

enum class TokenType
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
    NULLPTRLIT,

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
    WHILE,
    FOR,
    IF,
    ELSE,
    PATTERN,
    DEFAULT,
    BREAK,
    BREAKALL,
    BREAKTO,
    CONTINUE,
    ASSERT,

    EOF_,
    ERROR,
    SOF // start of file to make sure parser does not report error on invalid token with garbage values/zeroes
};

inline constexpr char const * stringifyTokenType(TokenType ty)
{
#define STOKTY(type, str) case TokenType::type: return #str;
    switch (ty)
    {

        STOKTY(OPARN, '(')
        STOKTY(CPARN, ')')
        STOKTY(OSQUB, '[')
        STOKTY(CSQUB, ']')
        STOKTY(OCURB, '{')
        STOKTY(CCURB, '}')
        STOKTY(COMMA, ',')
        STOKTY(PERIOD, '.')
        STOKTY(SEMICOLON, ';')
        STOKTY(QUESTION, '?')
        STOKTY(COLON, ':')
        STOKTY(BANG, '!')
        STOKTY(PLUS, '+')
        STOKTY(MINUS, '-')
        STOKTY(STAR, '*')
        STOKTY(SLASH, '/')
        STOKTY(PERCENT, '%')
        STOKTY(EQUAL, '=')
        STOKTY(GREATER, '>')
        STOKTY(LESS, '<')
        STOKTY(TILDE, '~')
        STOKTY(AMPER, '&')
        STOKTY(PIPE, '|')
        STOKTY(CARET, '^')
        STOKTY(DOUBLEPLUS, '++')
        STOKTY(DOUBLEMINUS, '--')
        STOKTY(DOUBLEGREATER, '>>')
        STOKTY(DOUBLELESS, '<<')
        STOKTY(DOUBLEAMPER, '&&')
        STOKTY(DOUBLEPIPE, '||')
        STOKTY(DOUBLEEQUAL, '==')
        STOKTY(DOUBLECOLON, '::')
        STOKTY(PLUSEQUAL, '+=')
        STOKTY(MINUSEQUAL, '-=')
        STOKTY(STAREQUAL, '*=')
        STOKTY(SLASHEQUAL, '/=')
        STOKTY(BANGEQUAL, '!=')
        STOKTY(GREATEREQUAL, '>=')
        STOKTY(LESSEQUAL, '<=')
        STOKTY(PERCENTEQUAL, '%=')
        STOKTY(DOUBLELESSEQUAL, '<<=')
        STOKTY(DOUBLEGREATEREQUAL, '>>=')
        STOKTY(AMPEREQUAL, '&=')
        STOKTY(PIPEEQUAL, '|=')
        STOKTY(CARETEQUAL, '^=')
        STOKTY(IDENTIFIER, identifier)
        STOKTY(CHARLIT, character literal)
        STOKTY(STRINGLIT, string literal)
        STOKTY(DECINTLIT, decimal integer literal)
        STOKTY(OCTINTLIT, octal integer literal)
        STOKTY(BININTLIT, binary integer literal)
        STOKTY(HEXINTLIT, hexadecimal integer literal)
        STOKTY(FLOATLIT, floating point literal)
        STOKTY(TRUELIT, 'true')
        STOKTY(FALSELIT, 'false')
        STOKTY(NULLPTRLIT, 'nullptr')
        STOKTY(UINT8, 'uint8')
        STOKTY(UINT16, 'uint16')
        STOKTY(UINT32, 'uint32')
        STOKTY(UINT64, 'uint64')
        STOKTY(SINT8, 'sint8')
        STOKTY(SINT16, 'sint16')
        STOKTY(SINT32, 'sint32')
        STOKTY(SINT64, 'sint64')
        STOKTY(FLOAT, 'float')
        STOKTY(BOOL, 'bool')
        STOKTY(DOUBLE, 'double')
        STOKTY(CHAR, 'char')
        STOKTY(VAR, 'var')
        STOKTY(FUN, 'fun')
        STOKTY(VOID, 'void')
        STOKTY(NAMESPACE, 'namespace')
        STOKTY(CLASS, 'class')
        STOKTY(ENUM, 'enum')
        STOKTY(RETURN, 'return')
        STOKTY(WHILE, 'while')
        STOKTY(FOR, 'for')
        STOKTY(IF, 'if')
        STOKTY(ELSE, 'else')
        STOKTY(PATTERN, 'pattern')
        STOKTY(DEFAULT, 'default')
        STOKTY(BREAK, 'break')
        STOKTY(BREAKALL, 'breakall')
        STOKTY(BREAKTO, 'breakto')
        STOKTY(CONTINUE, 'continue')
        STOKTY(ASSERT, 'assert')
        STOKTY(EOF_, end of file)
        STOKTY(ERROR, error token)
        STOKTY(SOF, start of file)

        default:
            return "unknown token type";
    }
#undef STOKTY
}
