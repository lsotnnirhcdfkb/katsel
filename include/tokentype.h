/// @file tokentype.h
/// The TokenType enum

#pragma once

/// An enum of all the different things a Token can represent in a source file
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
