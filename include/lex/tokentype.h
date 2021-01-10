#pragma once

#include <string>

enum class TokenType {
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
    DOLLAR,
    HASH,

    RIGHTARROW,
    LEFTARROW,

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

    // more keywords
    THIS,
    VAR,
    FUN,
    LET,
    MUT,
    CLASS,
    DATA,
    IMPL,
    RETURN,
    WHILE,
    FOR,
    IF,
    ELSE,
    MATCH,
    BREAK,
    CONTINUE,
    ASSERT,

    BOOM,

    NEWLINE,
    INDENT,
    DEDENT,

    EOF_,
    ERROR,
    SOF // start of file to make sure parser does not report error on invalid token with garbage values/zeroes
};

inline constexpr char const * stringifyTokenType(TokenType ty) {
#define STOKTY(type, str) case TokenType::type: return #str;
    switch (ty) {
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
        STOKTY(HASH, '#')
        STOKTY(DOLLAR, '$')
        STOKTY(LEFTARROW, '<-')
        STOKTY(RIGHTARROW, '->')
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
        STOKTY(HEXINTLIT, hex integer literal)
        STOKTY(FLOATLIT, floating point literal)
        STOKTY(TRUELIT, 'true')
        STOKTY(FALSELIT, 'false')
        STOKTY(NULLPTRLIT, 'nullptr')
        STOKTY(THIS, 'this')
        STOKTY(VAR, 'var')
        STOKTY(LET, 'let')
        STOKTY(MUT, 'mut')
        STOKTY(FUN, 'fun')
        STOKTY(CLASS, 'class')
        STOKTY(DATA, 'data')
        STOKTY(IMPL, 'impl')
        STOKTY(RETURN, 'return')
        STOKTY(WHILE, 'while')
        STOKTY(FOR, 'for')
        STOKTY(IF, 'if')
        STOKTY(ELSE, 'else')
        STOKTY(MATCH, 'match')
        STOKTY(BREAK, 'break')
        STOKTY(CONTINUE, 'continue')
        STOKTY(ASSERT, 'assert')
        STOKTY(BOOM, boom)
        STOKTY(NEWLINE, newline)
        STOKTY(INDENT, indent)
        STOKTY(DEDENT, dedent)
        STOKTY(EOF_, end of file)
        STOKTY(ERROR, error token)
        STOKTY(SOF, start of file)
    }
#undef STOKTY
}
