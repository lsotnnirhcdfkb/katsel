#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile) {}

Token Parser::consume()
{
    Token cur;
    errored.clear();
    while (true)
    {
        cur = lexer.nextToken();
        if (cur.type != TokenType::ERROR) return cur;

        Error(Error::MsgType::ERROR, cur, cur.message)
            .underline(Error::Underline(cur, '^')
                .error(cur.message)
                .note("erroneous tokens are ignored"))
            .report();
        errored.push_back(cur);
    }

    return cur;
}

void Parser::invalidSyntaxWhile(const char *justparsed, const char *expected, const char *whileparsing, Token const &lookahead, Token const &last)
{
    std::stringstream ssl;
    std::stringstream sss;
    ssl << "expected " << expected << " after " << justparsed << " of " << whileparsing << ", but got " << stringifyTokenType(lookahead.type) << " instead";
    sss << "expected " << expected;
    Error e = Error(Error::MsgType::ERROR, lookahead, ssl.str())
        .underline(Error::Underline(last, '^')
            .error(sss.str()))
        .underline(Error::Underline(lookahead, '~')
            .note("unexpected token here"));

    for (Token const &t : errored)
        e.underline(Error::Underline(t, '-')
            .note(concatMsg("erroneous token ignored here with error message \"", t.message, "\"")));

    e.report();
}
void Parser::invalidSyntax(const char *justparsed, const char *expected, Token const &lookahead, Token const &last)
{
    std::stringstream ssl;
    std::stringstream sss;
    ssl << "expected " << expected << " after " << justparsed << ", but got " << stringifyTokenType(lookahead.type) << " instead";
    sss << "expected " << expected;
    Error e = Error(Error::MsgType::ERROR, lookahead, ssl.str())
        .underline(Error::Underline(last, '^')
            .error(sss.str()))
        .underline(Error::Underline(lookahead, '~')
            .note("unexpected token here"));

    for (Token const &t : errored)
        e.underline(Error::Underline(t, '-')
            .note(concatMsg("erroneous token ignored here with error message \"", t.message, "\"")));
    e.report();
}
void Parser::invalidSyntaxNoExpect(const char *justparsed, const char *whileparsing, Token const &lookahead, Token const &last)
{
    std::stringstream ssl;
    ssl << "invalid token to follow " << justparsed << " of " << whileparsing << ": " << stringifyTokenType(lookahead.type);
    Error e = Error(Error::MsgType::ERROR, lookahead, ssl.str())
        .underline(Error::Underline(last, '^'))
        .underline(Error::Underline(lookahead, '~')
            .note("invalid token here"));

    for (Token const &t : errored)
        e.underline(Error::Underline(t, '-')
            .note(concatMsg("erroneous token ignored here with error message \"", t.message, "\"")));

    e.report();
}
