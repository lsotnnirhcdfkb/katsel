#include "parse/parser.h"
#include "parsestack.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile) {}

std::unique_ptr<ASTNS::DeclB> Parser::parse()
{
    std::unique_ptr<ASTNS::DeclB> ret (nullptr);

    std::vector<stackitem> stack;
    stack.emplace_back(0);

    _parse(*this, stack, false, ret, consume());
    return ret;
}

Token Parser::consume()
{
    Token cur;
    while (true)
    {
        cur = lexer.nextToken();
        if (cur.type != TokenType::ERROR) return cur;

        cur.errf();
    }

    return cur;
}

Error Parser::invalidSyntaxWhile(std::string justparsed, std::string expected, std::string whileparsing, Token const &lookahead, Token const &last)
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

    return e;
}
Error Parser::invalidSyntax(std::string justparsed, std::string expected, Token const &lookahead, Token const &last)
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

    return e;
}
Error Parser::invalidSyntaxNoExpect(std::string justparsed, std::string whileparsing, Token const &lookahead, Token const &last)
{
    std::stringstream ssl;
    ssl << "invalid token to follow " << justparsed << " of " << whileparsing << ": " << stringifyTokenType(lookahead.type);
    Error e = Error(Error::MsgType::ERROR, lookahead, ssl.str())
        .underline(Error::Underline(last, '^')
            .error(concatMsg(stringifyTokenType(lookahead.type), " cannot follow ", justparsed, " of ", whileparsing)))
        .underline(Error::Underline(lookahead, '~')
            .note("invalid token here"));

    return e;
}
