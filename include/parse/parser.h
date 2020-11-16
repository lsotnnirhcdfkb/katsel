#pragma once

#include "lex/lexer.h"
#include "lex/token.h"
#include "parse/ast.h"
#include "utils/file.h"
#include "message/errors.h"

#include <memory>
#include <string>

#include <map>

class Parser
{
public:
    Parser(Lexer &l, File &sourcefile);

    std::unique_ptr<ASTNS::DeclB> parse();

    Token consume();

    Error invalidSyntaxWhile(std::string justparsed, std::string expected, std::string whileparsing, Token const &lookahead, Token const &last);
    Error invalidSyntax(std::string justparsed, std::string expected, Token const &lookahead, Token const &last);
    Error invalidSyntaxNoExpect(std::string justparsed, std::string whileparsing, Token const &lookahead, Token const &last);

    Lexer &lexer;
    File &sourcefile;

private:
    std::vector<Token> errored;
};
