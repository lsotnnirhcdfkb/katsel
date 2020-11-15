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

    std::unique_ptr<ASTNS::Decls> parse();

    Token consume();

private:
    Lexer &lexer;
    File &sourcefile;

    template <typename AST>
    size_t getGoto(size_t state);

    Error invalidSyntaxWhile(const char *justparsed, const char *expected, const char *whileparsing, Token const &lookahead, Token const &last);
    Error invalidSyntax(const char *justparsed, const char *expected, Token const &lookahead, Token const &last);
    Error invalidSyntaxNoExpect(const char *justparsed, const char *whileparsing, Token const &lookahead, Token const &last);

    std::vector<Token> errored;
};
