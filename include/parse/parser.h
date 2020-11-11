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

    std::unique_ptr<ASTNS::NewBaseAST> parse();

private:
    Lexer &lexer;
    File &sourcefile;

    template <typename AST>
    size_t getGoto(size_t state);

    Token consume();
};
