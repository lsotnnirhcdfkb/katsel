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

    std::unique_ptr<ASTNS::AST> parse();

private:
    Lexer &lexer;
    File &sourcefile;

    class Action {};
    class ShiftAction {};
    class ReduceAction {};
    class AcceptAction {};

    Action getAction(size_t state, Token lookahead);
    size_t getGoto(size_t state, Token lookahead);

    Token consume();
};
