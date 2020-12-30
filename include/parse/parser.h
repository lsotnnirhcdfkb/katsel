#pragma once

#include "lex/lexer.h"
#include "lex/token.h"
#include "ast/ast.h"
#include "utils/file.h"

#include <memory>
#include <string>

#include <map>

class Parser {
public:
    Parser(Lexer &l, File &sourcefile);

    std::unique_ptr<ASTNS::CUB> parse();

    Token consume();

    Lexer &lexer;
    File &sourcefile;

    bool errored;
};
