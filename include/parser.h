#pragma once

#include "lexer.h"
#include "token.h"
#include "ast.h"
#include "file.h"

#include <memory>

class Parser
{
public:
    Parser(Lexer &l, File &sourcefile);

    std::unique_ptr<ASTNS::Program> parse();

private:
    Lexer &lexer;
    File &sourcefile;

    void advance();

    Token prevToken;
    Token currToken;

    Token& peek();
    Token& prev();
};
