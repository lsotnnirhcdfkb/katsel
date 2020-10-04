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

    Token prevToken;
    Token currToken;

    Token& peek();
    Token& prev();

    void advance();
    bool checkConsume(TokenType type);
    bool check(TokenType type);
    bool atEnd();

    bool ispanic;
    void panic();
    void unpanic();
    void syncTokens();
};
