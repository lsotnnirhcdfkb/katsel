#pragma once

#include "lexer.h"
#include "token.h"
#include "tokentype.h"
#include "errors.h"
#include "ast.h"

#include <vector>

class Parser
{
public:
    Parser(Lexer &l, std::string &string);

    std::vector<std::unique_ptr<AST>> parse();
    std::unique_ptr<AST> statement();
    std::unique_ptr<AST> exprstmt();
    std::unique_ptr<AST> expression();
    std::unique_ptr<AST> ternaryexpr();
    std::unique_ptr<AST> binorexpr();
    std::unique_ptr<AST> binandexpr();
    std::unique_ptr<AST> binnotexpr();
    std::unique_ptr<AST> compeqexpr();
    std::unique_ptr<AST> complgtexpr();
    std::unique_ptr<AST> bitxorexpr();
    std::unique_ptr<AST> bitorexpr();
    std::unique_ptr<AST> bitandexpr();
    std::unique_ptr<AST> bitshiftexpr();
    std::unique_ptr<AST> additionexpr();
    std::unique_ptr<AST> multexpr();
    std::unique_ptr<AST> unary();
    std::unique_ptr<AST> primary();

private:
    Token prevToken;
    Token currToken;

    Lexer &lexer;

    std::string &source;

    Token& peek();
    Token& prev();
    Token& consume(TokenType type, std::string message);
    void advance();
    bool match(TokenType type);
    bool check(TokenType type);

    bool atEnd();
};
