#pragma once

#include "lexer.h"
#include "token.h"
#include "tokentype.h"
#include "errors.h"
#include "astnode.h"

class Parser
{
public:
    Parser(Lexer &l);

    ASTNode parse();
    ASTNode expression();
    ASTNode ternaryexpr();
    ASTNode binorexpr();
    ASTNode binandexpr();
    ASTNode binnotexpr();
    ASTNode compeqexpr();
    ASTNode complgtexpr();
    ASTNode bitxorexpr();
    ASTNode bitorexpr();
    ASTNode bitandexpr();
    ASTNode bitshiftexpr();
    ASTNode additionexpr();
    ASTNode multexpr();
    ASTNode unary();
    ASTNode primary();

private:
    Token prevToken;
    Token currToken;

    Lexer &lexer;

    Token& peek();
    Token& prev();
    Token& consume(TokenType type, std::string message, ASTNode &node);
    void advance();
    bool match(TokenType type);
    bool check(TokenType type);

    bool atEnd();

    void makeErrorNode(ASTNode &node, std::string message);
};
