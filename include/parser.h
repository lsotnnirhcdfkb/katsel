#pragma once

#include <vector>
#include <string>

#include <iostream>

#include "lexer.h"
#include "errors.h"

class ASTNode
{
public:
    Token op;
    bool errored;
    std::string errormsg;

    ASTNode(Token op);

    int numNodes();
    
    void addNode(ASTNode n);
    void removeNode(int i);

    ASTNode& getNode(int i);

    void print();

private:
    std::vector<ASTNode> nodes;

    void print(int indent);
};

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
