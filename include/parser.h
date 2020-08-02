#pragma once

#include <vector>
#include <string>

#include <iostream>

#include "lexer.h"

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
    ASTNode addition();
    ASTNode multiplication();
    ASTNode primary();

private:
    Token prevToken;
    Token currToken;

    Lexer &lexer;

    Token& peek();
    Token& prev();
    Token& advance();
    Token& consume(TokenType type, std::string message, ASTNode &node);
    bool match(TokenType type);
    bool check(TokenType type);

    bool atEnd();

    void makeErrorNode(ASTNode &node, std::string message);
};
