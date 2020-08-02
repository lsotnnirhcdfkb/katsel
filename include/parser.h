#pragma once

#include <vector>
#include <string>

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

private:
    std::vector<ASTNode> nodes;
};

class Parser
{
public:
    Parser(Lexer l);

    ASTNode expression();
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
