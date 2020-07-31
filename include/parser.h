#pragma once

#include <vector>
#include "lexer.h"

class ASTNode
{
public:
    Token op;

    int numNodes();
    
    void addNode(ASTNode n);
    void removeNode(int i);

    ASTNode& getNode(int i);

private:
    std::vector<ASTNode> nodes;
};

class Parser
{
    Token current;
    Token next;
};
