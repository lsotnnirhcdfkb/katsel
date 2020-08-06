#pragma once

#include <vector>
#include <string>
#include <iostream>
#include "token.h"

class ASTNode
{
public:
    Token op;
    Token op2;
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
