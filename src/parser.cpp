#include "parser.h"

// {{{ ASTNode methods
int ASTNode::numNodes()
{
    return nodes.size();
}

void ASTNode::addNode(ASTNode n)
{
    nodes.push_back(n);
}

void ASTNode::removeNode(int i)
{
    nodes.erase(nodes.begin() + i);
}

ASTNode& ASTNode::getNode(int i)
{
    return nodes[i];
}
// }}}

Parser::Parser(Lexer l): lexer(l) {}

// {{{ parser parsing methods
void Parser::expression() {
    
}
// }}}

// {{{ parser helper methods
// }}}
