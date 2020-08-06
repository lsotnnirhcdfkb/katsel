#include "astnode.h"

ASTNode::ASTNode(Token op) : op(op), errored(false), errormsg("") {}

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

void ASTNode::print()
{
    print(0);
}

void ASTNode::print(int indent)
{
    std::cout << std::string(indent, ' ') << "- " << op.type << " \"" << std::string(op.start, op.end) << "\"" << std::endl;
    std::cout << std::string(indent, ' ') << "  " << op2.type << " \"" << std::string(op2.start, op2.end) << "\"" << std::endl;
    std::cout << std::string(indent, ' ') << "  + errored: " << errored << std::endl;
    std::cout << std::string(indent, ' ') << "  + errormsg: " << (errored ? errormsg : "None") << std::endl;

    for (ASTNode &node : nodes)
    {
        node.print(indent + 8);
    }

}
