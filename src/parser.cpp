#include "parser.h"

// {{{ ASTNode methods
ASTNode::ASTNode(Token op) : op(op) {}

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
    std::cout << std::string(indent, ' ') << "- Token: " << op.type << " \"" << std::string(op.start, op.end) << "\"" << std::endl;
    std::cout << std::string(indent, ' ') << "-   errored: " << errored << std::endl;
    std::cout << std::string(indent, ' ') << "-   errormsg: " << (errored ? errormsg : "None") << std::endl;

    for (ASTNode node : nodes)
    {
        node.print(indent + 4);
    }

}
// }}}

Parser::Parser(Lexer &l): lexer(l) {
    advance(); // get first token
}

// {{{ parser parsing methods
ASTNode Parser::expression()
{
    return primary();
}

ASTNode Parser::primary()
{
    if (match(TokenType::TRUELIT) || match(TokenType::FALSELIT) ||
        match(TokenType::FLOATLIT) ||
        match(TokenType::DECINTLIT) || match(TokenType::OCTINTLIT) || match(TokenType::BININTLIT) || match(TokenType::HEXINTLIT) ||
        match(TokenType::CHARLIT) || match(TokenType::STRINGLIT) ||
        match(TokenType::IDENTIFIER))
    {
        return ASTNode(prev());
    }

    if (match(TokenType::OPARN))
    {
        ASTNode expr = expression();
        consume(TokenType::CPARN, "Expect ')' after expression", expr);
        return expr;
    }

    ASTNode primary (peek());
    makeErrorNode(primary, "Expected expression");
    return primary;
}
// }}}

// {{{ parser helper methods
Token& Parser::peek()
{
    return currToken;
}

Token& Parser::prev()
{
    return prevToken;
}

Token& Parser::advance()
{
    prevToken = currToken;
    currToken = lexer.nextToken();

    return prev();
}

Token& Parser::consume(TokenType type, std::string message, ASTNode &node)
{
    if (match(type))
    {
        return prev();
    }

    makeErrorNode(node, message);
    return prev();
}

bool Parser::match(TokenType type)
{
    if (check(type))
    {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type)
{
    if (atEnd()) return false;
    return peek().type == type;
}

bool Parser::atEnd()
{
    return peek().type == TokenType::EOF_;
}

void Parser::makeErrorNode(ASTNode &node, std::string message)
{
    node.errored = true;
    node.errormsg = message;
}
// }}}
