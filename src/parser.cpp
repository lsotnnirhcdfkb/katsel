#include "parser.h"

// {{{ ASTNode methods
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
    std::cout << std::string(indent, ' ') << "  + errored: " << errored << std::endl;
    std::cout << std::string(indent, ' ') << "  + errormsg: " << (errored ? errormsg : "None") << std::endl;

    for (ASTNode &node : nodes)
    {
        node.print(indent + 8);
    }

}
// }}}

Parser::Parser(Lexer &l): lexer(l) {
    advance(); // get first token
}

// {{{ parser parsing methods
ASTNode Parser::parse()
{
    return expression();
}

ASTNode Parser::expression()
{
    return ternaryexpr();
}

ASTNode Parser::ternaryexpr()
{
    ASTNode node = binorexpr();

    if (match(TokenType::QUESTION))
    {
        ASTNode ternarynode (prev());

        ASTNode second = binorexpr();
        consume(TokenType::COLON, "Expect colon after first expression of ternary expression", second);
        ASTNode third = ternaryexpr();

        ternarynode.addNode(node);
        ternarynode.addNode(second);
        ternarynode.addNode(third);
        return ternarynode;
    }

    return node;
}

ASTNode Parser::binorexpr()
{
    ASTNode lnode = binandexpr();
    
    while (match(TokenType::DOUBLEPIPE))
    {
        Token op = prev();

        ASTNode rnode = binandexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::binandexpr()
{
    ASTNode lnode = binnotexpr();
    
    while (match(TokenType::DOUBLEAMPER))
    {
        Token op = prev();

        ASTNode rnode = binnotexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::binnotexpr()
{
    if (match(TokenType::BANG))
    {
        ASTNode node (prev());
        node.addNode(binnotexpr());

        return node;
    }

    return compeqexpr();
}

ASTNode Parser::compeqexpr()
{
    ASTNode lnode = complgtexpr();
    
    while (match(TokenType::DOUBLEEQUAL) || match(TokenType::BANGEQUAL))
    {
        Token op = prev();

        ASTNode rnode = complgtexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::complgtexpr()
{
    ASTNode lnode = bitxorexpr();
    
    while (match(TokenType::LESS) || match(TokenType::GREATER) || match(TokenType::LESSEQUAL) || match(TokenType::GREATEREQUAL))
    {
        Token op = prev();

        ASTNode rnode = bitxorexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::bitxorexpr()
{
    ASTNode lnode = bitorexpr();
    
    while (match(TokenType::CARET))
    {
        Token op = prev();

        ASTNode rnode = bitorexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::bitorexpr()
{
    ASTNode lnode = bitandexpr();
    
    while (match(TokenType::PIPE))
    {
        Token op = prev();

        ASTNode rnode = bitandexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::bitandexpr()
{
    ASTNode lnode = bitshiftexpr();
    
    while (match(TokenType::AMPER))
    {
        Token op = prev();

        ASTNode rnode = bitshiftexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::bitshiftexpr()
{
    ASTNode lnode = additionexpr();
    
    while (match(TokenType::DOUBLELESS) || match(TokenType::DOUBLEGREATER))
    {
        Token op = prev();

        ASTNode rnode = additionexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::additionexpr()
{
    ASTNode lnode = multexpr();

    while (match(TokenType::PLUS) || match(TokenType::MINUS))
    {
        Token op = prev();

        ASTNode rnode = multexpr();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::multexpr()
{
    ASTNode lnode = unary();

    while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT))
    {
        Token op = prev();

        ASTNode rnode = unary();

        ASTNode pnode (op);
        pnode.addNode(lnode);
        pnode.addNode(rnode);

        lnode = pnode;
    }

    return lnode;
}

ASTNode Parser::unary()
{
    if (match(TokenType::TILDE) || match(TokenType::MINUS))
    {
        Token op = prev();

        ASTNode node (op);
        node.addNode(unary());

        return node;
    }

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

    ASTNode primary (prev());
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

void Parser::advance()
{
    prevToken = currToken;

    while (true)
    {
        currToken = lexer.nextToken();

        if (currToken.type != TokenType::ERROR) break; // continue loop if it is an error token

        // if it is an error token then report error
        reportError("Error token " + std::string(currToken.start, currToken.end));
    }
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
