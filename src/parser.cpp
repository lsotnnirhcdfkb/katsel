#include "parser.h"

Parser::Parser(Lexer &l, std::string &source): lexer(l), source(source) {
    advance(); // get first token
}

// {{{ parser parsing methods
std::vector<std::unique_ptr<AST>> Parser::parse()
{
    std::vector<std::unique_ptr<AST>> program;

    while (!atEnd()) // if there is no expression
    {
        program.push_back(statement());
    }

    consume(TokenType::EOF_, "Expected EOF token at end of file (internal compiling error)");

    return program;
}

std::unique_ptr<AST> Parser::statement()
{
    std::unique_ptr<AST> exprstmtast = exprstmt();
    consume(TokenType::SEMICOLON, "Expected ';' after statement");
    return exprstmtast;
}

std::unique_ptr<AST> Parser::exprstmt()
{
    std::unique_ptr<AST> expr = expression();
    return std::make_unique<ExprStmtAST>(expr);
}

std::unique_ptr<AST> Parser::expression()
{
    return ternaryexpr();
}

std::unique_ptr<AST> Parser::ternaryexpr()
{
    std::unique_ptr<AST> binexpr = binorexpr();

    if (match(TokenType::QUESTION))
    {
        std::unique_ptr<AST> trueexpr = binorexpr();
        consume(TokenType::COLON, "Expect colon after first expression");
        std::unique_ptr<AST> falseexpr = ternaryexpr();

        std::unique_ptr<TernaryOpAST> ternast = std::make_unique<TernaryOpAST>(binexpr, trueexpr, falseexpr);

        return ternast;
    }

    return binexpr;
}

std::unique_ptr<AST> Parser::binorexpr()
{
    std::unique_ptr<AST> lnode = binandexpr();
    
    while (match(TokenType::DOUBLEPIPE))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = binandexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::binandexpr()
{
    std::unique_ptr<AST> lnode = binnotexpr();
    
    while (match(TokenType::DOUBLEAMPER))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = binnotexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::binnotexpr()
{
    if (match(TokenType::BANG))
    {
        std::unique_ptr<AST> binnot = binnotexpr();
        std::unique_ptr<UnaryAST> node = std::make_unique<UnaryAST>(prev(), binnot);
        return node;
    }

    return compeqexpr();
}

std::unique_ptr<AST> Parser::compeqexpr()
{
    std::unique_ptr<AST> lnode = complgtexpr();
    
    while (match(TokenType::DOUBLEEQUAL) || match(TokenType::BANGEQUAL))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = complgtexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::complgtexpr()
{
    std::unique_ptr<AST> lnode = bitxorexpr();
    
    while (match(TokenType::LESS) || match(TokenType::GREATER) || match(TokenType::LESSEQUAL) || match(TokenType::GREATEREQUAL))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitxorexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitxorexpr()
{
    std::unique_ptr<AST> lnode = bitorexpr();
    
    while (match(TokenType::CARET))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitorexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitorexpr()
{
    std::unique_ptr<AST> lnode = bitandexpr();
    
    while (match(TokenType::PIPE))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitandexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitandexpr()
{
    std::unique_ptr<AST> lnode = bitshiftexpr();
    
    while (match(TokenType::AMPER))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitshiftexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitshiftexpr()
{
    std::unique_ptr<AST> lnode = additionexpr();
    
    while (match(TokenType::DOUBLELESS) || match(TokenType::DOUBLEGREATER))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = additionexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::additionexpr()
{
    std::unique_ptr<AST> lnode = multexpr();

    while (match(TokenType::PLUS) || match(TokenType::MINUS))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = multexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::multexpr()
{
    std::unique_ptr<AST> lnode = unary();

    while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT))
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = unary();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, lnode, rnode);

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::unary()
{
    if (match(TokenType::TILDE) || match(TokenType::MINUS))
    {
        Token op = prev();
        std::unique_ptr<AST> unaryno = unary();

        std::unique_ptr<UnaryAST> node = std::make_unique<UnaryAST>(op, unaryno);

        return node;
    }

    return primary();
}

std::unique_ptr<AST> Parser::primary()
{
    if (match(TokenType::TRUELIT) || match(TokenType::FALSELIT) ||
        match(TokenType::FLOATLIT) ||
        match(TokenType::DECINTLIT) || match(TokenType::OCTINTLIT) || match(TokenType::BININTLIT) || match(TokenType::HEXINTLIT) ||
        match(TokenType::CHARLIT) || match(TokenType::STRINGLIT) ||
        match(TokenType::IDENTIFIER))
    {
        return std::make_unique<PrimaryAST>(prev());
    }

    if (match(TokenType::OPARN))
    {
        std::unique_ptr<AST> expr = expression();
        consume(TokenType::CPARN, "Expect ')' after expression");
        return expr;
    }

    reportError(peek(), "Expected expression", source);
    return nullptr;
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
        reportError(currToken, currToken.message, source);
    }
}

Token& Parser::consume(TokenType type, std::string message)
{
    if (match(type))
    {
        return prev();
    }

    reportError(peek(), message, source);
    return peek();
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
    return peek().type == type;
}

bool Parser::atEnd()
{
    return peek().type == TokenType::EOF_;
}
// }}}
