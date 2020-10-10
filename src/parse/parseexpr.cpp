#include "parser.h"
#include "ast.h"

#include "errors.h"

#include <map>

// even though everything else is recursive descent,
// expressions are parsed with a Pratt parser, because I don't
// wnat to repeat the function call mess that was the first
// recursive descent only parser. it'll work either way
// as long as it returns a valid parse tree

// because of 'declaration reflects use' this delcaration i will totally
// forget the meaning of so here it is:
// declare a map with key type TokenType and value type of (function pointer
// to function returning unique ptr to Expr and accepting no arguments)
static std::map<TokenType, std::unique_ptr<ASTNS::Expr> (*)()> prefixParserTable =
{
    // {TokenType::EQUAL,
};

// declare a map with key type TokenType and value type of (function pointer
// to function returning unique ptr to Expr and accepting one argument that
// is of type unique ptr to Expr)
static std::map<TokenType, std::unique_ptr<ASTNS::Expr> (*)(std::unique_ptr<ASTNS::Expr>)> nonPrefixTable =
{
};

std::unique_ptr<ASTNS::UnaryExpr> Parser::prefixOp()
{
    Token op = prev();
    std::unique_ptr<ASTNS::Expr> operand = expr();
    return std::make_unique<ASTNS::UnaryExpr>(std::move(operand), op);
}

std::unique_ptr<ASTNS::BinaryExpr> Parser::binaryOp(std::unique_ptr<ASTNS::Expr> left)
{
    Token op = prev();
    std::unique_ptr<ASTNS::Expr> right = expr();
    return std::make_unique<ASTNS::BinaryExpr>(std::move(left), std::move(right), op);
}

std::unique_ptr<ASTNS::PrimaryExpr> Parser::primary()
{
    return std::make_unique<ASTNS::PrimaryExpr>(prev());
}

std::unique_ptr<ASTNS::Expr> Parser::expr()
{
    Token t = consume();

    std::unique_ptr<ASTNS::Expr> (*prefixParser)() = prefixParserTable[t.type];

    if (!prefixParser)
    {
        reportError(prev(), "Expected primary or unary operator", sourcefile);
        return nullptr;
    }

    std::unique_ptr<ASTNS::Expr> prefixParsed =  prefixParser();

    Token nonPrefixOp = peek();
    std::unique_ptr<ASTNS::Expr> (*nonPrefixParser)(std::unique_ptr<ASTNS::Expr>) = nonPrefixTable[nonPrefixOp.type];

    if (!nonPrefixParser)
        return prefixParsed;

    consume();

    return nonPrefixParser(std::move(prefixParsed));
}
