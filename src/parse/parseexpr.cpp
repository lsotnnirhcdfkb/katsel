#include "parser.h"
#include "ast.h"

#include "errors.h"

// even though everything else is recursive descent,
// expressions are parsed with a Pratt parser, because I don't
// wnat to repeat the function call mess that was the first
// recursive descent only parser. it'll work either way
// as long as it returns a valid parse tree

const std::map<TokenType, Parser::PrefixPF> Parser::prefixParserTable = {
    {TokenType::BANG,  &Parser::prefixOp},
    {TokenType::TILDE, &Parser::prefixOp},
    {TokenType::MINUS, &Parser::prefixOp},

    {TokenType::TRUELIT,       &Parser::primary},
    {TokenType::FALSELIT,      &Parser::primary},
    {TokenType::FLOATLIT,      &Parser::primary},
    {TokenType::NULLLIT,       &Parser::primary},
    {TokenType::DECINTLIT,     &Parser::primary},
    {TokenType::OCTINTLIT,     &Parser::primary},
    {TokenType::BININTLIT,     &Parser::primary},
    {TokenType::HEXINTLIT,     &Parser::primary},
    {TokenType::CHARLIT,       &Parser::primary},
    {TokenType::STRINGLIT,     &Parser::primary},
    // {TokenType::OPARN,      &Parser::parenExpr}, // TODO: make parentheses rule
};

const std::map<TokenType, Parser::NonPrefixPF> Parser::nonPrefixTable = {
    {TokenType::EQUAL,         &Parser::binaryOp}, // TODO: right associative
    // {TokenType::QUESTION,      &Parser::ternaryOp}, // TODO: make this function
    {TokenType::DOUBLEPIPE,    &Parser::binaryOp},
    {TokenType::DOUBLEAMPER,   &Parser::binaryOp},
    {TokenType::DOUBLEEQUAL,   &Parser::binaryOp},
    {TokenType::BANGEQUAL,     &Parser::binaryOp},
    {TokenType::LESS,          &Parser::binaryOp},
    {TokenType::GREATER,       &Parser::binaryOp},
    {TokenType::LESSEQUAL,     &Parser::binaryOp},
    {TokenType::GREATEREQUAL,  &Parser::binaryOp},
    {TokenType::CARET,         &Parser::binaryOp},
    {TokenType::PIPE,          &Parser::binaryOp},
    {TokenType::AMPER,         &Parser::binaryOp},
    {TokenType::DOUBLEGREATER, &Parser::binaryOp},
    {TokenType::DOUBLELESS,    &Parser::binaryOp},
    {TokenType::PLUS,          &Parser::binaryOp},
    {TokenType::MINUS,         &Parser::binaryOp},
    {TokenType::STAR,          &Parser::binaryOp},
    {TokenType::SLASH,         &Parser::binaryOp},
    {TokenType::PERCENT,       &Parser::binaryOp},
};

std::unique_ptr<ASTNS::Expr> Parser::prefixOp()
{
    Token op = prev();
    std::unique_ptr<ASTNS::Expr> operand = expr();
    return std::make_unique<ASTNS::UnaryExpr>(std::move(operand), op);
}

std::unique_ptr<ASTNS::Expr> Parser::binaryOp(std::unique_ptr<ASTNS::Expr> left)
{
    Token op = prev();
    std::unique_ptr<ASTNS::Expr> right = expr();
    return std::make_unique<ASTNS::BinaryExpr>(std::move(left), std::move(right), op);
}

std::unique_ptr<ASTNS::Expr> Parser::primary()
{
    return std::make_unique<ASTNS::PrimaryExpr>(prev());
}

std::unique_ptr<ASTNS::Expr> Parser::expr()
{
    Token t = consume();

    auto prefixParser = prefixParserTable.find(t.type);

    if (prefixParser == prefixParserTable.end())
    {
        reportError(prev(), "Expected primary or unary operator", sourcefile);
        return nullptr;
    }

    std::unique_ptr<ASTNS::Expr> prefixParsed = (this->*(prefixParser->second))();

    Token nonPrefixOp = peek();
    auto nonPrefixParser = nonPrefixTable.find(t.type);

    if (nonPrefixParser == nonPrefixTable.end())
        return prefixParsed;

    consume();

    return (this->*(nonPrefixParser->second))(std::move(prefixParsed));
}
