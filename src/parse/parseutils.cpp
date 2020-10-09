#include "parser.h"
#include "ast.h"

#include "errors.h"

std::unique_ptr<ASTNS::Type> Parser::type()
{
    // only builtin types for now
    if (checkConsume(TokenType::UINT8) || checkConsume(TokenType::UINT16) || checkConsume(TokenType::UINT32) || checkConsume(TokenType::UINT64) ||
        checkConsume(TokenType::SINT8) || checkConsume(TokenType::SINT16) || checkConsume(TokenType::SINT32) || checkConsume(TokenType::SINT64) ||

        checkConsume(TokenType::FLOAT) ||
        checkConsume(TokenType::CHAR) ||
        checkConsume(TokenType::BOOL) ||
        checkConsume(TokenType::DOUBLE) ||
        checkConsume(TokenType::VOID))
    {
        return std::make_unique<ASTNS::BaseType>(prev());
    }

    consume();
    reportError(prev(), "Expected type", sourcefile);
    return nullptr;
}

std::unique_ptr<ASTNS::BlockStmt> Parser::block()
{

}

std::unique_ptr<ASTNS::Param> Parser::params()
{
    std::unique_ptr<ASTNS::Type> firstPType (std::move(type()));
    Token firstPName (assertConsume(TokenType::IDENTIFIER, "Expected parameter name"));

    std::unique_ptr<ASTNS::Param> firstParam = std::make_unique<ASTNS::Param>(std::move(firstPType), firstPName, nullptr);
    ASTNS::Param *lastParam = firstParam.get();

    while (!check(TokenType::CPARN))
    {
        assertConsume(TokenType::COMMA, "Expected comma as delimieter");

        std::unique_ptr<ASTNS::Type> cparamty (std::move(type()));
        Token cparamName (assertConsume(TokenType::IDENTIFIER, "Expected parameter name"));

        std::unique_ptr<ASTNS::Param> cparam = std::make_unique<ASTNS::Param>(std::move(cparamty), cparamName, nullptr);
        lastParam->next = std::move(cparam);
        lastParam = lastParam->next.get();
    }

    return firstParam;
}

std::unique_ptr<ASTNS::Arg> Parser::args()
{

}
