#include "parse/parser.h"
#include "parse/ast.h"

#include "message/errors.h"
#include "message/fmtmessage.h"

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
    report(MsgType::ERROR, msg::expectedType(), prev(), prev());
    return nullptr;
}


std::unique_ptr<ASTNS::Param> Parser::params()
{
    std::unique_ptr<ASTNS::Type> firstPType (type());
    assertConsume(TokenType::IDENTIFIER, "Expected parameter name");
    Token firstPName (prev());

    std::unique_ptr<ASTNS::Param> firstParam = std::make_unique<ASTNS::Param>(std::move(firstPType), firstPName, nullptr);
    ASTNS::Param *lastParam = firstParam.get();

    while (!check(TokenType::CPARN))
    {
        assertConsume(TokenType::COMMA, "Expected comma as delimieter in parameter list");

        std::unique_ptr<ASTNS::Type> cparamty (type());
        assertConsume(TokenType::IDENTIFIER, "Expected parameter name");
        Token cparamName (prev());

        std::unique_ptr<ASTNS::Param> cparam = std::make_unique<ASTNS::Param>(std::move(cparamty), cparamName, nullptr);
        lastParam->next = std::move(cparam);
        lastParam = lastParam->next.get();
    }

    return firstParam;
}

std::unique_ptr<ASTNS::Arg> Parser::args()
{
    std::unique_ptr<ASTNS::Arg> firstArg = std::make_unique<ASTNS::Arg>(expr(), nullptr);
    ASTNS::Arg *lastArg = firstArg.get();

    while (!check(TokenType::CPARN))
    {
        assertConsume(TokenType::COMMA, "Expected comma as delimieter in argument list");

        std::unique_ptr<ASTNS::Arg> carg = std::make_unique<ASTNS::Arg>(expr(), nullptr);
        lastArg->next = std::move(carg);
        lastArg = lastArg->next.get();
    }

    return firstArg;
}
