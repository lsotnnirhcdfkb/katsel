#include "parse/parser.h"
#include "parse/ast.h"

#include "message/errors.h"
// parse method {{{1
std::unique_ptr<ASTNS::Program> Parser::parse()
{
    std::vector<std::unique_ptr<ASTNS::Decl>> programV;

    while (!atEnd())
    {
        std::unique_ptr<ASTNS::Decl> declast = decl();

        // if panicing then this ast
        // has an error and something
        // could be nullptr or the ast
        // is malformed so dont add it
        if (declast && !ispanic) programV.push_back(std::move(declast));

        if (ispanic)
        {
            unpanic();
            syncTokens();
        }
    }

    assertConsume(TokenType::EOF_, Error::makeBasicErr(peek(), "Expected EOF Token"));

    std::unique_ptr<ASTNS::Program> program = std::make_unique<ASTNS::Program>(programV);
    return program;
}
// parse decl {{{1
std::unique_ptr<ASTNS::Decl> Parser::decl()
{
    switch (peek().type)
    {
        case TokenType::FUN:
            return functiondecl();

        default:
            Error(Error::MsgType::ERROR, peek(), "Expected declaration")
                .primary(Error::Primary(peek())
                    .error("Expected declaration"))
                .report();
            panic();
            return nullptr;
    }
}
// function decls {{{1
std::unique_ptr<ASTNS::FunctionDecl> Parser::functiondecl()
{
    assertConsume(TokenType::FUN);

    std::unique_ptr<ASTNS::Type> rtype (type());
    assertConsume(TokenType::IDENTIFIER, Error::makeBasicErr(peek(), "Expected identifier for function name"));
    Token name (prev());

    assertConsume(TokenType::OPARN, Error::makeBasicErr(peek(), "Expected opening parenthesis after function name"));

    std::unique_ptr<ASTNS::Param> fparams;
    if (!check(TokenType::CPARN))
         fparams = params();

    assertConsume(TokenType::CPARN, Error::makeBasicErr(peek(), "Expected closing parenthesis after parameters"));

    std::unique_ptr<ASTNS::BlockStmt> fblock (blockstmt());

    return std::make_unique<ASTNS::FunctionDecl>(std::move(rtype), name, std::move(fparams), std::move(fblock));
}