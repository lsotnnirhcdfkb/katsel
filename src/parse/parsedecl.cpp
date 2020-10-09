#include "parser.h"
#include "ast.h"

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

    assertConsume(TokenType::EOF_, "Expected EOF token at end of file (internal compiling error)");

    std::unique_ptr<ASTNS::Program> program = std::make_unique<ASTNS::Program>(programV);
    return program;
}
// parse decl {{{1
std::unique_ptr<ASTNS::Decl> Parser::decl()
{
    return functiondecl();
}
// function decls {{{1
std::unique_ptr<ASTNS::FunctionDecl> Parser::functiondecl()
{
    assertConsume(TokenType::FUN);

    std::unique_ptr<ASTNS::Type> rtype (std::move(type()));
    Token name = assertConsume(TokenType::IDENTIFIER, "Expected identifier for function name");

    assertConsume(TokenType::OPARN, "Expected opening parenthesis after function name");

    std::unique_ptr<ASTNS::Param> fparams;
    if (!check(TokenType::CPARN))
         fparams = std::move(params());

    assertConsume(TokenType::CPARN, "Expected closing parenthesis after parameters");

    std::unique_ptr<ASTNS::BlockStmt> fblock (std::move(block()));

    return std::make_unique<ASTNS::FunctionDecl>(std::move(rtype), name, std::move(fparams), std::move(fblock));
}
