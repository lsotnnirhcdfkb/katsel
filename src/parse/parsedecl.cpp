#include "parser.h"
#include "ast.h"

// parse method {{{1
std::unique_ptr<ASTNS::Program> Parser::parse()
{
    std::vector<std::unique_ptr<ASTNS::Decl>> programV;

    while (!atEnd()) // if there is no expression
    {
        if (ispanic)
        {
            unpanic();
            syncTokens();
        }

        if (atEnd()) // if syncTokens reached the end
            break;

        std::unique_ptr<ASTNS::Decl> declast = decl();

        // if panicing then this ast
        // has an error and something
        // could be nullptr or the ast
        // is malformed so dont add it
        // if (declast && !ispanic) programV.push_back(std::move(declast));
    }

    assertConsume(TokenType::EOF_, "Expected EOF token at end of file (internal compiling error)");

    std::unique_ptr<ASTNS::Program> program = std::make_unique<ASTNS::Program>(programV);
    return program;
}
// function decls {{{1
std::unique_ptr<ASTNS::FunctionDecl> Parser::functiondecl()
{
}
