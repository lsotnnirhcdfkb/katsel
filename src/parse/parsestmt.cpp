#include "parser.h"
#include "ast.h"

// parent stmt method {{{1
std::unique_ptr<ASTNS::Stmt> Parser::stmt()
{
    std::unique_ptr<ASTNS::Stmt> statementast = nullptr;
    switch (peek().type)
    {
        case TokenType::VAR:
            statementast = varstmt();
            break;

        case TokenType::OCURB:
            statementast = blockstmt();
            break;

        case TokenType::RETURN:
            statementast = returnstmt();
            break;

        default:
            statementast = exprstmt();

    }
    return statementast;
}
// varstmt method {{{1
std::unique_ptr<ASTNS::VarStmt> Parser::varstmt()
{
    assertConsume(TokenType::VAR);
    std::unique_ptr<ASTNS::Type> typeast (std::move(type()));

    Token name = assertConsume(TokenType::IDENTIFIER, "Expected identifier for variable name");

    std::unique_ptr<ASTNS::Expr> expressionast = nullptr;
    if (checkConsume(TokenType::EQUAL))
    {
        expressionast = expr();
    }

    assertConsume(TokenType::SEMICOLON, "Expected semicolon after var statement");

    std::unique_ptr<ASTNS::VarStmt> stmtast = std::make_unique<ASTNS::VarStmt>(std::move(typeast), name, std::move(expressionast));
    return stmtast;
}
// exprstmt method {{{1
std::unique_ptr<ASTNS::ExprStmt> Parser::exprstmt()
{
    auto exprstmt (std::make_unique<ASTNS::ExprStmt>(std::move(expr())));
    assertConsume(TokenType::SEMICOLON, "Expected semicolon after expression statement");
    return exprstmt;
}
// returnstmt method {{{1
std::unique_ptr<ASTNS::ReturnStmt> Parser::returnstmt()
{
    assertConsume(TokenType::RETURN);
    return std::make_unique<ASTNS::ReturnStmt>(std::move(expr()));
}
// blockstmt method {{{1
std::unique_ptr<ASTNS::BlockStmt> Parser::blockstmt()
{
    std::vector<std::unique_ptr<ASTNS::Stmt>> blockv;
    assertConsume(TokenType::OCURB, "Expected opening curly bracket to open block");

    while (!check(TokenType::CCURB))
    {
        std::unique_ptr<ASTNS::Stmt> stmtast = stmt();

        if (stmtast && !ispanic) blockv.push_back(std::move(stmtast));

        if (ispanic)
        {
            unpanic();
            syncTokens();
        }
    }

    assertConsume(TokenType::CCURB, "Expected closing curly bracket to close block");

    std::unique_ptr<ASTNS::BlockStmt> blockast = std::make_unique<ASTNS::BlockStmt>(blockv);
    return blockast;
}
