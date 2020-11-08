#include "parse/parser.h"
#include "parse/ast.h"
#include "message/errors.h"

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
    std::unique_ptr<ASTNS::Type> typeast (type());

    std::vector<std::unique_ptr<ASTNS::Expr>> assignments;
    if (checkConsume(TokenType::SEMICOLON))
    {
        Error(Error::MsgType::ERROR, prev(), "Expected at least 1 assignment expression")
            .primary(Error::Primary(peek())
                .error("Expected at least 1 assignment expression"))
            .report();
        panic();
        return nullptr;
    }

    while (!check(TokenType::SEMICOLON))
    {
        assertConsume(TokenType::IDENTIFIER, "Expected identifier for variable name");
        Token name (prev());
        if (checkConsume(TokenType::EQUAL))
        {
            std::unique_ptr<ASTNS::Expr> assignment = binaryOp(std::make_unique<ASTNS::PrimaryExpr>(name));
            ASTNS::BinaryExpr *asbinary = dynamic_cast<ASTNS::BinaryExpr*>(assignment.get());
            if (!asbinary || asbinary->op.type != TokenType::EQUAL)
            {
                Error(Error::MsgType::ERROR, assignment.get(), "Expected assignment expression")
                    .primary(Error::Primary(assignment.get())
                        .error("Expected assignment expression"))
                    .report();
                panic();
                return nullptr;
            }

            assignments.push_back(std::move(assignment));
        }
        else
            assignments.push_back(std::make_unique<ASTNS::PrimaryExpr>(name));

        if (!check(TokenType::SEMICOLON))
        {
            assertConsume(TokenType::COMMA, "Expected comma as delimeter between variables");
            panic();
            return nullptr;
        }
    }

    assertConsume(TokenType::SEMICOLON, "Expected semicolon after var statement");

    std::unique_ptr<ASTNS::VarStmt> stmtast = std::make_unique<ASTNS::VarStmt>(std::move(typeast), assignments);
    return stmtast;
}
// exprstmt method {{{1
std::unique_ptr<ASTNS::ExprStmt> Parser::exprstmt()
{
    auto exprstmt (std::make_unique<ASTNS::ExprStmt>(expr()));
    assertConsume(TokenType::SEMICOLON, "Expected semicolon after expression statement");
    return exprstmt;
}
// returnstmt method {{{1
std::unique_ptr<ASTNS::ReturnStmt> Parser::returnstmt()
{
    assertConsume(TokenType::RETURN);
    std::unique_ptr<ASTNS::Expr> retVal;
    if (!check(TokenType::SEMICOLON))
        retVal = expr();

    assertConsume(TokenType::SEMICOLON, "Expected semicolon after return statement");
    return std::make_unique<ASTNS::ReturnStmt>(std::move(retVal));
}
// blockstmt method {{{1
std::unique_ptr<ASTNS::BlockStmt> Parser::blockstmt()
{
    std::vector<std::unique_ptr<ASTNS::Stmt>> blockv;
    assertConsume(TokenType::OCURB, "Expected opening curly bracket to open block");

    while (!check(TokenType::CCURB) && !atEnd())
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
