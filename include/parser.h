#pragma once

#include "lexer.h"
#include "token.h"
#include "ast.h"
#include "file.h"

#include <memory>
#include <string>

class Parser
{
public:
    Parser(Lexer &l, File &sourcefile);

    std::unique_ptr<ASTNS::Program> parse();

private:
    Lexer &lexer;
    File &sourcefile;

    Token prevToken;
    Token currToken;

    Token& peek();
    Token& prev();

    void consume();
    bool checkConsume(TokenType type);
    Token& assertConsume(TokenType type, std::string message="");
    bool check(TokenType type);
    bool atEnd();

    bool ispanic;
    void panic();
    void unpanic();
    void syncTokens();

    std::unique_ptr<ASTNS::Decl> decl();
    std::unique_ptr<ASTNS::FunctionDecl> functiondecl();

    std::unique_ptr<ASTNS::Stmt> stmt();
    std::unique_ptr<ASTNS::VarStmt> varstmt();
    std::unique_ptr<ASTNS::ExprStmt> exprstmt();
    std::unique_ptr<ASTNS::ReturnStmt> returnstmt();
    std::unique_ptr<ASTNS::BlockStmt> blockstmt();

    std::unique_ptr<ASTNS::Expr> expr();

    std::unique_ptr<ASTNS::Type> type();
    std::unique_ptr<ASTNS::Param> params();
    std::unique_ptr<ASTNS::Arg> args();
};
