#pragma once

#include "lex/lexer.h"
#include "lex/token.h"
#include "parse/ast.h"
#include "utils/file.h"

#include <memory>
#include <string>

#include <map>

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

    Token& consume();
    bool checkConsume(TokenType type);
    bool check(TokenType type);
    bool atEnd();

    bool assertConsume(TokenType type, std::string const &message="");

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

    std::unique_ptr<ASTNS::Expr> expr(int prec=0);

    std::unique_ptr<ASTNS::Expr> primary();
    std::unique_ptr<ASTNS::Expr> prefixOp();
    std::unique_ptr<ASTNS::Expr> parenExpr();
    std::unique_ptr<ASTNS::Expr> binaryOp(std::unique_ptr<ASTNS::Expr>);
    std::unique_ptr<ASTNS::Expr> ternaryOp(std::unique_ptr<ASTNS::Expr>);
    std::unique_ptr<ASTNS::Expr> callOp(std::unique_ptr<ASTNS::Expr>);

    int curPrec();

    // because of 'declaration reflects use' this delcaration i will totally
    // forget the meaning of so here it is:
    // declare a map with key type TokenType and value type of (function pointer
    // to function returning unique ptr to Expr and accepting no arguments)
    typedef std::unique_ptr<ASTNS::Expr> (Parser::*PrefixPF)();
    const static std::map<TokenType, PrefixPF> prefixParserTable;

    // declare a map with key type TokenType and value type of (function pointer
    // to function returning unique ptr to Expr and accepting one argument that
    // is of type unique ptr to Expr)
    typedef std::unique_ptr<ASTNS::Expr> (Parser::*NonPrefixPF)(std::unique_ptr<ASTNS::Expr>);
    const static std::map<TokenType, NonPrefixPF> nonPrefixTable;

    std::unique_ptr<ASTNS::Type> type();
    std::unique_ptr<ASTNS::Param> params();
    std::unique_ptr<ASTNS::Arg> args();
};
