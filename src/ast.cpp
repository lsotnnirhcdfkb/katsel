#include "ast.h"

BinaryAST::BinaryAST(Token op, std::unique_ptr<AST> &last, std::unique_ptr<AST> &rast): op(op), last(std::move(last)), rast(std::move(rast)) {}
TernaryOpAST::TernaryOpAST(std::unique_ptr<AST> &conditional, std::unique_ptr<AST> &trueast, std::unique_ptr<AST> &falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}
UnaryAST::UnaryAST(Token op, std::unique_ptr<AST> &ast): op(op), ast(std::move(ast)) {}
PrimaryAST::PrimaryAST(Token value): value(value) {}
ExprStmtAST::ExprStmtAST(std::unique_ptr<AST> &ast): ast(std::move(ast)) {}

void BinaryAST::print()
{
    std::cout << '(';
    last->print();
    std::cout << std::string(op.start, op.end);
    rast->print();
    std::cout << ')';
}
void TernaryOpAST::print()
{
    std::cout << '(';
    conditional->print();
    std::cout << '?';
    trueast->print();
    std::cout << ':';
    falseast->print();
    std::cout << ')';
}
void UnaryAST::print()
{
    std::cout << '(';
    std::cout << std::string(op.start, op.end);
    ast->print();
    std::cout << ')';
}

void PrimaryAST::print()
{
    std::cout << std::string(value.start, value.end);
}

void ExprStmtAST::print()
{
    std::cout << "Stmt: ";
    ast->print();
}
