#include "visitor.h"
#include "ast.h"

void PrintVisitor::visitBinaryAST(const BinaryAST *ast) {
    std::cout << '(';
    ast->last->accept(this);
    std::cout << std::string(ast->op.start, ast->op.end);
    ast->rast->accept(this);
    std::cout << ')';
}

void PrintVisitor::visitTernaryOpAST(const TernaryOpAST *ast) {
    std::cout << '(';
    ast->conditional->accept(this);
    std::cout << '?';
    ast->trueast->accept(this);
    std::cout << ':';
    ast->falseast->accept(this);
    std::cout << ')';
}

void PrintVisitor::visitUnaryAST(const UnaryAST *ast) {
    std::cout << '(';
    std::cout << std::string(ast->op.start, ast->op.end);
    ast->ast->accept(this);
    std::cout << ')';
}

void PrintVisitor::visitPrimaryAST(const PrimaryAST *ast) {
    std::cout << std::string(ast->value.start, ast->value.end);
}

void PrintVisitor::visitExprStmtAST(const ExprStmtAST *ast) {
    std::cout << "ExprStmt: ";
    ast->ast->accept(this);
    std::cout << std::endl;
}

void PrintVisitor::visitProgramAST(const ProgramAST *ast) {
    std::cout << "Program: " << std::endl;

    for (const std::unique_ptr<AST> &sast : ast->asts) {
        std::cout << "  ";
        sast->accept(this);
    }
}
