#include "visitor.h"
#include "ast.h"

// {{{ print
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

void PrintVisitor::visitVarStmtAST(const VarStmtAST *ast) {
    std::cout << "VarStmt: var " + std::string(ast->name.start, ast->name.end) << " of type ";
    ast->type->accept(this);
    std::cout << " being assigned ";
    ast->expression->accept(this);
}

void PrintVisitor::visitTypeAST(const TypeAST *ast) {
    std::cout << "TypeAST: " << std::string(ast->type.start, ast->type.end);
}
// }}}

// {{{ python
void PythonGenVisitor::visitBinaryAST(const BinaryAST *ast) {
    std::cout << '(';
    ast->last->accept(this);
    std::cout << std::string(ast->op.start, ast->op.end);
    ast->rast->accept(this);
    std::cout << ')';
}

void PythonGenVisitor::visitTernaryOpAST(const TernaryOpAST *ast) {
    std::cout << '(';
    ast->trueast->accept(this);
    std::cout << " if ";
    ast->conditional->accept(this);
    std::cout << " else ";
    ast->falseast->accept(this);
    std::cout << ')';
}

void PythonGenVisitor::visitUnaryAST(const UnaryAST *ast) {
    std::cout << '(';
    std::cout << std::string(ast->op.start, ast->op.end);
    ast->ast->accept(this);
    std::cout << ')';
}

void PythonGenVisitor::visitPrimaryAST(const PrimaryAST *ast) {
    std::cout << '(' << std::string(ast->value.start, ast->value.end) << ')';
}

void PythonGenVisitor::visitExprStmtAST(const ExprStmtAST *ast) {
    ast->ast->accept(this);
    std::cout << std::endl;
}

void PythonGenVisitor::visitProgramAST(const ProgramAST *ast) {
    for (const std::unique_ptr<AST> &sast : ast->asts) {
        sast->accept(this);
    }
}

void PythonGenVisitor::visitVarStmtAST(const VarStmtAST *ast) {
}

void PythonGenVisitor::visitTypeAST(const TypeAST *ast) {
}
// }}}
