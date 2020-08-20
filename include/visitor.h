#pragma once

// forward declare because circular includes don't work
class AST;

class BinaryAST;
class TernaryOpAST;
class UnaryAST;
class PrimaryAST;
class ExprStmtAST;
class ProgramAST;

class Visitor
{
public:
    virtual void visitBinaryAST(const BinaryAST *ast) = 0;
    virtual void visitTernaryOpAST(const TernaryOpAST *ast) = 0;
    virtual void visitUnaryAST(const UnaryAST *ast) = 0;
    virtual void visitPrimaryAST(const PrimaryAST *ast) = 0;
    virtual void visitExprStmtAST(const ExprStmtAST *ast) = 0;
    virtual void visitProgramAST(const ProgramAST *ast) = 0;
};

