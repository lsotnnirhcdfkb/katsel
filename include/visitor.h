#pragma once

// forward declare because circular includes don't work
class AST;

class BinaryAST;
class TernaryOpAST;
class UnaryAST;
class PrimaryAST;
class ExprStmtAST;
class ProgramAST;
class VarStmtAST;
class TypeAST;

class Visitor
{
public:
    virtual void visitBinaryAST(const BinaryAST *ast) = 0;
    virtual void visitTernaryOpAST(const TernaryOpAST *ast) = 0;
    virtual void visitUnaryAST(const UnaryAST *ast) = 0;
    virtual void visitPrimaryAST(const PrimaryAST *ast) = 0;
    virtual void visitExprStmtAST(const ExprStmtAST *ast) = 0;
    virtual void visitProgramAST(const ProgramAST *ast) = 0;
    virtual void visitVarStmtAST(const VarStmtAST *ast) = 0;
    virtual void visitTypeAST(const TypeAST *ast) = 0;
};

class PrintVisitor : public Visitor
{
public:
    void visitBinaryAST(const BinaryAST *ast);
    void visitTernaryOpAST(const TernaryOpAST *ast);
    void visitUnaryAST(const UnaryAST *ast);
    void visitPrimaryAST(const PrimaryAST *ast);
    void visitExprStmtAST(const ExprStmtAST *ast);
    void visitProgramAST(const ProgramAST *ast);
    void visitVarStmtAST(const VarStmtAST *ast);
    void visitTypeAST(const TypeAST *ast);
};

class PythonGenVisitor : public Visitor
{
public:
    void visitBinaryAST(const BinaryAST *ast);
    void visitTernaryOpAST(const TernaryOpAST *ast);
    void visitUnaryAST(const UnaryAST *ast);
    void visitPrimaryAST(const PrimaryAST *ast);
    void visitExprStmtAST(const ExprStmtAST *ast);
    void visitProgramAST(const ProgramAST *ast);
    void visitVarStmtAST(const VarStmtAST *ast);
    void visitTypeAST(const TypeAST *ast);
};
