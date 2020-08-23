#pragma once

#include <string>

// forward declare because circular includes don't work
class AST;

class BinaryAST;
class TernaryOpAST;
class UnaryAST;
class PrimaryAST;
class ExprStmtAST;
class ProgramAST;
class FunctionAST;
class BlockAST;
class TypeAST;
class ArgAST;
class ArgsAST;
class VarStmtAST;

class Visitor
{
public:
    virtual void visitBinaryAST(const BinaryAST *ast) = 0;
    virtual void visitTernaryOpAST(const TernaryOpAST *ast) = 0;
    virtual void visitUnaryAST(const UnaryAST *ast) = 0;
    virtual void visitPrimaryAST(const PrimaryAST *ast) = 0;
    virtual void visitExprStmtAST(const ExprStmtAST *ast) = 0;
    virtual void visitProgramAST(const ProgramAST *ast) = 0;
<<<<<<< HEAD
    virtual void visitFunctionAST(const FunctionAST *ast) = 0;
    virtual void visitBlockAST(const BlockAST *ast) = 0;
    virtual void visitTypeAST(const TypeAST *ast) = 0;
    virtual void visitArgAST(const ArgAST *ast) = 0;
    virtual void visitArgsAST(const ArgsAST *ast) = 0;
||||||| a45fef1
=======
    virtual void visitVarStmtAST(const VarStmtAST *ast) = 0;
    virtual void visitTypeAST(const TypeAST *ast) = 0;
>>>>>>> assignment
};

class PrintVisitor : public Visitor
{
public:
<<<<<<< HEAD
    PrintVisitor();
||||||| a45fef1
    void visitBinaryAST(const BinaryAST *ast);
    void visitTernaryOpAST(const TernaryOpAST *ast);
    void visitUnaryAST(const UnaryAST *ast);
    void visitPrimaryAST(const PrimaryAST *ast);
    void visitExprStmtAST(const ExprStmtAST *ast);
    void visitProgramAST(const ProgramAST *ast);
};
=======
    void visitBinaryAST(const BinaryAST *ast);
    void visitTernaryOpAST(const TernaryOpAST *ast);
    void visitUnaryAST(const UnaryAST *ast);
    void visitPrimaryAST(const PrimaryAST *ast);
    void visitExprStmtAST(const ExprStmtAST *ast);
    void visitProgramAST(const ProgramAST *ast);
    void visitVarStmtAST(const VarStmtAST *ast);
    void visitTypeAST(const TypeAST *ast);
};
>>>>>>> assignment

<<<<<<< HEAD
    void visitBinaryAST(const BinaryAST *ast) override;
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    void visitUnaryAST(const UnaryAST *ast) override;
    void visitPrimaryAST(const PrimaryAST *ast) override;
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    void visitProgramAST(const ProgramAST *ast) override;
    void visitFunctionAST(const FunctionAST *ast) override;
    void visitBlockAST(const BlockAST *ast) override;
    void visitTypeAST(const TypeAST *ast) override;
    void visitArgAST(const ArgAST *ast) override;
    void visitArgsAST(const ArgsAST *ast) override;

private:
    int indent;
    bool pindent;

    void print(std::string &str);
    void print(std::string &&str);
||||||| a45fef1
class PythonGenVisitor : public Visitor
{
public:
    void visitBinaryAST(const BinaryAST *ast);
    void visitTernaryOpAST(const TernaryOpAST *ast);
    void visitUnaryAST(const UnaryAST *ast);
    void visitPrimaryAST(const PrimaryAST *ast);
    void visitExprStmtAST(const ExprStmtAST *ast);
    void visitProgramAST(const ProgramAST *ast);
=======
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
>>>>>>> assignment
};
