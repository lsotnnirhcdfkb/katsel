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
class ParamAST;
class ParamsAST;
class VarStmtAST;
class AssignAST;
class VariableRefAST;
class ReturnStmtAST;
class ArgAST;
class ArgsAST;
class CallAST;

class Visitor
{
public:
    virtual void visitBinaryAST(const BinaryAST *ast) = 0;
    virtual void visitTernaryOpAST(const TernaryOpAST *ast) = 0;
    virtual void visitUnaryAST(const UnaryAST *ast) = 0;
    virtual void visitPrimaryAST(const PrimaryAST *ast) = 0;
    virtual void visitExprStmtAST(const ExprStmtAST *ast) = 0;
    virtual void visitProgramAST(const ProgramAST *ast) = 0;
    virtual void visitFunctionAST(const FunctionAST *ast) = 0;
    virtual void visitBlockAST(const BlockAST *ast) = 0;
    virtual void visitTypeAST(const TypeAST *ast) = 0;
    virtual void visitParamAST(const ParamAST *ast) = 0;
    virtual void visitParamsAST(const ParamsAST *ast) = 0;
    virtual void visitVarStmtAST(const VarStmtAST *ast) = 0;
    virtual void visitAssignAST(const AssignAST *ast) = 0;
    virtual void visitVariableRefAST(const VariableRefAST *ast) = 0;
    virtual void visitReturnStmtAST(const ReturnStmtAST *ast) = 0;
    virtual void visitArgAST(const ArgAST *ast) = 0;
    virtual void visitArgsAST(const ArgsAST *ast) = 0;
    virtual void visitCallAST(const CallAST *ast) = 0;
};

class PrintVisitor : public Visitor
{
public:
    PrintVisitor();
    void visitBinaryAST(const BinaryAST *ast) override;
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    void visitUnaryAST(const UnaryAST *ast) override;
    void visitPrimaryAST(const PrimaryAST *ast) override;
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    void visitProgramAST(const ProgramAST *ast) override;
    void visitVarStmtAST(const VarStmtAST *ast) override;
    void visitTypeAST(const TypeAST *ast) override;
    void visitFunctionAST(const FunctionAST *ast) override;
    void visitBlockAST(const BlockAST *ast) override;
    void visitParamAST(const ParamAST *ast) override;
    void visitParamsAST(const ParamsAST *ast) override;
    void visitAssignAST(const AssignAST *ast) override;
    void visitVariableRefAST(const VariableRefAST *ast) override;
    void visitReturnStmtAST(const ReturnStmtAST *ast) override;
    void visitArgAST(const ArgAST *ast) override;
    void visitArgsAST(const ArgsAST *ast) override;
    void visitCallAST(const CallAST *ast) override;

private:
    int indent;
    bool pindent;

    void print(std::string &str);
    void print(std::string &&str);
};

class BlankVisitor : public Visitor
{
public:
    void visitBinaryAST(const BinaryAST *ast) override;
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    void visitUnaryAST(const UnaryAST *ast) override;
    void visitPrimaryAST(const PrimaryAST *ast) override;
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    void visitProgramAST(const ProgramAST *ast) override;
    void visitFunctionAST(const FunctionAST *ast) override;
    void visitBlockAST(const BlockAST *ast) override;
    void visitTypeAST(const TypeAST *ast) override;
    void visitParamAST(const ParamAST *ast) override;
    void visitParamsAST(const ParamsAST *ast) override;
    void visitVarStmtAST(const VarStmtAST *ast) override;
    void visitAssignAST(const AssignAST *ast) override;
    void visitVariableRefAST(const VariableRefAST *ast) override;
    void visitReturnStmtAST(const ReturnStmtAST *ast) override;
    void visitArgAST(const ArgAST *ast) override;
    void visitArgsAST(const ArgsAST *ast) override;
    void visitCallAST(const CallAST *ast) override;
};
