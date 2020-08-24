#include "visitor.h"

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
};
