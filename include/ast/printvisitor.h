#pragma once

#include "ast/ast.h"

#include "llvm/Support/raw_ostream.h"

namespace ASTNS
{
    class PrintVisitor :
        // PRINTVISIT INHERIT START
// The following code was autogenerated - see the utils/ directory
public ASTNS::CUB::Visitor,
public ASTNS::Decl::Visitor,
public ASTNS::Stmt::Visitor,
public ASTNS::Expr::Visitor,
public ASTNS::Type::Visitor,
public ASTNS::ArgB::Visitor,
public ASTNS::ParamB::Visitor,
public ASTNS::VStmtIB::Visitor
// This code was autogenerated - see the utils/ directory
        // PRINTVISIT INHERIT END
    {
    public:
        PrintVisitor(llvm::raw_ostream &ostream);
        // PRINTVISIT METHODS START
// The following code was autogenerated - see the utils/ directory
void visitCU(ASTNS::CU *ast) override;
void visitDeclList(ASTNS::DeclList *ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl *ast) override;
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) override;
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitStmtList(ASTNS::StmtList *ast) override;
void visitImplRet(ASTNS::ImplRet *ast) override;
void visitPrimitiveType(ASTNS::PrimitiveType *ast) override;
void visitArg(ASTNS::Arg *ast) override;
void visitArgList(ASTNS::ArgList *ast) override;
void visitParam(ASTNS::Param *ast) override;
void visitParamList(ASTNS::ParamList *ast) override;
void visitBlock(ASTNS::Block *ast) override;
void visitIfExpr(ASTNS::IfExpr *ast) override;
void visitForExpr(ASTNS::ForExpr *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast) override;
void visitBinaryExpr(ASTNS::BinaryExpr *ast) override;
void visitCastExpr(ASTNS::CastExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
// This code was autogenerated - see the utils/ directory
        // PRINTVISIT METHODS END

    private:
        int indent;
        bool pindent;
        // short for print at indent
        void pai(std::string const &s);

        template <typename T>
        void printField(std::unique_ptr<T> &ast)
        {
            ast->accept(this);
        }
        template <typename T>
        void printField(std::vector<std::unique_ptr<T>> &v)
        {
            pai("[\n");
            ++indent;
            for (std::unique_ptr<T> &a : v)
                a->accept(this);
            --indent;
            pai("]\n");
        }
        void printField(Token const &t);

        llvm::raw_ostream &ostream;
    };
}
