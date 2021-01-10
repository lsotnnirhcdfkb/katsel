#pragma once

#include "ast/ast.h"

#include "llvm/Support/raw_ostream.h"

namespace ASTNS {
    class PrintVisitor :
        // PRINTVISIT INHERIT START
// The following code was autogenerated - see the utils/ directory
public ASTNS::CUB::Visitor,
public ASTNS::Decl::Visitor,
public ASTNS::ImplItem::Visitor,
public ASTNS::Stmt::Visitor,
public ASTNS::Expr::Visitor,
public ASTNS::Type::Visitor,
public ASTNS::ArgB::Visitor,
public ASTNS::ParamB::Visitor,
public ASTNS::VStmtIB::Visitor,
public ASTNS::PathB::Visitor,
public ASTNS::ListB::Visitor,
public ASTNS::PureLocationB::Visitor
// This code was autogenerated - see the utils/ directory
        // PRINTVISIT INHERIT END
    {
    public:
        PrintVisitor(llvm::raw_ostream &ostream);
        // PRINTVISIT METHODS START
// The following code was autogenerated - see the utils/ directory
void visitDeclList(ASTNS::DeclList *ast) override;
void visitStmtList(ASTNS::StmtList *ast) override;
void visitParamList(ASTNS::ParamList *ast) override;
void visitArgList(ASTNS::ArgList *ast) override;
void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) override;
void visitImplItemList(ASTNS::ImplItemList *ast) override;
void visitPureLocation(ASTNS::PureLocation *ast) override;
void visitImplicitDecl(ASTNS::ImplicitDecl *ast) override;
void visitCU(ASTNS::CU *ast) override;
void visitImplDecl(ASTNS::ImplDecl *ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl *ast) override;
void visitFunctionImplItem(ASTNS::FunctionImplItem *ast) override;
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitPathType(ASTNS::PathType *ast) override;
void visitPointerType(ASTNS::PointerType *ast) override;
void visitThisType(ASTNS::ThisType *ast) override;
void visitArg(ASTNS::Arg *ast) override;
void visitParam(ASTNS::Param *ast) override;
void visitThisParam(ASTNS::ThisParam *ast) override;
void visitBlock(ASTNS::Block *ast) override;
void visitIfExpr(ASTNS::IfExpr *ast) override;
void visitForExpr(ASTNS::ForExpr *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast) override;
void visitBinaryExpr(ASTNS::BinaryExpr *ast) override;
void visitCastExpr(ASTNS::CastExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
void visitAddrofExpr(ASTNS::AddrofExpr *ast) override;
void visitDerefExpr(ASTNS::DerefExpr *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
void visitPathExpr(ASTNS::PathExpr *ast) override;
void visitPath(ASTNS::Path *ast) override;
// This code was autogenerated - see the utils/ directory
        // PRINTVISIT METHODS END

        void pai(std::string const &s);
        llvm::raw_ostream &ostream;
        int indent;

    private:
        bool pindent;
        // short for print at indent
    };
}
