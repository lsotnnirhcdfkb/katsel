#pragma once

#include "ast/visitor.h"
#include "ast/ast.h"

#include "llvm/Support/raw_ostream.h"

namespace ASTNS
{
    class PrintVisitor :
        public ASTNS::DeclBVisitor,
        public ASTNS::ArgBVisitor,
        public ASTNS::StmtBVisitor,
        public ASTNS::ExprBVisitor,
        public ASTNS::VStmtIBVisitor,
        public ASTNS::PListBVisitor,
        public ASTNS::TypeBVisitor,
        public ASTNS::CUBVisitor
    {
    public:
        PrintVisitor(llvm::raw_ostream &ostream);
        // PRINTVISIT METHODS START

// The following code was autogenerated - see the utils/ directory
void visitAdditionExpr(ASTNS::AdditionExpr *ast) override;
void visitArg(ASTNS::Arg *ast) override;
void visitArgList(ASTNS::ArgList *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitBinandExpr(ASTNS::BinandExpr *ast) override;
void visitBinorExpr(ASTNS::BinorExpr *ast) override;
void visitBitandExpr(ASTNS::BitandExpr *ast) override;
void visitBitorExpr(ASTNS::BitorExpr *ast) override;
void visitBitshiftExpr(ASTNS::BitshiftExpr *ast) override;
void visitBitxorExpr(ASTNS::BitxorExpr *ast) override;
void visitBlock(ASTNS::Block *ast) override;
void visitBuiltinTypeNoVoid(ASTNS::BuiltinTypeNoVoid *ast) override;
void visitBuiltinTypeVoid(ASTNS::BuiltinTypeVoid *ast) override;
void visitCU(ASTNS::CU *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitCompeqExpr(ASTNS::CompeqExpr *ast) override;
void visitComplgtExpr(ASTNS::ComplgtExpr *ast) override;
void visitDeclList(ASTNS::DeclList *ast) override;
void visitEmptyStmt(ASTNS::EmptyStmt *ast) override;
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitFunction(ASTNS::Function *ast) override;
void visitMoreArg(ASTNS::MoreArg *ast) override;
void visitMoreDecl(ASTNS::MoreDecl *ast) override;
void visitMoreParam(ASTNS::MoreParam *ast) override;
void visitMoreStmt(ASTNS::MoreStmt *ast) override;
void visitMoreVarStmtItem(ASTNS::MoreVarStmtItem *ast) override;
void visitMultExpr(ASTNS::MultExpr *ast) override;
void visitParam(ASTNS::Param *ast) override;
void visitParamList(ASTNS::ParamList *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitStmtList(ASTNS::StmtList *ast) override;
void visitTernaryExpr(ASTNS::TernaryExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) override;
// This code was autogenerated - see the utils/ directory

        // PRINTVISIT METHODS END

    private:
        int indent;
        bool pindent;
        // short for print at indent
        void pai(std::string &s);
        void pai(std::string &&s);

        llvm::raw_ostream &ostream;
    };
}
