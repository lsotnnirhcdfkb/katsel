#pragma once

#include "ast/visitor.h"

#include "llvm/Support/raw_ostream.h"

namespace ASTNS {
    class PrintVisitor :
        // PRINTVISIT INHERIT START
        public ASTNS::CUBVisitor,
        public ASTNS::DeclVisitor,
        public ASTNS::ImplMemberVisitor,
        public ASTNS::StmtVisitor,
        public ASTNS::ExprVisitor,
        public ASTNS::TypeVisitor,
        public ASTNS::ParamBVisitor,
        public ASTNS::VStmtIBVisitor,
        public ASTNS::PathBVisitor
        // PRINTVISIT INHERIT END
    {
    public:
        PrintVisitor(llvm::raw_ostream &ostream);
        // PRINTVISIT METHODS START
        void ast_visit(ASTNS::CU &ast) override;
        void ast_visit(ASTNS::ImplDecl &ast) override;
        void ast_visit(ASTNS::FunctionDecl &ast) override;
        void ast_visit(ASTNS::FunctionImplMember &ast) override;
        void ast_visit(ASTNS::VarStmt &ast) override;
        void ast_visit(ASTNS::ExprStmt &ast) override;
        void ast_visit(ASTNS::RetStmt &ast) override;
        void ast_visit(ASTNS::PathType &ast) override;
        void ast_visit(ASTNS::PointerType &ast) override;
        void ast_visit(ASTNS::ThisType &ast) override;
        void ast_visit(ASTNS::Param &ast) override;
        void ast_visit(ASTNS::ThisParam &ast) override;
        void ast_visit(ASTNS::Block &ast) override;
        void ast_visit(ASTNS::IfExpr &ast) override;
        void ast_visit(ASTNS::WhileExpr &ast) override;
        void ast_visit(ASTNS::AssignmentExpr &ast) override;
        void ast_visit(ASTNS::ShortCircuitExpr &ast) override;
        void ast_visit(ASTNS::BinaryExpr &ast) override;
        void ast_visit(ASTNS::CastExpr &ast) override;
        void ast_visit(ASTNS::UnaryExpr &ast) override;
        void ast_visit(ASTNS::AddrofExpr &ast) override;
        void ast_visit(ASTNS::DerefExpr &ast) override;
        void ast_visit(ASTNS::CallExpr &ast) override;
        void ast_visit(ASTNS::FieldAccessExpr &ast) override;
        void ast_visit(ASTNS::MethodCallExpr &ast) override;
        void ast_visit(ASTNS::BoolLit &ast) override;
        void ast_visit(ASTNS::FloatLit &ast) override;
        void ast_visit(ASTNS::IntLit &ast) override;
        void ast_visit(ASTNS::CharLit &ast) override;
        void ast_visit(ASTNS::StringLit &ast) override;
        void ast_visit(ASTNS::ThisExpr &ast) override;
        void ast_visit(ASTNS::PathExpr &ast) override;
        void ast_visit(ASTNS::Path &ast) override;
        // PRINTVISIT METHODS END

        void pai(std::string const &s);
        llvm::raw_ostream &ostream;
        int indent;

    private:
        bool pindent;
        // short for print at indent
    };
}
