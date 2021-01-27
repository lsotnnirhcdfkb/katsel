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
        public ASTNS::ArgBVisitor,
        public ASTNS::ParamBVisitor,
        public ASTNS::VStmtIBVisitor,
        public ASTNS::PathBVisitor,
        public ASTNS::ListBVisitor,
        public ASTNS::PureLocationBVisitor
        // PRINTVISIT INHERIT END
    {
    public:
        PrintVisitor(llvm::raw_ostream &ostream);
        // PRINTVISIT METHODS START
        void visit(ASTNS::DeclList &ast) override;
        void visit(ASTNS::StmtList &ast) override;
        void visit(ASTNS::ParamList &ast) override;
        void visit(ASTNS::ArgList &ast) override;
        void visit(ASTNS::VarStmtItemList &ast) override;
        void visit(ASTNS::ImplMemberList &ast) override;
        void visit(ASTNS::PureLocation &ast) override;
        void visit(ASTNS::ImplicitDecl &ast) override;
        void visit(ASTNS::CU &ast) override;
        void visit(ASTNS::ImplDecl &ast) override;
        void visit(ASTNS::FunctionDecl &ast) override;
        void visit(ASTNS::FunctionImplMember &ast) override;
        void visit(ASTNS::VarStmt &ast) override;
        void visit(ASTNS::VarStmtItem &ast) override;
        void visit(ASTNS::ExprStmt &ast) override;
        void visit(ASTNS::RetStmt &ast) override;
        void visit(ASTNS::PathType &ast) override;
        void visit(ASTNS::PointerType &ast) override;
        void visit(ASTNS::ThisType &ast) override;
        void visit(ASTNS::Arg &ast) override;
        void visit(ASTNS::Param &ast) override;
        void visit(ASTNS::ThisParam &ast) override;
        void visit(ASTNS::Block &ast) override;
        void visit(ASTNS::IfExpr &ast) override;
        void visit(ASTNS::WhileExpr &ast) override;
        void visit(ASTNS::AssignmentExpr &ast) override;
        void visit(ASTNS::ShortCircuitExpr &ast) override;
        void visit(ASTNS::BinaryExpr &ast) override;
        void visit(ASTNS::CastExpr &ast) override;
        void visit(ASTNS::UnaryExpr &ast) override;
        void visit(ASTNS::AddrofExpr &ast) override;
        void visit(ASTNS::DerefExpr &ast) override;
        void visit(ASTNS::CallExpr &ast) override;
        void visit(ASTNS::FieldAccessExpr &ast) override;
        void visit(ASTNS::MethodCallExpr &ast) override;
        void visit(ASTNS::BoolLit &ast) override;
        void visit(ASTNS::FloatLit &ast) override;
        void visit(ASTNS::IntLit &ast) override;
        void visit(ASTNS::CharLit &ast) override;
        void visit(ASTNS::StringLit &ast) override;
        void visit(ASTNS::ThisExpr &ast) override;
        void visit(ASTNS::PathExpr &ast) override;
        void visit(ASTNS::Path &ast) override;
        // PRINTVISIT METHODS END

        void pai(std::string const &s);
        llvm::raw_ostream &ostream;
        int indent;

    private:
        bool pindent;
        // short for print at indent
    };
}
