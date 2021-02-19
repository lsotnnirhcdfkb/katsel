#pragma once

#include "ast/astfwd.h"

namespace ASTNS {
    // ASTVISITH START
    class CUBVisitor {
    public:
        virtual ~CUBVisitor() {}
        virtual void ast_visit(ASTNS::CU &ast) = 0;
    };
    class DeclVisitor {
    public:
        virtual ~DeclVisitor() {}
        virtual void ast_visit(ASTNS::ImplicitDecl &ast) = 0;
        virtual void ast_visit(ASTNS::ImplDecl &ast) = 0;
        virtual void ast_visit(ASTNS::FunctionDecl &ast) = 0;
    };
    class ImplMemberVisitor {
    public:
        virtual ~ImplMemberVisitor() {}
        virtual void ast_visit(ASTNS::FunctionImplMember &ast) = 0;
    };
    class StmtVisitor {
    public:
        virtual ~StmtVisitor() {}
        virtual void ast_visit(ASTNS::VarStmt &ast) = 0;
        virtual void ast_visit(ASTNS::ExprStmt &ast) = 0;
        virtual void ast_visit(ASTNS::RetStmt &ast) = 0;
    };
    class ExprVisitor {
    public:
        virtual ~ExprVisitor() {}
        virtual void ast_visit(ASTNS::Block &ast) = 0;
        virtual void ast_visit(ASTNS::IfExpr &ast) = 0;
        virtual void ast_visit(ASTNS::WhileExpr &ast) = 0;
        virtual void ast_visit(ASTNS::AssignmentExpr &ast) = 0;
        virtual void ast_visit(ASTNS::ShortCircuitExpr &ast) = 0;
        virtual void ast_visit(ASTNS::BinaryExpr &ast) = 0;
        virtual void ast_visit(ASTNS::CastExpr &ast) = 0;
        virtual void ast_visit(ASTNS::UnaryExpr &ast) = 0;
        virtual void ast_visit(ASTNS::AddrofExpr &ast) = 0;
        virtual void ast_visit(ASTNS::DerefExpr &ast) = 0;
        virtual void ast_visit(ASTNS::CallExpr &ast) = 0;
        virtual void ast_visit(ASTNS::FieldAccessExpr &ast) = 0;
        virtual void ast_visit(ASTNS::MethodCallExpr &ast) = 0;
        virtual void ast_visit(ASTNS::BoolLit &ast) = 0;
        virtual void ast_visit(ASTNS::FloatLit &ast) = 0;
        virtual void ast_visit(ASTNS::IntLit &ast) = 0;
        virtual void ast_visit(ASTNS::CharLit &ast) = 0;
        virtual void ast_visit(ASTNS::StringLit &ast) = 0;
        virtual void ast_visit(ASTNS::ThisExpr &ast) = 0;
        virtual void ast_visit(ASTNS::PathExpr &ast) = 0;
    };
    class TypeVisitor {
    public:
        virtual ~TypeVisitor() {}
        virtual void ast_visit(ASTNS::PathType &ast) = 0;
        virtual void ast_visit(ASTNS::PointerType &ast) = 0;
        virtual void ast_visit(ASTNS::ThisType &ast) = 0;
    };
    class ArgBVisitor {
    public:
        virtual ~ArgBVisitor() {}
        virtual void ast_visit(ASTNS::Arg &ast) = 0;
    };
    class ParamBVisitor {
    public:
        virtual ~ParamBVisitor() {}
        virtual void ast_visit(ASTNS::Param &ast) = 0;
        virtual void ast_visit(ASTNS::ThisParam &ast) = 0;
    };
    class VStmtIBVisitor {
    public:
        virtual ~VStmtIBVisitor() {}
    };
    class PathBVisitor {
    public:
        virtual ~PathBVisitor() {}
        virtual void ast_visit(ASTNS::Path &ast) = 0;
    };
    class ListBVisitor {
    public:
        virtual ~ListBVisitor() {}
        virtual void ast_visit(ASTNS::DeclList &ast) = 0;
        virtual void ast_visit(ASTNS::StmtList &ast) = 0;
        virtual void ast_visit(ASTNS::ParamList &ast) = 0;
        virtual void ast_visit(ASTNS::ArgList &ast) = 0;
        virtual void ast_visit(ASTNS::ImplMemberList &ast) = 0;
    };
    class PureLocationBVisitor {
    public:
        virtual ~PureLocationBVisitor() {}
        virtual void ast_visit(ASTNS::PureLocation &ast) = 0;
    };
    // ASTVISITH END
}
