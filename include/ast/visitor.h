#pragma once

#include "ast/astfwd.h"

namespace ASTNS {
    // ASTVISITH START
    class CUBVisitor {
    public:
        virtual ~CUBVisitor() {}
        virtual void visit(ASTNS::CU &ast) = 0;
    };
    class DeclVisitor {
    public:
        virtual ~DeclVisitor() {}
        virtual void visit(ASTNS::ImplicitDecl &ast) = 0;
        virtual void visit(ASTNS::ImplDecl &ast) = 0;
        virtual void visit(ASTNS::FunctionDecl &ast) = 0;
    };
    class ImplMemberVisitor {
    public:
        virtual ~ImplMemberVisitor() {}
        virtual void visit(ASTNS::FunctionImplMember &ast) = 0;
    };
    class StmtVisitor {
    public:
        virtual ~StmtVisitor() {}
        virtual void visit(ASTNS::VarStmt &ast) = 0;
        virtual void visit(ASTNS::ExprStmt &ast) = 0;
        virtual void visit(ASTNS::RetStmt &ast) = 0;
    };
    class ExprVisitor {
    public:
        virtual ~ExprVisitor() {}
        virtual void visit(ASTNS::Block &ast) = 0;
        virtual void visit(ASTNS::IfExpr &ast) = 0;
        virtual void visit(ASTNS::WhileExpr &ast) = 0;
        virtual void visit(ASTNS::AssignmentExpr &ast) = 0;
        virtual void visit(ASTNS::ShortCircuitExpr &ast) = 0;
        virtual void visit(ASTNS::BinaryExpr &ast) = 0;
        virtual void visit(ASTNS::CastExpr &ast) = 0;
        virtual void visit(ASTNS::UnaryExpr &ast) = 0;
        virtual void visit(ASTNS::AddrofExpr &ast) = 0;
        virtual void visit(ASTNS::DerefExpr &ast) = 0;
        virtual void visit(ASTNS::CallExpr &ast) = 0;
        virtual void visit(ASTNS::FieldAccessExpr &ast) = 0;
        virtual void visit(ASTNS::MethodCallExpr &ast) = 0;
        virtual void visit(ASTNS::PrimaryExpr &ast) = 0;
        virtual void visit(ASTNS::PathExpr &ast) = 0;
    };
    class TypeVisitor {
    public:
        virtual ~TypeVisitor() {}
        virtual void visit(ASTNS::PathType &ast) = 0;
        virtual void visit(ASTNS::PointerType &ast) = 0;
        virtual void visit(ASTNS::ThisType &ast) = 0;
    };
    class ArgBVisitor {
    public:
        virtual ~ArgBVisitor() {}
        virtual void visit(ASTNS::Arg &ast) = 0;
    };
    class ParamBVisitor {
    public:
        virtual ~ParamBVisitor() {}
        virtual void visit(ASTNS::Param &ast) = 0;
        virtual void visit(ASTNS::ThisParam &ast) = 0;
    };
    class VStmtIBVisitor {
    public:
        virtual ~VStmtIBVisitor() {}
        virtual void visit(ASTNS::VarStmtItem &ast) = 0;
    };
    class PathBVisitor {
    public:
        virtual ~PathBVisitor() {}
        virtual void visit(ASTNS::Path &ast) = 0;
    };
    class ListBVisitor {
    public:
        virtual ~ListBVisitor() {}
        virtual void visit(ASTNS::DeclList &ast) = 0;
        virtual void visit(ASTNS::StmtList &ast) = 0;
        virtual void visit(ASTNS::ParamList &ast) = 0;
        virtual void visit(ASTNS::ArgList &ast) = 0;
        virtual void visit(ASTNS::VarStmtItemList &ast) = 0;
        virtual void visit(ASTNS::ImplMemberList &ast) = 0;
    };
    class PureLocationBVisitor {
    public:
        virtual ~PureLocationBVisitor() {}
        virtual void visit(ASTNS::PureLocation &ast) = 0;
    };
    // ASTVISITH END
}
