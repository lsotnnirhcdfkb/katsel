#pragma once

#include <memory>
#include <vector>
#include "lex/token.h"

namespace ASTNS
{
// ASTHEADER START
// The following code was autogenerated - see the utils/ directory
    class AST;
    class CUB;
    class Decl;
    class Stmt;
    class Expr;
    class Type;
    class ArgB;
    class ParamB;
    class VStmtIB;
    class CU;
    class FunctionDecl;
    class VarStmt;
    class VarStmtItem;
    class ExprStmt;
    class RetStmt;
    class Block;
    class PrimitiveType;
    class Arg;
    class Param;
    class IfExpr;
    class ForExpr;
    class AssignmentExpr;
    class ShortCircuitExpr;
    class BinaryExpr;
    class CastExpr;
    class UnaryExpr;
    class CallExpr;
    class PrimaryExpr;
    class AST
    {
    public:
        virtual ~AST() {}
    };
    class CUB : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitCU(ASTNS::CU *ast) = 0;
        };
        virtual ~CUB() {}
        virtual void accept(Visitor *v) = 0;
    };
    class Decl : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitFunctionDecl(ASTNS::FunctionDecl *ast) = 0;
        };
        virtual ~Decl() {}
        virtual void accept(Visitor *v) = 0;
    };
    class Stmt : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitVarStmt(ASTNS::VarStmt *ast) = 0;
            virtual void visitExprStmt(ASTNS::ExprStmt *ast) = 0;
            virtual void visitRetStmt(ASTNS::RetStmt *ast) = 0;
            virtual void visitBlock(ASTNS::Block *ast) = 0;
        };
        virtual ~Stmt() {}
        virtual void accept(Visitor *v) = 0;
    };
    class Expr : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitIfExpr(ASTNS::IfExpr *ast) = 0;
            virtual void visitForExpr(ASTNS::ForExpr *ast) = 0;
            virtual void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) = 0;
            virtual void visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast) = 0;
            virtual void visitBinaryExpr(ASTNS::BinaryExpr *ast) = 0;
            virtual void visitCastExpr(ASTNS::CastExpr *ast) = 0;
            virtual void visitUnaryExpr(ASTNS::UnaryExpr *ast) = 0;
            virtual void visitCallExpr(ASTNS::CallExpr *ast) = 0;
            virtual void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) = 0;
        };
        virtual ~Expr() {}
        virtual void accept(Visitor *v) = 0;
    };
    class Type : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitPrimitiveType(ASTNS::PrimitiveType *ast) = 0;
        };
        virtual ~Type() {}
        virtual void accept(Visitor *v) = 0;
    };
    class ArgB : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitArg(ASTNS::Arg *ast) = 0;
        };
        virtual ~ArgB() {}
        virtual void accept(Visitor *v) = 0;
    };
    class ParamB : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitParam(ASTNS::Param *ast) = 0;
        };
        virtual ~ParamB() {}
        virtual void accept(Visitor *v) = 0;
    };
    class VStmtIB : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitVarStmtItem(ASTNS::VarStmtItem *ast) = 0;
        };
        virtual ~VStmtIB() {}
        virtual void accept(Visitor *v) = 0;
    };
    class CU : public CUB
    {
    public:
        std::unique_ptr<Decl> decls;
        virtual void accept(ASTNS::CUB::Visitor *v) override;
        CU(std::unique_ptr<Decl> decls);
    };
    class FunctionDecl : public Decl
    {
    public:
        std::unique_ptr<Type> retty;
        Token name;
        std::vector<std::unique_ptr<Param>> params;
        virtual void accept(ASTNS::Decl::Visitor *v) override;
        FunctionDecl(std::unique_ptr<Type> retty, Token name, std::vector<std::unique_ptr<Param>> params);
    };
    class VarStmt : public Stmt
    {
    public:
        std::unique_ptr<Type> type;
        std::vector<std::unique_ptr<VarStmtItem>> assignments;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        VarStmt(std::unique_ptr<Type> type, std::vector<std::unique_ptr<VarStmtItem>> assignments);
    };
    class VarStmtItem : public VStmtIB
    {
    public:
        Token name;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::VStmtIB::Visitor *v) override;
        VarStmtItem(Token name, std::unique_ptr<Expr> expr);
    };
    class ExprStmt : public Stmt
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        ExprStmt(std::unique_ptr<Expr> expr);
    };
    class RetStmt : public Stmt
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        RetStmt(std::unique_ptr<Expr> expr);
    };
    class Block : public Stmt
    {
    public:
        std::vector<std::unique_ptr<Stmt>> stmts;
        std::unique_ptr<Expr> implRet;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        Block(std::vector<std::unique_ptr<Stmt>> stmts, std::unique_ptr<Expr> implRet);
    };
    class PrimitiveType : public Type
    {
    public:
        Token ty;
        virtual void accept(ASTNS::Type::Visitor *v) override;
        PrimitiveType(Token ty);
    };
    class Arg : public ArgB
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::ArgB::Visitor *v) override;
        Arg(std::unique_ptr<Expr> expr);
    };
    class Param : public ParamB
    {
    public:
        std::unique_ptr<Type> ty;
        Token name;
        virtual void accept(ASTNS::ParamB::Visitor *v) override;
        Param(std::unique_ptr<Type> ty, Token name);
    };
    class IfExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        IfExpr(std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
    };
    class ForExpr : public Expr
    {
    public:
        std::unique_ptr<VarStmt> start;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> increment;
        std::unique_ptr<Expr> body;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        ForExpr(std::unique_ptr<VarStmt> start, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> increment, std::unique_ptr<Expr> body);
    };
    class AssignmentExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> target;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        AssignmentExpr(std::unique_ptr<Expr> target, std::unique_ptr<Expr> expr);
    };
    class ShortCircuitExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        ShortCircuitExpr(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
    };
    class BinaryExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        BinaryExpr(std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
    };
    class CastExpr : public Expr
    {
    public:
        std::unique_ptr<Type> castto;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        CastExpr(std::unique_ptr<Type> castto, std::unique_ptr<Expr> expr);
    };
    class UnaryExpr : public Expr
    {
    public:
        Token op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        UnaryExpr(Token op, std::unique_ptr<Expr> expr);
    };
    class CallExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> callee;
        Token oparn;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        CallExpr(std::unique_ptr<Expr> callee, Token oparn, std::vector<std::unique_ptr<Arg>> args);
    };
    class PrimaryExpr : public Expr
    {
    public:
        Token value;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        PrimaryExpr(Token value);
    };
// This code was autogenerated - see the utils/ directory
// ASTHEADER END
}
