#pragma once

#include <memory>
#include "token.h"
#include "annotations.h"
#include "visitor.h"

namespace ASTNS
{
// ASTHEADER START
    class Expr;
    class Decl;
    class Type;
    class Stmt;
    class Program;
    class BinaryExpr;
    class TernaryExpr;
    class UnaryExpr;
    class PrimaryExpr;
    class AssignExpr;
    class CallExpr;
    class LtoRVExpr;
    class BlockStmt;
    class ExprStmt;
    class ReturnStmt;
    class VarStmt;
    class VarRef;
    class BaseType;
    class FunctionDecl;
    class GlobalVarDecl;
    class Param;
    class Arg;

    class Expr
    {
    public:
        virtual ~Expr() {}
        ExprAn exprAn;
        virtual void accept(ExprVisitor *v) = 0;
    };
    class Decl
    {
    public:
        virtual ~Decl() {}
        virtual void accept(DeclVisitor *v) = 0;
    };
    class Type
    {
    public:
        virtual ~Type() {}
        virtual void accept(TypeVisitor *v) = 0;
    };
    class Stmt
    {
    public:
        virtual ~Stmt() {}
        virtual void accept(StmtVisitor *v) = 0;
    };
    class Program
    {
    public:
        Program(std::vector<std::unique_ptr<Decl>> &decls);
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(ProgramVisitor *v);
    };
    class BinaryExpr : public Expr
    {
    public:
        BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op);
        std::unique_ptr<Expr> lhs;
        std::unique_ptr<Expr> rhs;
        Token op;
        virtual void accept(ExprVisitor *v);
    };
    class TernaryExpr : public Expr
    {
    public:
        TernaryExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
        std::unique_ptr<Expr> condition;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void accept(ExprVisitor *v);
    };
    class UnaryExpr : public Expr
    {
    public:
        UnaryExpr(std::unique_ptr<Expr> operand, Token op);
        std::unique_ptr<Expr> operand;
        Token op;
        virtual void accept(ExprVisitor *v);
    };
    class PrimaryExpr : public Expr
    {
    public:
        PrimaryExpr(Token value);
        Token value;
        virtual void accept(ExprVisitor *v);
    };
    class AssignExpr : public Expr
    {
    public:
        AssignExpr(std::unique_ptr<Expr> assignee, std::unique_ptr<Expr> value);
        std::unique_ptr<Expr> assignee;
        std::unique_ptr<Expr> value;
        virtual void accept(ExprVisitor *v);
    };
    class CallExpr : public Expr
    {
    public:
        CallExpr(std::unique_ptr<Expr> func, std::unique_ptr<Arg> args);
        std::unique_ptr<Expr> func;
        std::unique_ptr<Arg> args;
        virtual void accept(ExprVisitor *v);
    };
    class LtoRVExpr : public Expr
    {
    public:
        LtoRVExpr(std::unique_ptr<Expr> val);
        std::unique_ptr<Expr> val;
        virtual void accept(ExprVisitor *v);
    };
    class BlockStmt : public Stmt
    {
    public:
        BlockStmt(std::vector<std::unique_ptr<Stmt>> &stmts);
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(StmtVisitor *v);
    };
    class ExprStmt : public Stmt
    {
    public:
        ExprStmt(std::unique_ptr<Expr> expr);
        std::unique_ptr<Expr> expr;
        virtual void accept(StmtVisitor *v);
    };
    class ReturnStmt : public Stmt
    {
    public:
        ReturnStmt(std::unique_ptr<Expr> val);
        std::unique_ptr<Expr> val;
        virtual void accept(StmtVisitor *v);
    };
    class VarStmt : public Stmt
    {
    public:
        VarStmt(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value);
        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Expr> value;
        virtual void accept(StmtVisitor *v);
    };
    class VarRef : public Expr
    {
    public:
        VarRef(Token var);
        Token var;
        virtual void accept(ExprVisitor *v);
    };
    class BaseType : public Type
    {
    public:
        BaseType(Token type);
        Token type;
        virtual void accept(TypeVisitor *v);
    };
    class FunctionDecl : public Decl
    {
    public:
        FunctionDecl(std::unique_ptr<Type> rettype, Token name, std::unique_ptr<Param> params, std::unique_ptr<BlockStmt> block);
        std::unique_ptr<Type> rettype;
        Token name;
        std::unique_ptr<Param> params;
        std::unique_ptr<BlockStmt> block;
        FuncDeclAn funcDeclAn;
        virtual void accept(DeclVisitor *v);
    };
    class GlobalVarDecl : public Decl
    {
    public:
        GlobalVarDecl(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value);
        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Expr> value;
        virtual void accept(DeclVisitor *v);
    };
    class Param
    {
    public:
        Param(std::unique_ptr<Type> type, Token name, std::unique_ptr<Param> next);
        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Param> next;
        virtual void accept(ParamVisitor *v);
    };
    class Arg
    {
    public:
        Arg(std::unique_ptr<Expr> value, std::unique_ptr<Arg> next);
        std::unique_ptr<Expr> value;
        std::unique_ptr<Arg> next;
        virtual void accept(ArgVisitor *v);
    };
// ASTHEADER END
}
