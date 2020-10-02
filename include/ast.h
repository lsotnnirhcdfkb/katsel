#include <memory>
#include "token.h"

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
        bool exprAnValid;
        ExprAn exprAn;
    };
    class Decl
    {
    public:
        virtual ~Decl() {}
    };
    class Type
    {
    public:
        virtual ~Type() {}
    };
    class Stmt
    {
    public:
        virtual ~Stmt() {}
    };
    class Program
    {
    public:
        Program(std::vector<Decl> &decls);

        std::vector<Decl> decls;
    };
    class BinaryExpr : public Expr
    {
    public:
        BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op);

        std::unique_ptr<Expr> lhs;
        std::unique_ptr<Expr> rhs;
        Token op;
    };
    class TernaryExpr : public Expr
    {
    public:
        TernaryExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);

        std::unique_ptr<Expr> condition;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
    };
    class UnaryExpr : public Expr
    {
    public:
        UnaryExpr(std::unique_ptr<Expr> operand, Token op);

        std::unique_ptr<Expr> operand;
        Token op;
    };
    class PrimaryExpr : public Expr
    {
    public:
        PrimaryExpr(Token value);

        Token value;
    };
    class AssignExpr : public Expr
    {
    public:
        AssignExpr(std::unique_ptr<Expr> assignee, std::unique_ptr<Expr> value);

        std::unique_ptr<Expr> assignee;
        std::unique_ptr<Expr> value;
    };
    class CallExpr : public Expr
    {
    public:
        CallExpr(std::unique_ptr<Expr> func, std::unique_ptr<Arg> args);

        std::unique_ptr<Expr> func;
        std::unique_ptr<Arg> args;
    };
    class LtoRVExpr : public Expr
    {
    public:
        LtoRVExpr(std::unique_ptr<Expr> val);

        std::unique_ptr<Expr> val;
    };
    class BlockStmt : public Stmt
    {
    public:
        BlockStmt(std::vector<Stmt> &stmts);

        std::vector<Stmt> stmts;
    };
    class ExprStmt : public Stmt
    {
    public:
        ExprStmt(std::unique_ptr<Expr> expr);

        std::unique_ptr<Expr> expr;
    };
    class ReturnStmt : public Stmt
    {
    public:
        ReturnStmt(std::unique_ptr<Expr> val);

        std::unique_ptr<Expr> val;
    };
    class VarStmt : public Stmt
    {
    public:
        VarStmt(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value);

        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Expr> value;
    };
    class VarRef : public Expr
    {
    public:
        VarRef(Token var);

        Token var;
    };
    class BaseType : public Type
    {
    public:
        BaseType(Token type);

        Token type;
    };
    class FunctionDecl : public Decl
    {
    public:
        FunctionDecl(std::unique_ptr<Type> type, Token name, std::unique_ptr<Param> params, std::unique_ptr<BlockStmt> block);

        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Param> params;
        std::unique_ptr<BlockStmt> block;
        bool funcDeclAnValid;
        FuncDeclAn funcDeclAn;
    };
    class GlobalVarDecl : public Decl
    {
    public:
        GlobalVarDecl(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value);

        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Expr> value;
    };
    class Param
    {
    public:
        Param(std::unique_ptr<Type> type, Token name, std::unique_ptr<Param> next);

        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Param> next;
    };
    class Arg
    {
    public:
        Arg(std::unique_ptr<Expr> value, std::unique_ptr<Arg> next);

        std::unique_ptr<Expr> value;
        std::unique_ptr<Arg> next;
    };
// ASTHEADER END
}
