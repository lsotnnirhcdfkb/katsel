#include <memory>
#include "token.h"

namespace ASTNS
{
// ASTHEADER START
    class Expr;
    class Decl;
    class Type;
    class LValue;
    class Stmt;
    class Program;
    class BinaryExpr;
    class TernaryExpr;
    class UnaryExpr;
    class PrimaryExpr;
    class AssignExpr;
    class CallExpr;
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
    class LValue
    {
    public:
        virtual ~LValue() {}
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
    class BinaryExpr : Expr
    {
    public:
        BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op);

        std::unique_ptr<Expr> lhs;
        std::unique_ptr<Expr> rhs;
        Token op;
    };
    class TernaryExpr : Expr
    {
    public:
        TernaryExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);

        std::unique_ptr<Expr> condition;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
    };
    class UnaryExpr : Expr
    {
    public:
        UnaryExpr(std::unique_ptr<Expr> operand, Token op);

        std::unique_ptr<Expr> operand;
        Token op;
    };
    class PrimaryExpr : Expr
    {
    public:
        PrimaryExpr(Token value);

        Token value;
    };
    class AssignExpr : Expr
    {
    public:
        AssignExpr(std::unique_ptr<LValue> assignee, std::unique_ptr<Expr> value);

        std::unique_ptr<LValue> assignee;
        std::unique_ptr<Expr> value;
    };
    class CallExpr : Expr
    {
    public:
        CallExpr(std::unique_ptr<LValue> func, std::unique_ptr<Arg> args);

        std::unique_ptr<LValue> func;
        std::unique_ptr<Arg> args;
    };
    class BlockStmt : Stmt
    {
    public:
        BlockStmt(std::vector<Stmt> &stmts);

        std::vector<Stmt> stmts;
    };
    class ExprStmt : Stmt
    {
    public:
        ExprStmt(std::unique_ptr<Expr> expr);

        std::unique_ptr<Expr> expr;
    };
    class ReturnStmt : Stmt
    {
    public:
        ReturnStmt(std::unique_ptr<Expr> val);

        std::unique_ptr<Expr> val;
    };
    class VarStmt : Stmt
    {
    public:
        VarStmt(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value);

        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<Expr> value;
    };
    class VarRef : LValue
    {
    public:
        VarRef(Token var);

        Token var;
    };
    class BaseType : Type
    {
    public:
        BaseType(Token type);

        Token type;
    };
    class FunctionDecl : Decl
    {
    public:
        FunctionDecl(std::unique_ptr<Type> type, Token name, std::unique_ptr<BlockStmt> block);

        std::unique_ptr<Type> type;
        Token name;
        std::unique_ptr<BlockStmt> block;
    };
    class GlobalVarDecl : Decl
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
