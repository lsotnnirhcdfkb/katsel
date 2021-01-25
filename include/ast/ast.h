#pragma once

#include <memory>
#include <vector>
#include "lex/token.h"
#include "ast/astfwd.h"
#include "ast/visitor.h"
#include "utils/location.h"
#include "utils/ptr.h"
#include "utils/maybe.h"

namespace ASTNS {
// ASTHEADER START
    class AST {
    public:
        AST(File const &file);
        virtual ~AST() {}
        virtual Maybe<Location const> const &start() const = 0;
        virtual Maybe<Location const> const &end() const = 0;
        File const &file;
    };
    class CUB : public AST {
    public:
        virtual ~CUB() {}
        virtual void accept(CUBVisitor &v) = 0;
        CUB(File const &file);
    };
    class Decl : public AST {
    public:
        virtual ~Decl() {}
        virtual void accept(DeclVisitor &v) = 0;
        Decl(File const &file);
    };
    class ImplMember : public AST {
    public:
        virtual ~ImplMember() {}
        virtual void accept(ImplMemberVisitor &v) = 0;
        ImplMember(File const &file);
    };
    class Stmt : public AST {
    public:
        virtual ~Stmt() {}
        virtual void accept(StmtVisitor &v) = 0;
        Stmt(File const &file);
    };
    class Expr : public AST {
    public:
        virtual ~Expr() {}
        virtual void accept(ExprVisitor &v) = 0;
        Expr(File const &file);
    };
    class Type : public AST {
    public:
        virtual ~Type() {}
        virtual void accept(TypeVisitor &v) = 0;
        Type(File const &file);
    };
    class ArgB : public AST {
    public:
        virtual ~ArgB() {}
        virtual void accept(ArgBVisitor &v) = 0;
        ArgB(File const &file);
    };
    class ParamB : public AST {
    public:
        virtual ~ParamB() {}
        virtual void accept(ParamBVisitor &v) = 0;
        ParamB(File const &file);
    };
    class VStmtIB : public AST {
    public:
        virtual ~VStmtIB() {}
        virtual void accept(VStmtIBVisitor &v) = 0;
        VStmtIB(File const &file);
    };
    class PathB : public AST {
    public:
        virtual ~PathB() {}
        virtual void accept(PathBVisitor &v) = 0;
        PathB(File const &file);
    };
    class ListB : public AST {
    public:
        virtual ~ListB() {}
        virtual void accept(ListBVisitor &v) = 0;
        ListB(File const &file);
    };
    class DeclList : public ListB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        DeclList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Decl>> decls);
    };
    class StmtList : public ListB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        StmtList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class ParamList : public ListB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<ParamB>> params;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ParamList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<ParamB>> params);
    };
    class ArgList : public ListB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ArgList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Arg>> args);
    };
    class VarStmtItemList : public ListB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        VarStmtItemList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<VarStmtItem>> items);
    };
    class ImplMemberList : public ListB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<ImplMember>> members;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ImplMemberList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<ImplMember>> members);
    };
    class PureLocationB : public AST {
    public:
        virtual ~PureLocationB() {}
        virtual void accept(PureLocationBVisitor &v) = 0;
        PureLocationB(File const &file);
    };
    class PureLocation : public PureLocationB {
    public:
        Maybe<Location const> _start, _end;
        int dummy;
        virtual void accept(PureLocationBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        PureLocation(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, int dummy);
    };
    class ImplicitDecl : public Decl {
    public:
        Maybe<Location const> _start, _end;
        int dummy;
        virtual void accept(DeclVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ImplicitDecl(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, int dummy);
    };
    class CU : public CUB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(CUBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        CU(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Decl>> decls);
    };
    class ImplDecl : public Decl {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Type> impl_for;
        std::vector<std::unique_ptr<ImplMember>> members;
        virtual void accept(DeclVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ImplDecl(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members);
    };
    class FunctionDecl : public Decl {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Type> retty;
        Token name;
        std::vector<std::unique_ptr<ParamB>> params;
        std::unique_ptr<Block> body;
        virtual void accept(DeclVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        FunctionDecl(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> retty, Token name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body);
    };
    class FunctionImplMember : public ImplMember {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<FunctionDecl> fun;
        virtual void accept(ImplMemberVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        FunctionImplMember(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<FunctionDecl> fun);
    };
    class VarStmt : public Stmt {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(StmtVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        VarStmt(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<VarStmtItem>> items);
    };
    class VarStmtItem : public VStmtIB {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Type> type;
        bool mut;
        Token name;
        Token equal;
        std::unique_ptr<Expr> expr;
        virtual void accept(VStmtIBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        VarStmtItem(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> type, bool mut, Token name, Token equal, std::unique_ptr<Expr> expr);
    };
    class ExprStmt : public Stmt {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> expr;
        bool suppress;
        Maybe<Location const> dot;
        virtual void accept(StmtVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ExprStmt(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> expr, bool suppress, Maybe<Location const> dot);
    };
    class RetStmt : public Stmt {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> expr;
        virtual void accept(StmtVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        RetStmt(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> expr);
    };
    class PathType : public Type {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Path> path;
        virtual void accept(TypeVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        PathType(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Path> path);
    };
    class PointerType : public Type {
    public:
        Maybe<Location const> _start, _end;
        bool mut;
        std::unique_ptr<Type> type;
        virtual void accept(TypeVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        PointerType(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, bool mut, std::unique_ptr<Type> type);
    };
    class ThisType : public Type {
    public:
        Maybe<Location const> _start, _end;
        Token th;
        virtual void accept(TypeVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ThisType(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token th);
    };
    class Arg : public ArgB {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> expr;
        virtual void accept(ArgBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        Arg(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> expr);
    };
    class Param : public ParamB {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Type> type;
        Token name;
        bool mut;
        virtual void accept(ParamBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        Param(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> type, Token name, bool mut);
    };
    class ThisParam : public ParamB {
    public:
        Maybe<Location const> _start, _end;
        bool ptr;
        bool mut;
        virtual void accept(ParamBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ThisParam(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, bool ptr, bool mut);
    };
    class Block : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        Block(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class IfExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        Token iftok;
        Token elsetok;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        IfExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token iftok, Token elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
    };
    class WhileExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> body;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        WhileExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body);
    };
    class AssignmentExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> target;
        Token equal;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        AssignmentExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> target, Token equal, std::unique_ptr<Expr> expr);
    };
    class ShortCircuitExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        ShortCircuitExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
    };
    class BinaryExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        BinaryExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
    };
    class CastExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        CastExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr);
    };
    class UnaryExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        Token op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        UnaryExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token op, std::unique_ptr<Expr> expr);
    };
    class AddrofExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        Token op;
        std::unique_ptr<Expr> expr;
        bool mut;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        AddrofExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token op, std::unique_ptr<Expr> expr, bool mut);
    };
    class DerefExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        Token op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        DerefExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token op, std::unique_ptr<Expr> expr);
    };
    class CallExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> callee;
        Token oparn;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        CallExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> callee, Token oparn, std::vector<std::unique_ptr<Arg>> args);
    };
    class FieldAccessExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> operand;
        Token dot;
        Token field;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        FieldAccessExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> operand, Token dot, Token field);
    };
    class MethodCallExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Expr> operand;
        Token dot;
        Token method;
        Token oparn;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        MethodCallExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> operand, Token dot, Token method, Token oparn, std::vector<std::unique_ptr<Arg>> args);
    };
    class PrimaryExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        Token value;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        PrimaryExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token value);
    };
    class PathExpr : public Expr {
    public:
        Maybe<Location const> _start, _end;
        std::unique_ptr<Path> path;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        PathExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Path> path);
    };
    class Path : public PathB {
    public:
        Maybe<Location const> _start, _end;
        std::vector<Token> segments;
        virtual void accept(PathBVisitor &v) override;
        virtual Maybe<Location const> const &start() const override;
        virtual Maybe<Location const> const &end() const override;
        Path(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<Token> segments);
    };
// ASTHEADER END
}
