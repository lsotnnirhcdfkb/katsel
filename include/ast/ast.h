#pragma once

#include <memory>
#include <vector>
#include "lex/tokens.h"
#include "ast/astfwd.h"
#include "ast/visitor.h"
#include "utils/location.h"
#include "utils/ptr.h"
#include "utils/maybe.h"

namespace ASTNS {
    // ASTHEADER START
    class AST {
    public:
        virtual ~AST() {}
        virtual Span const &span() const = 0;
    };
    class CUB : public AST {
    public:
        virtual ~CUB() {}
        virtual void ast_accept(CUBVisitor &v) = 0;
    };
    class Decl : public AST {
    public:
        virtual ~Decl() {}
        virtual void ast_accept(DeclVisitor &v) = 0;
    };
    class ImplMember : public AST {
    public:
        virtual ~ImplMember() {}
        virtual void ast_accept(ImplMemberVisitor &v) = 0;
    };
    class Stmt : public AST {
    public:
        virtual ~Stmt() {}
        virtual void ast_accept(StmtVisitor &v) = 0;
    };
    class Expr : public AST {
    public:
        virtual ~Expr() {}
        virtual void ast_accept(ExprVisitor &v) = 0;
    };
    class Type : public AST {
    public:
        virtual ~Type() {}
        virtual void ast_accept(TypeVisitor &v) = 0;
    };
    class ParamB : public AST {
    public:
        virtual ~ParamB() {}
        virtual void ast_accept(ParamBVisitor &v) = 0;
    };
    class VStmtIB : public AST {
    public:
        virtual ~VStmtIB() {}
        virtual void ast_accept(VStmtIBVisitor &v) = 0;
    };
    class PathB : public AST {
    public:
        virtual ~PathB() {}
        virtual void ast_accept(PathBVisitor &v) = 0;
    };
    class CU : public CUB {
    public:
        Span const _span;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void ast_accept(CUBVisitor &v) override;
        virtual Span const &span() const override;
        CU(Span const &span, std::vector<std::unique_ptr<Decl>> decls);
    };
    class ImplDecl : public Decl {
    public:
        Span const _span;
        std::unique_ptr<Type> impl_for;
        std::vector<std::unique_ptr<ImplMember>> members;
        virtual void ast_accept(DeclVisitor &v) override;
        virtual Span const &span() const override;
        ImplDecl(Span const &span, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members);
    };
    class FunctionDecl : public Decl {
    public:
        Span const _span;
        std::unique_ptr<Type> retty;
        Located<Tokens::Identifier> name;
        std::vector<std::unique_ptr<ParamB>> params;
        std::unique_ptr<Block> body;
        virtual void ast_accept(DeclVisitor &v) override;
        virtual Span const &span() const override;
        FunctionDecl(Span const &span, std::unique_ptr<Type> retty, Located<Tokens::Identifier> name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body);
    };
    class FunctionImplMember : public ImplMember {
    public:
        Span const _span;
        std::unique_ptr<FunctionDecl> fun;
        virtual void ast_accept(ImplMemberVisitor &v) override;
        virtual Span const &span() const override;
        FunctionImplMember(Span const &span, std::unique_ptr<FunctionDecl> fun);
    };
    class VarStmt : public Stmt {
    public:
        Span const _span;
        std::unique_ptr<Type> type;
        bool mut;
        Located<Tokens::Identifier> name;
        Maybe<Located<Tokens::Equal>> equal;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(StmtVisitor &v) override;
        virtual Span const &span() const override;
        VarStmt(Span const &span, std::unique_ptr<Type> type, bool mut, Located<Tokens::Identifier> name, Maybe<Located<Tokens::Equal>> equal, std::unique_ptr<Expr> expr);
    };
    class ExprStmt : public Stmt {
    public:
        Span const _span;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(StmtVisitor &v) override;
        virtual Span const &span() const override;
        ExprStmt(Span const &span, std::unique_ptr<Expr> expr);
    };
    class RetStmt : public Stmt {
    public:
        Span const _span;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(StmtVisitor &v) override;
        virtual Span const &span() const override;
        RetStmt(Span const &span, std::unique_ptr<Expr> expr);
    };
    class PathType : public Type {
    public:
        Span const _span;
        std::unique_ptr<Path> path;
        virtual void ast_accept(TypeVisitor &v) override;
        virtual Span const &span() const override;
        PathType(Span const &span, std::unique_ptr<Path> path);
    };
    class PointerType : public Type {
    public:
        Span const _span;
        bool mut;
        std::unique_ptr<Type> type;
        virtual void ast_accept(TypeVisitor &v) override;
        virtual Span const &span() const override;
        PointerType(Span const &span, bool mut, std::unique_ptr<Type> type);
    };
    class ThisType : public Type {
    public:
        Span const _span;
        Located<Tokens::This> th;
        virtual void ast_accept(TypeVisitor &v) override;
        virtual Span const &span() const override;
        ThisType(Span const &span, Located<Tokens::This> th);
    };
    class Param : public ParamB {
    public:
        Span const _span;
        std::unique_ptr<Type> type;
        Located<Tokens::Identifier> name;
        bool mut;
        virtual void ast_accept(ParamBVisitor &v) override;
        virtual Span const &span() const override;
        Param(Span const &span, std::unique_ptr<Type> type, Located<Tokens::Identifier> name, bool mut);
    };
    class ThisParam : public ParamB {
    public:
        Span const _span;
        bool ptr;
        bool mut;
        virtual void ast_accept(ParamBVisitor &v) override;
        virtual Span const &span() const override;
        ThisParam(Span const &span, bool ptr, bool mut);
    };
    class Block : public Expr {
    public:
        Span const _span;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        Block(Span const &span, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class IfExpr : public Expr {
    public:
        Span const _span;
        Located<Tokens::If> iftok;
        Maybe<Located<Tokens::Else>> elsetok;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        IfExpr(Span const &span, Located<Tokens::If> iftok, Maybe<Located<Tokens::Else>> elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
    };
    class WhileExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> body;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        WhileExpr(Span const &span, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body);
    };
    class AssignmentExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> target;
        Located<AssignOperator> equal;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        AssignmentExpr(Span const &span, std::unique_ptr<Expr> target, Located<AssignOperator> equal, std::unique_ptr<Expr> expr);
    };
    class ShortCircuitExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> lhs;
        Located<ShortCircuitOperator> op;
        std::unique_ptr<Expr> rhs;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        ShortCircuitExpr(Span const &span, std::unique_ptr<Expr> lhs, Located<ShortCircuitOperator> op, std::unique_ptr<Expr> rhs);
    };
    class BinaryExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> lhs;
        Located<BinaryOperator> op;
        std::unique_ptr<Expr> rhs;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        BinaryExpr(Span const &span, std::unique_ptr<Expr> lhs, Located<BinaryOperator> op, std::unique_ptr<Expr> rhs);
    };
    class CastExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        CastExpr(Span const &span, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr);
    };
    class UnaryExpr : public Expr {
    public:
        Span const _span;
        Located<UnaryOperator> op;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        UnaryExpr(Span const &span, Located<UnaryOperator> op, std::unique_ptr<Expr> expr);
    };
    class AddrofExpr : public Expr {
    public:
        Span const _span;
        Located<Tokens::Amper> op;
        std::unique_ptr<Expr> expr;
        bool mut;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        AddrofExpr(Span const &span, Located<Tokens::Amper> op, std::unique_ptr<Expr> expr, bool mut);
    };
    class DerefExpr : public Expr {
    public:
        Span const _span;
        Located<Tokens::Star> op;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        DerefExpr(Span const &span, Located<Tokens::Star> op, std::unique_ptr<Expr> expr);
    };
    class CallExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> callee;
        Located<Tokens::OParen> oparn;
        std::vector<std::unique_ptr<Expr>> args;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        CallExpr(Span const &span, std::unique_ptr<Expr> callee, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Expr>> args);
    };
    class FieldAccessExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> field;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        FieldAccessExpr(Span const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> field);
    };
    class MethodCallExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Expr> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> method;
        Located<Tokens::OParen> oparn;
        std::vector<std::unique_ptr<Expr>> args;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        MethodCallExpr(Span const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> method, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Expr>> args);
    };
    class BoolLit : public Expr {
    public:
        Span const _span;
        Located<Tokens::BoolLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        BoolLit(Span const &span, Located<Tokens::BoolLit> val);
    };
    class FloatLit : public Expr {
    public:
        Span const _span;
        Located<Tokens::FloatLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        FloatLit(Span const &span, Located<Tokens::FloatLit> val);
    };
    class IntLit : public Expr {
    public:
        Span const _span;
        Located<Tokens::IntLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        IntLit(Span const &span, Located<Tokens::IntLit> val);
    };
    class CharLit : public Expr {
    public:
        Span const _span;
        Located<Tokens::CharLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        CharLit(Span const &span, Located<Tokens::CharLit> val);
    };
    class StringLit : public Expr {
    public:
        Span const _span;
        Located<Tokens::StringLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        StringLit(Span const &span, Located<Tokens::StringLit> val);
    };
    class ThisExpr : public Expr {
    public:
        Span const _span;
        Located<Tokens::This> tok;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        ThisExpr(Span const &span, Located<Tokens::This> tok);
    };
    class PathExpr : public Expr {
    public:
        Span const _span;
        std::unique_ptr<Path> path;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Span const &span() const override;
        PathExpr(Span const &span, std::unique_ptr<Path> path);
    };
    class Path : public PathB {
    public:
        Span const _span;
        std::vector<Located<Tokens::Identifier>> segments;
        virtual void ast_accept(PathBVisitor &v) override;
        virtual Span const &span() const override;
        Path(Span const &span, std::vector<Located<Tokens::Identifier>> segments);
    };
    // ASTHEADER END
}
