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
        virtual Maybe<Span const> const &span() const = 0;
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
    class PureLocationB : public AST {
    public:
        virtual ~PureLocationB() {}
        virtual void ast_accept(PureLocationBVisitor &v) = 0;
    };
    class PureLocation : public PureLocationB {
    public:
        Maybe<Span const> _span;
        int dummy;
        virtual void ast_accept(PureLocationBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PureLocation(Maybe<Span const> const &span, int dummy);
    };
    class ImplicitDecl : public Decl {
    public:
        Maybe<Span const> _span;
        int dummy;
        virtual void ast_accept(DeclVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ImplicitDecl(Maybe<Span const> const &span, int dummy);
    };
    class CU : public CUB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void ast_accept(CUBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CU(Maybe<Span const> const &span, std::vector<std::unique_ptr<Decl>> decls);
    };
    class ImplDecl : public Decl {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> impl_for;
        std::vector<std::unique_ptr<ImplMember>> members;
        virtual void ast_accept(DeclVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ImplDecl(Maybe<Span const> const &span, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members);
    };
    class FunctionDecl : public Decl {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> retty;
        Located<Tokens::Identifier> name;
        std::vector<std::unique_ptr<ParamB>> params;
        std::unique_ptr<Block> body;
        virtual void ast_accept(DeclVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FunctionDecl(Maybe<Span const> const &span, std::unique_ptr<Type> retty, Located<Tokens::Identifier> name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body);
    };
    class FunctionImplMember : public ImplMember {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<FunctionDecl> fun;
        virtual void ast_accept(ImplMemberVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FunctionImplMember(Maybe<Span const> const &span, std::unique_ptr<FunctionDecl> fun);
    };
    class VarStmt : public Stmt {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> type;
        bool mut;
        Located<Tokens::Identifier> name;
        Maybe<Located<Tokens::Equal>> equal;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(StmtVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        VarStmt(Maybe<Span const> const &span, std::unique_ptr<Type> type, bool mut, Located<Tokens::Identifier> name, Maybe<Located<Tokens::Equal>> equal, std::unique_ptr<Expr> expr);
    };
    class ExprStmt : public Stmt {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(StmtVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ExprStmt(Maybe<Span const> const &span, std::unique_ptr<Expr> expr);
    };
    class RetStmt : public Stmt {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(StmtVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        RetStmt(Maybe<Span const> const &span, std::unique_ptr<Expr> expr);
    };
    class PathType : public Type {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Path> path;
        virtual void ast_accept(TypeVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PathType(Maybe<Span const> const &span, std::unique_ptr<Path> path);
    };
    class PointerType : public Type {
    public:
        Maybe<Span const> _span;
        bool mut;
        std::unique_ptr<Type> type;
        virtual void ast_accept(TypeVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PointerType(Maybe<Span const> const &span, bool mut, std::unique_ptr<Type> type);
    };
    class ThisType : public Type {
    public:
        Maybe<Span const> _span;
        Located<Tokens::This> th;
        virtual void ast_accept(TypeVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ThisType(Maybe<Span const> const &span, Located<Tokens::This> th);
    };
    class Param : public ParamB {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> type;
        Located<Tokens::Identifier> name;
        bool mut;
        virtual void ast_accept(ParamBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Param(Maybe<Span const> const &span, std::unique_ptr<Type> type, Located<Tokens::Identifier> name, bool mut);
    };
    class ThisParam : public ParamB {
    public:
        Maybe<Span const> _span;
        bool ptr;
        bool mut;
        virtual void ast_accept(ParamBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ThisParam(Maybe<Span const> const &span, bool ptr, bool mut);
    };
    class Block : public Expr {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Block(Maybe<Span const> const &span, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class IfExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::If> iftok;
        Maybe<Located<Tokens::Else>> elsetok;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        IfExpr(Maybe<Span const> const &span, Located<Tokens::If> iftok, Maybe<Located<Tokens::Else>> elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
    };
    class WhileExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> body;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        WhileExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body);
    };
    class AssignmentExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> target;
        Located<AssignOperator> equal;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        AssignmentExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> target, Located<AssignOperator> equal, std::unique_ptr<Expr> expr);
    };
    class ShortCircuitExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> lhs;
        Located<ShortCircuitOperator> op;
        std::unique_ptr<Expr> rhs;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ShortCircuitExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> lhs, Located<ShortCircuitOperator> op, std::unique_ptr<Expr> rhs);
    };
    class BinaryExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> lhs;
        Located<BinaryOperator> op;
        std::unique_ptr<Expr> rhs;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        BinaryExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> lhs, Located<BinaryOperator> op, std::unique_ptr<Expr> rhs);
    };
    class CastExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CastExpr(Maybe<Span const> const &span, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr);
    };
    class UnaryExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<UnaryOperator> op;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        UnaryExpr(Maybe<Span const> const &span, Located<UnaryOperator> op, std::unique_ptr<Expr> expr);
    };
    class AddrofExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::Amper> op;
        std::unique_ptr<Expr> expr;
        bool mut;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        AddrofExpr(Maybe<Span const> const &span, Located<Tokens::Amper> op, std::unique_ptr<Expr> expr, bool mut);
    };
    class DerefExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::Star> op;
        std::unique_ptr<Expr> expr;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        DerefExpr(Maybe<Span const> const &span, Located<Tokens::Star> op, std::unique_ptr<Expr> expr);
    };
    class CallExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> callee;
        Located<Tokens::OParen> oparn;
        std::vector<std::unique_ptr<Expr>> args;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CallExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> callee, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Expr>> args);
    };
    class FieldAccessExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> field;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FieldAccessExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> field);
    };
    class MethodCallExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> method;
        Located<Tokens::OParen> oparn;
        std::vector<std::unique_ptr<Expr>> args;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        MethodCallExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> method, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Expr>> args);
    };
    class BoolLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::BoolLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        BoolLit(Maybe<Span const> const &span, Located<Tokens::BoolLit> val);
    };
    class FloatLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::FloatLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FloatLit(Maybe<Span const> const &span, Located<Tokens::FloatLit> val);
    };
    class IntLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::IntLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        IntLit(Maybe<Span const> const &span, Located<Tokens::IntLit> val);
    };
    class CharLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::CharLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CharLit(Maybe<Span const> const &span, Located<Tokens::CharLit> val);
    };
    class StringLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::StringLit> val;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        StringLit(Maybe<Span const> const &span, Located<Tokens::StringLit> val);
    };
    class ThisExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::This> tok;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ThisExpr(Maybe<Span const> const &span, Located<Tokens::This> tok);
    };
    class PathExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Path> path;
        virtual void ast_accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PathExpr(Maybe<Span const> const &span, std::unique_ptr<Path> path);
    };
    class Path : public PathB {
    public:
        Maybe<Span const> _span;
        std::vector<Located<Tokens::Identifier>> segments;
        virtual void ast_accept(PathBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Path(Maybe<Span const> const &span, std::vector<Located<Tokens::Identifier>> segments);
    };
    // ASTHEADER END
}
