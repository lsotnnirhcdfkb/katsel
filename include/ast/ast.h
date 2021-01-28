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
    enum class AssignOperator {
        EQUAL,
    };
    enum class BinaryOperator {
        PLUS,
        MINUS,
        STAR,
        SLASH,
        PERCENT,
        GREATER,
        LESS,
        GREATEREQUAL,
        LESSEQUAL,
        AMPER,
        PIPE,
        CARET,
        DOUBLEGREATER,
        DOUBLELESS,
        DOUBLEEQUAL,
        BANGEQUAL,
    };
    enum class UnaryOperator {
        BANG,
        TILDE,
        MINUS,
        DOUBLEPLUS,
        DOUBLEMINUS,
    };
    enum class ShortCircuitOperator {
        DOUBLEPIPE,
        DOUBLEAMPER,
    };
    // ASTHEADER START
    class AST {
    public:
        AST(File const &file);
        virtual ~AST() {}
        virtual Maybe<Span const> const &span() const = 0;
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
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        DeclList(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<Decl>> decls);
    };
    class StmtList : public ListB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        StmtList(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class ParamList : public ListB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<ParamB>> params;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ParamList(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<ParamB>> params);
    };
    class ArgList : public ListB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ArgList(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<Arg>> args);
    };
    class VarStmtItemList : public ListB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        VarStmtItemList(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<VarStmtItem>> items);
    };
    class ImplMemberList : public ListB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<ImplMember>> members;
        virtual void accept(ListBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ImplMemberList(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<ImplMember>> members);
    };
    class PureLocationB : public AST {
    public:
        virtual ~PureLocationB() {}
        virtual void accept(PureLocationBVisitor &v) = 0;
        PureLocationB(File const &file);
    };
    class PureLocation : public PureLocationB {
    public:
        Maybe<Span const> _span;
        int dummy;
        virtual void accept(PureLocationBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PureLocation(File const &file, Maybe<Span const> const &span, int dummy);
    };
    class ImplicitDecl : public Decl {
    public:
        Maybe<Span const> _span;
        int dummy;
        virtual void accept(DeclVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ImplicitDecl(File const &file, Maybe<Span const> const &span, int dummy);
    };
    class CU : public CUB {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(CUBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CU(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<Decl>> decls);
    };
    class ImplDecl : public Decl {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> impl_for;
        std::vector<std::unique_ptr<ImplMember>> members;
        virtual void accept(DeclVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ImplDecl(File const &file, Maybe<Span const> const &span, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members);
    };
    class FunctionDecl : public Decl {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> retty;
        Located<Tokens::Identifier> name;
        std::vector<std::unique_ptr<ParamB>> params;
        std::unique_ptr<Block> body;
        virtual void accept(DeclVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FunctionDecl(File const &file, Maybe<Span const> const &span, std::unique_ptr<Type> retty, Located<Tokens::Identifier> name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body);
    };
    class FunctionImplMember : public ImplMember {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<FunctionDecl> fun;
        virtual void accept(ImplMemberVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FunctionImplMember(File const &file, Maybe<Span const> const &span, std::unique_ptr<FunctionDecl> fun);
    };
    class VarStmt : public Stmt {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(StmtVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        VarStmt(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<VarStmtItem>> items);
    };
    class VarStmtItem : public VStmtIB {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> type;
        bool mut;
        Located<Tokens::Identifier> name;
        Located<Tokens::Equal> equal;
        std::unique_ptr<Expr> expr;
        virtual void accept(VStmtIBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        VarStmtItem(File const &file, Maybe<Span const> const &span, std::unique_ptr<Type> type, bool mut, Located<Tokens::Identifier> name, Located<Tokens::Equal> equal, std::unique_ptr<Expr> expr);
    };
    class ExprStmt : public Stmt {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> expr;
        bool suppress;
        Maybe<Span const> dot;
        virtual void accept(StmtVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ExprStmt(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> expr, bool suppress, Maybe<Span const> dot);
    };
    class RetStmt : public Stmt {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> expr;
        virtual void accept(StmtVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        RetStmt(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> expr);
    };
    class PathType : public Type {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Path> path;
        virtual void accept(TypeVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PathType(File const &file, Maybe<Span const> const &span, std::unique_ptr<Path> path);
    };
    class PointerType : public Type {
    public:
        Maybe<Span const> _span;
        bool mut;
        std::unique_ptr<Type> type;
        virtual void accept(TypeVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PointerType(File const &file, Maybe<Span const> const &span, bool mut, std::unique_ptr<Type> type);
    };
    class ThisType : public Type {
    public:
        Maybe<Span const> _span;
        Located<Tokens::This> th;
        virtual void accept(TypeVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ThisType(File const &file, Maybe<Span const> const &span, Located<Tokens::This> th);
    };
    class Arg : public ArgB {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> expr;
        virtual void accept(ArgBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Arg(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> expr);
    };
    class Param : public ParamB {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> type;
        Located<Tokens::Identifier> name;
        bool mut;
        virtual void accept(ParamBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Param(File const &file, Maybe<Span const> const &span, std::unique_ptr<Type> type, Located<Tokens::Identifier> name, bool mut);
    };
    class ThisParam : public ParamB {
    public:
        Maybe<Span const> _span;
        bool ptr;
        bool mut;
        virtual void accept(ParamBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ThisParam(File const &file, Maybe<Span const> const &span, bool ptr, bool mut);
    };
    class Block : public Expr {
    public:
        Maybe<Span const> _span;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Block(File const &file, Maybe<Span const> const &span, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class IfExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::If> iftok;
        Maybe<Located<Tokens::Else>> elsetok;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        IfExpr(File const &file, Maybe<Span const> const &span, Located<Tokens::If> iftok, Maybe<Located<Tokens::Else>> elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
    };
    class WhileExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> body;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        WhileExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body);
    };
    class AssignmentExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> target;
        Located<AssignOperator> equal;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        AssignmentExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> target, Located<AssignOperator> equal, std::unique_ptr<Expr> expr);
    };
    class ShortCircuitExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> lhs;
        Located<ShortCircuitOperator> op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ShortCircuitExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> lhs, Located<ShortCircuitOperator> op, std::unique_ptr<Expr> rhs);
    };
    class BinaryExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> lhs;
        Located<BinaryOperator> op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        BinaryExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> lhs, Located<BinaryOperator> op, std::unique_ptr<Expr> rhs);
    };
    class CastExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CastExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr);
    };
    class UnaryExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<UnaryOperator> op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        UnaryExpr(File const &file, Maybe<Span const> const &span, Located<UnaryOperator> op, std::unique_ptr<Expr> expr);
    };
    class AddrofExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::Amper> op;
        std::unique_ptr<Expr> expr;
        bool mut;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        AddrofExpr(File const &file, Maybe<Span const> const &span, Located<Tokens::Amper> op, std::unique_ptr<Expr> expr, bool mut);
    };
    class DerefExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::Star> op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        DerefExpr(File const &file, Maybe<Span const> const &span, Located<Tokens::Star> op, std::unique_ptr<Expr> expr);
    };
    class CallExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> callee;
        Located<Tokens::OParen> oparn;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CallExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> callee, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Arg>> args);
    };
    class FieldAccessExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> field;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FieldAccessExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> field);
    };
    class MethodCallExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Expr> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> method;
        Located<Tokens::OParen> oparn;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        MethodCallExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> method, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Arg>> args);
    };
    class BoolLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::BoolLit> val;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        BoolLit(File const &file, Maybe<Span const> const &span, Located<Tokens::BoolLit> val);
    };
    class FloatLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::FloatLit> val;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        FloatLit(File const &file, Maybe<Span const> const &span, Located<Tokens::FloatLit> val);
    };
    class IntLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::IntLit> val;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        IntLit(File const &file, Maybe<Span const> const &span, Located<Tokens::IntLit> val);
    };
    class CharLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::CharLit> val;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        CharLit(File const &file, Maybe<Span const> const &span, Located<Tokens::CharLit> val);
    };
    class StringLit : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::StringLit> val;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        StringLit(File const &file, Maybe<Span const> const &span, Located<Tokens::StringLit> val);
    };
    class ThisExpr : public Expr {
    public:
        Maybe<Span const> _span;
        Located<Tokens::This> tok;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        ThisExpr(File const &file, Maybe<Span const> const &span, Located<Tokens::This> tok);
    };
    class PathExpr : public Expr {
    public:
        Maybe<Span const> _span;
        std::unique_ptr<Path> path;
        virtual void accept(ExprVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        PathExpr(File const &file, Maybe<Span const> const &span, std::unique_ptr<Path> path);
    };
    class Path : public PathB {
    public:
        Maybe<Span const> _span;
        std::vector<Located<Tokens::Identifier>> segments;
        virtual void accept(PathBVisitor &v) override;
        virtual Maybe<Span const> const &span() const override;
        Path(File const &file, Maybe<Span const> const &span, std::vector<Located<Tokens::Identifier>> segments);
    };
    // ASTHEADER END
}
