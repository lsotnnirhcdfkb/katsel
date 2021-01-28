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
        std::vector<Located<std::unique_ptr<Decl>>> decls;
        virtual void accept(ListBVisitor &v) override;
        DeclList(std::vector<Located<std::unique_ptr<Decl>>> decls);
    };
    class StmtList : public ListB {
    public:
        std::vector<Located<std::unique_ptr<Stmt>>> stmts;
        virtual void accept(ListBVisitor &v) override;
        StmtList(std::vector<Located<std::unique_ptr<Stmt>>> stmts);
    };
    class ParamList : public ListB {
    public:
        std::vector<Located<std::unique_ptr<ParamB>>> params;
        virtual void accept(ListBVisitor &v) override;
        ParamList(std::vector<Located<std::unique_ptr<ParamB>>> params);
    };
    class ArgList : public ListB {
    public:
        std::vector<Located<std::unique_ptr<Arg>>> args;
        virtual void accept(ListBVisitor &v) override;
        ArgList(std::vector<Located<std::unique_ptr<Arg>>> args);
    };
    class VarStmtItemList : public ListB {
    public:
        std::vector<Located<std::unique_ptr<VarStmtItem>>> items;
        virtual void accept(ListBVisitor &v) override;
        VarStmtItemList(std::vector<Located<std::unique_ptr<VarStmtItem>>> items);
    };
    class ImplMemberList : public ListB {
    public:
        std::vector<Located<std::unique_ptr<ImplMember>>> members;
        virtual void accept(ListBVisitor &v) override;
        ImplMemberList(std::vector<Located<std::unique_ptr<ImplMember>>> members);
    };
    class PureLocationB : public AST {
    public:
        virtual ~PureLocationB() {}
        virtual void accept(PureLocationBVisitor &v) = 0;
        PureLocationB(File const &file);
    };
    class PureLocation : public PureLocationB {
    public:
        int dummy;
        virtual void accept(PureLocationBVisitor &v) override;
        PureLocation(int dummy);
    };
    class ImplicitDecl : public Decl {
    public:
        int dummy;
        virtual void accept(DeclVisitor &v) override;
        ImplicitDecl(int dummy);
    };
    class CU : public CUB {
    public:
        std::vector<Located<std::unique_ptr<Decl>>> decls;
        virtual void accept(CUBVisitor &v) override;
        CU(std::vector<Located<std::unique_ptr<Decl>>> decls);
    };
    class ImplDecl : public Decl {
    public:
        Located<std::unique_ptr<Type>> impl_for;
        std::vector<Located<std::unique_ptr<ImplMember>>> members;
        virtual void accept(DeclVisitor &v) override;
        ImplDecl(Located<std::unique_ptr<Type>> impl_for, std::vector<Located<std::unique_ptr<ImplMember>>> members);
    };
    class FunctionDecl : public Decl {
    public:
        Located<std::unique_ptr<Type>> retty;
        Located<Tokens::Identifier> name;
        std::vector<Located<std::unique_ptr<ParamB>>> params;
        Located<std::unique_ptr<Block>> body;
        virtual void accept(DeclVisitor &v) override;
        FunctionDecl(Located<std::unique_ptr<Type>> retty, Located<Tokens::Identifier> name, std::vector<Located<std::unique_ptr<ParamB>>> params, Located<std::unique_ptr<Block>> body);
    };
    class FunctionImplMember : public ImplMember {
    public:
        Located<std::unique_ptr<FunctionDecl>> fun;
        virtual void accept(ImplMemberVisitor &v) override;
        FunctionImplMember(Located<std::unique_ptr<FunctionDecl>> fun);
    };
    class VarStmt : public Stmt {
    public:
        std::vector<Located<std::unique_ptr<VarStmtItem>>> items;
        virtual void accept(StmtVisitor &v) override;
        VarStmt(std::vector<Located<std::unique_ptr<VarStmtItem>>> items);
    };
    class VarStmtItem : public VStmtIB {
    public:
        Located<std::unique_ptr<Type>> type;
        bool mut;
        Located<Tokens::Identifier> name;
        Located<Tokens::Equal> equal;
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(VStmtIBVisitor &v) override;
        VarStmtItem(Located<std::unique_ptr<Type>> type, bool mut, Located<Tokens::Identifier> name, Located<Tokens::Equal> equal, Located<std::unique_ptr<Expr>> expr);
    };
    class ExprStmt : public Stmt {
    public:
        Located<std::unique_ptr<Expr>> expr;
        bool suppress;
        Maybe<Span const> dot;
        virtual void accept(StmtVisitor &v) override;
        ExprStmt(Located<std::unique_ptr<Expr>> expr, bool suppress, Maybe<Span const> dot);
    };
    class RetStmt : public Stmt {
    public:
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(StmtVisitor &v) override;
        RetStmt(Located<std::unique_ptr<Expr>> expr);
    };
    class PathType : public Type {
    public:
        Located<std::unique_ptr<Path>> path;
        virtual void accept(TypeVisitor &v) override;
        PathType(Located<std::unique_ptr<Path>> path);
    };
    class PointerType : public Type {
    public:
        bool mut;
        Located<std::unique_ptr<Type>> type;
        virtual void accept(TypeVisitor &v) override;
        PointerType(bool mut, Located<std::unique_ptr<Type>> type);
    };
    class ThisType : public Type {
    public:
        Located<Tokens::This> th;
        virtual void accept(TypeVisitor &v) override;
        ThisType(Located<Tokens::This> th);
    };
    class Arg : public ArgB {
    public:
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(ArgBVisitor &v) override;
        Arg(Located<std::unique_ptr<Expr>> expr);
    };
    class Param : public ParamB {
    public:
        Located<std::unique_ptr<Type>> type;
        Located<Tokens::Identifier> name;
        bool mut;
        virtual void accept(ParamBVisitor &v) override;
        Param(Located<std::unique_ptr<Type>> type, Located<Tokens::Identifier> name, bool mut);
    };
    class ThisParam : public ParamB {
    public:
        bool ptr;
        bool mut;
        virtual void accept(ParamBVisitor &v) override;
        ThisParam(bool ptr, bool mut);
    };
    class Block : public Expr {
    public:
        std::vector<Located<std::unique_ptr<Stmt>>> stmts;
        virtual void accept(ExprVisitor &v) override;
        Block(std::vector<Located<std::unique_ptr<Stmt>>> stmts);
    };
    class IfExpr : public Expr {
    public:
        Located<Tokens::If> iftok;
        Maybe<Located<Tokens::Else>> elsetok;
        Located<std::unique_ptr<Expr>> cond;
        Located<std::unique_ptr<Expr>> trues;
        Located<std::unique_ptr<Expr>> falses;
        virtual void accept(ExprVisitor &v) override;
        IfExpr(Located<Tokens::If> iftok, Maybe<Located<Tokens::Else>> elsetok, Located<std::unique_ptr<Expr>> cond, Located<std::unique_ptr<Expr>> trues, Located<std::unique_ptr<Expr>> falses);
    };
    class WhileExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> cond;
        Located<std::unique_ptr<Expr>> body;
        virtual void accept(ExprVisitor &v) override;
        WhileExpr(Located<std::unique_ptr<Expr>> cond, Located<std::unique_ptr<Expr>> body);
    };
    class AssignmentExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> target;
        Located<AssignOperator> equal;
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(ExprVisitor &v) override;
        AssignmentExpr(Located<std::unique_ptr<Expr>> target, Located<AssignOperator> equal, Located<std::unique_ptr<Expr>> expr);
    };
    class ShortCircuitExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> lhs;
        Located<ShortCircuitOperator> op;
        Located<std::unique_ptr<Expr>> rhs;
        virtual void accept(ExprVisitor &v) override;
        ShortCircuitExpr(Located<std::unique_ptr<Expr>> lhs, Located<ShortCircuitOperator> op, Located<std::unique_ptr<Expr>> rhs);
    };
    class BinaryExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> lhs;
        Located<BinaryOperator> op;
        Located<std::unique_ptr<Expr>> rhs;
        virtual void accept(ExprVisitor &v) override;
        BinaryExpr(Located<std::unique_ptr<Expr>> lhs, Located<BinaryOperator> op, Located<std::unique_ptr<Expr>> rhs);
    };
    class CastExpr : public Expr {
    public:
        Located<std::unique_ptr<Type>> type;
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(ExprVisitor &v) override;
        CastExpr(Located<std::unique_ptr<Type>> type, Located<std::unique_ptr<Expr>> expr);
    };
    class UnaryExpr : public Expr {
    public:
        Located<UnaryOperator> op;
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(ExprVisitor &v) override;
        UnaryExpr(Located<UnaryOperator> op, Located<std::unique_ptr<Expr>> expr);
    };
    class AddrofExpr : public Expr {
    public:
        Located<Tokens::Amper> op;
        Located<std::unique_ptr<Expr>> expr;
        bool mut;
        virtual void accept(ExprVisitor &v) override;
        AddrofExpr(Located<Tokens::Amper> op, Located<std::unique_ptr<Expr>> expr, bool mut);
    };
    class DerefExpr : public Expr {
    public:
        Located<Tokens::Star> op;
        Located<std::unique_ptr<Expr>> expr;
        virtual void accept(ExprVisitor &v) override;
        DerefExpr(Located<Tokens::Star> op, Located<std::unique_ptr<Expr>> expr);
    };
    class CallExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> callee;
        Located<Tokens::OParen> oparn;
        std::vector<Located<std::unique_ptr<Arg>>> args;
        virtual void accept(ExprVisitor &v) override;
        CallExpr(Located<std::unique_ptr<Expr>> callee, Located<Tokens::OParen> oparn, std::vector<Located<std::unique_ptr<Arg>>> args);
    };
    class FieldAccessExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> field;
        virtual void accept(ExprVisitor &v) override;
        FieldAccessExpr(Located<std::unique_ptr<Expr>> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> field);
    };
    class MethodCallExpr : public Expr {
    public:
        Located<std::unique_ptr<Expr>> operand;
        Located<Tokens::Period> dot;
        Located<Tokens::Identifier> method;
        Located<Tokens::OParen> oparn;
        std::vector<Located<std::unique_ptr<Arg>>> args;
        virtual void accept(ExprVisitor &v) override;
        MethodCallExpr(Located<std::unique_ptr<Expr>> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> method, Located<Tokens::OParen> oparn, std::vector<Located<std::unique_ptr<Arg>>> args);
    };
    class BoolLit : public Expr {
    public:
        Located<bool> val;
        virtual void accept(ExprVisitor &v) override;
        BoolLit(Located<bool> val);
    };
    class FloatLit : public Expr {
    public:
        Located<double> val;
        virtual void accept(ExprVisitor &v) override;
        FloatLit(Located<double> val);
    };
    class IntLit : public Expr {
    public:
        Located<uint64_t> val;
        virtual void accept(ExprVisitor &v) override;
        IntLit(Located<uint64_t> val);
    };
    class CharLit : public Expr {
    public:
        Located<char> val;
        virtual void accept(ExprVisitor &v) override;
        CharLit(Located<char> val);
    };
    class StringLit : public Expr {
    public:
        Located<std::string> val;
        virtual void accept(ExprVisitor &v) override;
        StringLit(Located<std::string> val);
    };
    class ThisExpr : public Expr {
    public:
        Span tok;
        virtual void accept(ExprVisitor &v) override;
        ThisExpr(Span tok);
    };
    class PathExpr : public Expr {
    public:
        Located<std::unique_ptr<Path>> path;
        virtual void accept(ExprVisitor &v) override;
        PathExpr(Located<std::unique_ptr<Path>> path);
    };
    class Path : public PathB {
    public:
        std::vector<Located<Tokens::Identifier>> segments;
        virtual void accept(PathBVisitor &v) override;
        Path(std::vector<Located<Tokens::Identifier>> segments);
    };
    // ASTHEADER END
}
