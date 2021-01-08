#pragma once

#include <memory>
#include <vector>
#include "lex/token.h"
#include "utils/location.h"

namespace ASTNS {
// ASTHEADER START
// The following code was autogenerated - see the utils/ directory
    class AST;
    class CUB;
    class Decl;
    class ImplItem;
    class Stmt;
    class Expr;
    class Type;
    class ArgB;
    class ParamB;
    class VStmtIB;
    class PathB;
    class ListB;
    class DeclList;
    class StmtList;
    class ParamList;
    class ArgList;
    class VarStmtItemList;
    class ImplItemList;
    class PureLocationB;
    class PureLocation;
    class ImplicitDecl;
    class CU;
    class ImplDecl;
    class FunctionDecl;
    class FunctionImplItem;
    class VarStmt;
    class VarStmtItem;
    class ExprStmt;
    class RetStmt;
    class PathType;
    class PointerType;
    class Arg;
    class Param;
    class Block;
    class IfExpr;
    class ForExpr;
    class AssignmentExpr;
    class ShortCircuitExpr;
    class BinaryExpr;
    class CastExpr;
    class UnaryExpr;
    class AddrofExpr;
    class DerefExpr;
    class CallExpr;
    class PrimaryExpr;
    class PathExpr;
    class Path;
    class AST {
    public:
        AST(File const &file);
        virtual ~AST() {}
        virtual Location const & start() = 0;
        virtual Location const & end() = 0;
        File const &file;
    };
    class CUB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitCU(ASTNS::CU *ast) = 0;
        };
        virtual ~CUB() {}
        virtual void accept(Visitor *v) = 0;
        CUB(File const &file);
    };
    class Decl : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitImplicitDecl(ASTNS::ImplicitDecl *ast) = 0;
            virtual void visitImplDecl(ASTNS::ImplDecl *ast) = 0;
            virtual void visitFunctionDecl(ASTNS::FunctionDecl *ast) = 0;
        };
        virtual ~Decl() {}
        virtual void accept(Visitor *v) = 0;
        Decl(File const &file);
    };
    class ImplItem : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitFunctionImplItem(ASTNS::FunctionImplItem *ast) = 0;
        };
        virtual ~ImplItem() {}
        virtual void accept(Visitor *v) = 0;
        ImplItem(File const &file);
    };
    class Stmt : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitVarStmt(ASTNS::VarStmt *ast) = 0;
            virtual void visitExprStmt(ASTNS::ExprStmt *ast) = 0;
            virtual void visitRetStmt(ASTNS::RetStmt *ast) = 0;
        };
        virtual ~Stmt() {}
        virtual void accept(Visitor *v) = 0;
        Stmt(File const &file);
    };
    class Expr : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitBlock(ASTNS::Block *ast) = 0;
            virtual void visitIfExpr(ASTNS::IfExpr *ast) = 0;
            virtual void visitForExpr(ASTNS::ForExpr *ast) = 0;
            virtual void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) = 0;
            virtual void visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast) = 0;
            virtual void visitBinaryExpr(ASTNS::BinaryExpr *ast) = 0;
            virtual void visitCastExpr(ASTNS::CastExpr *ast) = 0;
            virtual void visitUnaryExpr(ASTNS::UnaryExpr *ast) = 0;
            virtual void visitAddrofExpr(ASTNS::AddrofExpr *ast) = 0;
            virtual void visitDerefExpr(ASTNS::DerefExpr *ast) = 0;
            virtual void visitCallExpr(ASTNS::CallExpr *ast) = 0;
            virtual void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) = 0;
            virtual void visitPathExpr(ASTNS::PathExpr *ast) = 0;
        };
        virtual ~Expr() {}
        virtual void accept(Visitor *v) = 0;
        Expr(File const &file);
    };
    class Type : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitPathType(ASTNS::PathType *ast) = 0;
            virtual void visitPointerType(ASTNS::PointerType *ast) = 0;
        };
        virtual ~Type() {}
        virtual void accept(Visitor *v) = 0;
        Type(File const &file);
    };
    class ArgB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitArg(ASTNS::Arg *ast) = 0;
        };
        virtual ~ArgB() {}
        virtual void accept(Visitor *v) = 0;
        ArgB(File const &file);
    };
    class ParamB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitParam(ASTNS::Param *ast) = 0;
        };
        virtual ~ParamB() {}
        virtual void accept(Visitor *v) = 0;
        ParamB(File const &file);
    };
    class VStmtIB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitVarStmtItem(ASTNS::VarStmtItem *ast) = 0;
        };
        virtual ~VStmtIB() {}
        virtual void accept(Visitor *v) = 0;
        VStmtIB(File const &file);
    };
    class PathB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitPath(ASTNS::Path *ast) = 0;
        };
        virtual ~PathB() {}
        virtual void accept(Visitor *v) = 0;
        PathB(File const &file);
    };
    class ListB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitDeclList(ASTNS::DeclList *ast) = 0;
            virtual void visitStmtList(ASTNS::StmtList *ast) = 0;
            virtual void visitParamList(ASTNS::ParamList *ast) = 0;
            virtual void visitArgList(ASTNS::ArgList *ast) = 0;
            virtual void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) = 0;
            virtual void visitImplItemList(ASTNS::ImplItemList *ast) = 0;
        };
        virtual ~ListB() {}
        virtual void accept(Visitor *v) = 0;
        ListB(File const &file);
    };
    class DeclList : public ListB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(ASTNS::ListB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        DeclList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Decl>> decls);
    };
    class StmtList : public ListB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ASTNS::ListB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        StmtList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class ParamList : public ListB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<Param>> params;
        virtual void accept(ASTNS::ListB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ParamList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Param>> params);
    };
    class ArgList : public ListB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ASTNS::ListB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ArgList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Arg>> args);
    };
    class VarStmtItemList : public ListB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(ASTNS::ListB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        VarStmtItemList(File const &file, Location start, Location end, std::vector<std::unique_ptr<VarStmtItem>> items);
    };
    class ImplItemList : public ListB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<ImplItem>> items;
        virtual void accept(ASTNS::ListB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ImplItemList(File const &file, Location start, Location end, std::vector<std::unique_ptr<ImplItem>> items);
    };
    class PureLocationB : public AST {
    public:
        class Visitor {
        public:
            virtual ~Visitor() {}
            virtual void visitPureLocation(ASTNS::PureLocation *ast) = 0;
        };
        virtual ~PureLocationB() {}
        virtual void accept(Visitor *v) = 0;
        PureLocationB(File const &file);
    };
    class PureLocation : public PureLocationB {
    public:
        Location _start, _end;
        int dummy;
        virtual void accept(ASTNS::PureLocationB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PureLocation(File const &file, Location start, Location end, int dummy);
    };
    class ImplicitDecl : public Decl {
    public:
        Location _start, _end;
        int dummy;
        virtual void accept(ASTNS::Decl::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ImplicitDecl(File const &file, Location start, Location end, int dummy);
    };
    class CU : public CUB {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(ASTNS::CUB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        CU(File const &file, Location start, Location end, std::vector<std::unique_ptr<Decl>> decls);
    };
    class ImplDecl : public Decl {
    public:
        Location _start, _end;
        std::unique_ptr<Type> implFor;
        std::vector<std::unique_ptr<ImplItem>> items;
        virtual void accept(ASTNS::Decl::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ImplDecl(File const &file, Location start, Location end, std::unique_ptr<Type> implFor, std::vector<std::unique_ptr<ImplItem>> items);
    };
    class FunctionDecl : public Decl {
    public:
        Location _start, _end;
        std::unique_ptr<Type> retty;
        Token name;
        std::vector<std::unique_ptr<Param>> params;
        std::unique_ptr<Block> body;
        virtual void accept(ASTNS::Decl::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        FunctionDecl(File const &file, Location start, Location end, std::unique_ptr<Type> retty, Token name, std::vector<std::unique_ptr<Param>> params, std::unique_ptr<Block> body);
    };
    class FunctionImplItem : public ImplItem {
    public:
        Location _start, _end;
        std::unique_ptr<FunctionDecl> fun;
        virtual void accept(ASTNS::ImplItem::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        FunctionImplItem(File const &file, Location start, Location end, std::unique_ptr<FunctionDecl> fun);
    };
    class VarStmt : public Stmt {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        VarStmt(File const &file, Location start, Location end, std::vector<std::unique_ptr<VarStmtItem>> items);
    };
    class VarStmtItem : public VStmtIB {
    public:
        Location _start, _end;
        std::unique_ptr<Type> type;
        bool mut;
        Token name;
        Token equal;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::VStmtIB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        VarStmtItem(File const &file, Location start, Location end, std::unique_ptr<Type> type, bool mut, Token name, Token equal, std::unique_ptr<Expr> expr);
    };
    class ExprStmt : public Stmt {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> expr;
        bool suppress;
        Location dot;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ExprStmt(File const &file, Location start, Location end, std::unique_ptr<Expr> expr, bool suppress, Location dot);
    };
    class RetStmt : public Stmt {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        RetStmt(File const &file, Location start, Location end, std::unique_ptr<Expr> expr);
    };
    class PathType : public Type {
    public:
        Location _start, _end;
        std::unique_ptr<Path> path;
        virtual void accept(ASTNS::Type::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PathType(File const &file, Location start, Location end, std::unique_ptr<Path> path);
    };
    class PointerType : public Type {
    public:
        Location _start, _end;
        bool mut;
        std::unique_ptr<Type> type;
        virtual void accept(ASTNS::Type::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PointerType(File const &file, Location start, Location end, bool mut, std::unique_ptr<Type> type);
    };
    class Arg : public ArgB {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::ArgB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Arg(File const &file, Location start, Location end, std::unique_ptr<Expr> expr);
    };
    class Param : public ParamB {
    public:
        Location _start, _end;
        std::unique_ptr<Type> type;
        Token name;
        bool mut;
        virtual void accept(ASTNS::ParamB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Param(File const &file, Location start, Location end, std::unique_ptr<Type> type, Token name, bool mut);
    };
    class Block : public Expr {
    public:
        Location _start, _end;
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Block(File const &file, Location start, Location end, std::vector<std::unique_ptr<Stmt>> stmts);
    };
    class IfExpr : public Expr {
    public:
        Location _start, _end;
        Token iftok;
        Token elsetok;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        IfExpr(File const &file, Location start, Location end, Token iftok, Token elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
    };
    class ForExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<VarStmt> initial;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> increment;
        std::unique_ptr<Expr> body;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ForExpr(File const &file, Location start, Location end, std::unique_ptr<VarStmt> initial, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> increment, std::unique_ptr<Expr> body);
    };
    class AssignmentExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> target;
        Token equal;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        AssignmentExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> target, Token equal, std::unique_ptr<Expr> expr);
    };
    class ShortCircuitExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ShortCircuitExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
    };
    class BinaryExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        BinaryExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
    };
    class CastExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<Type> type;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        CastExpr(File const &file, Location start, Location end, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr);
    };
    class UnaryExpr : public Expr {
    public:
        Location _start, _end;
        Token op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        UnaryExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr);
    };
    class AddrofExpr : public Expr {
    public:
        Location _start, _end;
        Token op;
        std::unique_ptr<Expr> expr;
        bool mut;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        AddrofExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr, bool mut);
    };
    class DerefExpr : public Expr {
    public:
        Location _start, _end;
        Token op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        DerefExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr);
    };
    class CallExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<Expr> callee;
        Token oparn;
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        CallExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> callee, Token oparn, std::vector<std::unique_ptr<Arg>> args);
    };
    class PrimaryExpr : public Expr {
    public:
        Location _start, _end;
        Token value;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PrimaryExpr(File const &file, Location start, Location end, Token value);
    };
    class PathExpr : public Expr {
    public:
        Location _start, _end;
        std::unique_ptr<Path> path;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PathExpr(File const &file, Location start, Location end, std::unique_ptr<Path> path);
    };
    class Path : public PathB {
    public:
        Location _start, _end;
        std::vector<Token> segments;
        virtual void accept(ASTNS::PathB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Path(File const &file, Location start, Location end, std::vector<Token> segments);
    };
// This code was autogenerated - see the utils/ directory
// ASTHEADER END
}
