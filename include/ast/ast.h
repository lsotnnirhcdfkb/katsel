#pragma once

#include <memory>
#include <vector>
#include "lex/token.h"
#include "utils/location.h"

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
    class ImplRetB;
    class CU;
    class DeclList;
    class FunctionDecl;
    class VarStmt;
    class VarStmtItem;
    class VarStmtItemList;
    class ExprStmt;
    class RetStmt;
    class StmtList;
    class ImplRet;
    class PrimitiveType;
    class Arg;
    class ArgList;
    class Param;
    class ParamList;
    class Block;
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
        inline AST(File const &file): file(file) {}
        virtual ~AST() {}
        virtual Location const & start() = 0;
        virtual Location const & end() = 0;
        File const &file;
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
            virtual void visitDeclList(ASTNS::DeclList *ast) = 0;
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
            virtual void visitStmtList(ASTNS::StmtList *ast) = 0;
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
            virtual void visitBlock(ASTNS::Block *ast) = 0;
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
            virtual void visitArgList(ASTNS::ArgList *ast) = 0;
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
            virtual void visitParamList(ASTNS::ParamList *ast) = 0;
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
            virtual void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) = 0;
        };
        virtual ~VStmtIB() {}
        virtual void accept(Visitor *v) = 0;
    };
    class ImplRetB : public AST
    {
    public:
        class Visitor
        {
        public:
            virtual ~Visitor() {}
            virtual void visitImplRet(ASTNS::ImplRet *ast) = 0;
        };
        virtual ~ImplRetB() {}
        virtual void accept(Visitor *v) = 0;
    };
    class CU : public CUB
    {
    public:
        std::unique_ptr<DeclList> decls;
        virtual void accept(ASTNS::CUB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        CU(File const &file, Location start, Location end, std::unique_ptr<DeclList> decls);
        Location _start, _end;
    };
    class DeclList : public Decl
    {
    public:
        std::vector<std::unique_ptr<Decl>> decls;
        virtual void accept(ASTNS::Decl::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        DeclList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Decl>> decls);
        Location _start, _end;
    };
    class FunctionDecl : public Decl
    {
    public:
        std::unique_ptr<Type> retty;
        Token name;
        std::unique_ptr<ParamList> params;
        std::unique_ptr<Block> body;
        virtual void accept(ASTNS::Decl::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        FunctionDecl(File const &file, Location start, Location end, std::unique_ptr<Type> retty, Token name, std::unique_ptr<ParamList> params, std::unique_ptr<Block> body);
        Location _start, _end;
    };
    class VarStmt : public Stmt
    {
    public:
        std::unique_ptr<Type> type;
        std::unique_ptr<VarStmtItemList> assignments;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        VarStmt(File const &file, Location start, Location end, std::unique_ptr<Type> type, std::unique_ptr<VarStmtItemList> assignments);
        Location _start, _end;
    };
    class VarStmtItem : public VStmtIB
    {
    public:
        Token name;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::VStmtIB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        VarStmtItem(File const &file, Location start, Location end, Token name, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class VarStmtItemList : public VStmtIB
    {
    public:
        std::vector<std::unique_ptr<VarStmtItem>> items;
        virtual void accept(ASTNS::VStmtIB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        VarStmtItemList(File const &file, Location start, Location end, std::vector<std::unique_ptr<VarStmtItem>> items);
        Location _start, _end;
    };
    class ExprStmt : public Stmt
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ExprStmt(File const &file, Location start, Location end, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class RetStmt : public Stmt
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        RetStmt(File const &file, Location start, Location end, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class StmtList : public Stmt
    {
    public:
        std::vector<std::unique_ptr<Stmt>> stmts;
        virtual void accept(ASTNS::Stmt::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        StmtList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Stmt>> stmts);
        Location _start, _end;
    };
    class ImplRet : public ImplRetB
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::ImplRetB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ImplRet(File const &file, Location start, Location end, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class PrimitiveType : public Type
    {
    public:
        Token ty;
        virtual void accept(ASTNS::Type::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PrimitiveType(File const &file, Location start, Location end, Token ty);
        Location _start, _end;
    };
    class Arg : public ArgB
    {
    public:
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::ArgB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Arg(File const &file, Location start, Location end, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class ArgList : public ArgB
    {
    public:
        std::vector<std::unique_ptr<Arg>> args;
        virtual void accept(ASTNS::ArgB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ArgList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Arg>> args);
        Location _start, _end;
    };
    class Param : public ParamB
    {
    public:
        std::unique_ptr<Type> ty;
        Token name;
        virtual void accept(ASTNS::ParamB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Param(File const &file, Location start, Location end, std::unique_ptr<Type> ty, Token name);
        Location _start, _end;
    };
    class ParamList : public ParamB
    {
    public:
        std::vector<std::unique_ptr<Param>> params;
        virtual void accept(ASTNS::ParamB::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ParamList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Param>> params);
        Location _start, _end;
    };
    class Block : public Expr
    {
    public:
        std::unique_ptr<StmtList> stmts;
        std::unique_ptr<ImplRet> implRet;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        Block(File const &file, Location start, Location end, std::unique_ptr<StmtList> stmts, std::unique_ptr<ImplRet> implRet);
        Location _start, _end;
    };
    class IfExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> trues;
        std::unique_ptr<Expr> falses;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        IfExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses);
        Location _start, _end;
    };
    class ForExpr : public Expr
    {
    public:
        std::unique_ptr<VarStmt> initial;
        std::unique_ptr<Expr> cond;
        std::unique_ptr<Expr> increment;
        std::unique_ptr<Expr> body;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ForExpr(File const &file, Location start, Location end, std::unique_ptr<VarStmt> initial, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> increment, std::unique_ptr<Expr> body);
        Location _start, _end;
    };
    class AssignmentExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> target;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        AssignmentExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> target, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class ShortCircuitExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        ShortCircuitExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
        Location _start, _end;
    };
    class BinaryExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> lhs;
        Token op;
        std::unique_ptr<Expr> rhs;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        BinaryExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs);
        Location _start, _end;
    };
    class CastExpr : public Expr
    {
    public:
        std::unique_ptr<Type> castto;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        CastExpr(File const &file, Location start, Location end, std::unique_ptr<Type> castto, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class UnaryExpr : public Expr
    {
    public:
        Token op;
        std::unique_ptr<Expr> expr;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        UnaryExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr);
        Location _start, _end;
    };
    class CallExpr : public Expr
    {
    public:
        std::unique_ptr<Expr> callee;
        Token oparn;
        std::unique_ptr<ArgList> args;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        CallExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> callee, Token oparn, std::unique_ptr<ArgList> args);
        Location _start, _end;
    };
    class PrimaryExpr : public Expr
    {
    public:
        Token value;
        virtual void accept(ASTNS::Expr::Visitor *v) override;
        virtual Location const & start() override;
        virtual Location const & end() override;
        PrimaryExpr(File const &file, Location start, Location end, Token value);
        Location _start, _end;
    };
// This code was autogenerated - see the utils/ directory
// ASTHEADER END
}
