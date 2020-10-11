#pragma once

namespace ASTNS
{
// ASTFORWDECL BEGIN
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
// ASTFORWDECL END
}

// PUREASTVISITCS START
class ExprVisitor
{
public:
    virtual void visitBinaryExpr(ASTNS::BinaryExpr *a) = 0;
    virtual void visitTernaryExpr(ASTNS::TernaryExpr *a) = 0;
    virtual void visitUnaryExpr(ASTNS::UnaryExpr *a) = 0;
    virtual void visitPrimaryExpr(ASTNS::PrimaryExpr *a) = 0;
    virtual void visitAssignExpr(ASTNS::AssignExpr *a) = 0;
    virtual void visitCallExpr(ASTNS::CallExpr *a) = 0;
    virtual void visitLtoRVExpr(ASTNS::LtoRVExpr *a) = 0;
    virtual void visitVarRef(ASTNS::VarRef *a) = 0;
    virtual ~ExprVisitor();
};
class DeclVisitor
{
public:
    virtual void visitFunctionDecl(ASTNS::FunctionDecl *a) = 0;
    virtual void visitGlobalVarDecl(ASTNS::GlobalVarDecl *a) = 0;
    virtual ~DeclVisitor();
};
class TypeVisitor
{
public:
    virtual void visitBaseType(ASTNS::BaseType *a) = 0;
    virtual ~TypeVisitor();
};
class StmtVisitor
{
public:
    virtual void visitBlockStmt(ASTNS::BlockStmt *a) = 0;
    virtual void visitExprStmt(ASTNS::ExprStmt *a) = 0;
    virtual void visitReturnStmt(ASTNS::ReturnStmt *a) = 0;
    virtual void visitVarStmt(ASTNS::VarStmt *a) = 0;
    virtual ~StmtVisitor();
};
class ProgramVisitor
{
public:
    virtual void visitProgram(ASTNS::Program *a) = 0;
    virtual ~ProgramVisitor();
};
class ParamVisitor
{
public:
    virtual void visitParam(ASTNS::Param *a) = 0;
    virtual ~ParamVisitor();
};
class ArgVisitor
{
public:
    virtual void visitArg(ASTNS::Arg *a) = 0;
    virtual ~ArgVisitor();
};
// PUREASTVISITCS END
