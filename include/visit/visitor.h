#pragma once

namespace ASTNS
{
// ASTFORWDECL BEGIN

// The following code was autogenerated - see the utils/ directory
class Expr;
class Decl;
class Type;
class Stmt;
class Program;
class BinaryExpr;
class TernaryExpr;
class UnaryExpr;
class PrimaryExpr;
class CallExpr;
class BlockStmt;
class ExprStmt;
class ReturnStmt;
class VarStmt;
class BaseType;
class FunctionDecl;
class GlobalVarDecl;
class Param;
class Arg;
// This code was autogenerated - see the utils/ directory

// ASTFORWDECL END
}

// PUREASTVISITCS START

// The following code was autogenerated - see the utils/ directory
class ExprVisitor
{
public:
    virtual void visitBinaryExpr(ASTNS::BinaryExpr *a) = 0;
    virtual void visitTernaryExpr(ASTNS::TernaryExpr *a) = 0;
    virtual void visitUnaryExpr(ASTNS::UnaryExpr *a) = 0;
    virtual void visitPrimaryExpr(ASTNS::PrimaryExpr *a) = 0;
    virtual void visitCallExpr(ASTNS::CallExpr *a) = 0;
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
// This code was autogenerated - see the utils/ directory

// PUREASTVISITCS END
