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
    virtual void visit(ASTNS::Expr *a) = 0;
    virtual ~ExprVisitor();
};
class DeclVisitor
{
public:
    virtual void visit(ASTNS::Decl *a) = 0;
    virtual ~DeclVisitor();
};
class TypeVisitor
{
public:
    virtual void visit(ASTNS::Type *a) = 0;
    virtual ~TypeVisitor();
};
class StmtVisitor
{
public:
    virtual void visit(ASTNS::Stmt *a) = 0;
    virtual ~StmtVisitor();
};
class ProgramVisitor
{
public:
    virtual void visit(ASTNS::Program *a) = 0;
    virtual ~ProgramVisitor();
};
class ParamVisitor
{
public:
    virtual void visit(ASTNS::Param *a) = 0;
    virtual ~ParamVisitor();
};
class ArgVisitor
{
public:
    virtual void visit(ASTNS::Arg *a) = 0;
    virtual ~ArgVisitor();
};
// PUREASTVISITCS END
