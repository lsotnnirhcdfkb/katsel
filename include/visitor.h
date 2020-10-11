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
};
class DeclVisitor
{
public:
    virtual void visit(ASTNS::Decl *a) = 0;
};
class TypeVisitor
{
public:
    virtual void visit(ASTNS::Type *a) = 0;
};
class StmtVisitor
{
public:
    virtual void visit(ASTNS::Stmt *a) = 0;
};
class ProgramVisitor
{
public:
    virtual void visit(ASTNS::Program *a) = 0;
};
class ParamVisitor
{
public:
    virtual void visit(ASTNS::Param *a) = 0;
};
class ArgVisitor
{
public:
    virtual void visit(ASTNS::Arg *a) = 0;
};
// PUREASTVISITCS END
