#include "replicate/replicatevisitor.h"

#include <iostream>
#include <string>

// short for "visit child"
#define VC(field) a->field->accept(this)
#define PTOK(tok) std::cout << tokenToStr(a->tok)
#define P(a) std::cout << a
#define PN P('\n')
#define POP P('(')
#define PCP P(')')
#define PS P(' ')
#define PSC P(';'); PN

void ReplicateVisitor::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    VC(lhs);
    PS;
    PTOK(op);
    PS;
    VC(rhs);
}
void ReplicateVisitor::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
    VC(condition);
    P(" ? ");
    VC(trues);
    P(" : ");
    VC(falses);
}
void ReplicateVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    PTOK(op);
    VC(operand);
}
void ReplicateVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    PTOK(value);
}
void ReplicateVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    VC(func);
    POP;
    VC(args);
    PCP;
}
void ReplicateVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    P("fun ");
    VC(rettype);
    PS;
    PTOK(name);
    POP;
    if (a->params)
        VC(params);
    PCP;
    PN;
    VC(block);
    PN;
}
void ReplicateVisitor::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    pi();
    P("var ");
    VC(type);
    PS;
    PTOK(name);
    if (a->value)
    {
        P(" = ");
        VC(value);
    }
    PSC;
}
void ReplicateVisitor::visitBaseType(ASTNS::BaseType *a)
{
    PTOK(type);
}
void ReplicateVisitor::visitBlockStmt(ASTNS::BlockStmt *a)
{
    pi();
    P('{');
    PN;
    for (std::unique_ptr<ASTNS::Stmt> &s : a->stmts)
        s->accept(this);
    P('}');
    PN;
}
void ReplicateVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    pi();
    VC(expr);
    PSC;
}
void ReplicateVisitor::visitReturnStmt(ASTNS::ReturnStmt *a)
{
    P("return ");
    VC(val);
    PSC;
}
void ReplicateVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    pi();
    P("var ");
    VC(type);
    PS;
    if (a->assign)
    {
        VC(assign);
    }
    else
        PTOK(name);
    PSC;
}
void ReplicateVisitor::visitProgram(ASTNS::Program *a)
{
    for (std::unique_ptr<ASTNS::Decl> &d : a->decls)
        d->accept(this);
    PN;
}
void ReplicateVisitor::visitParam(ASTNS::Param *a)
{
    VC(type);
    PS;
    PTOK(name);

    if (a->next)
    {
        P(", ");
        VC(next);
    }
}
void ReplicateVisitor::visitArg(ASTNS::Arg *a)
{
    VC(value);

    if (a->next)
    {
        P(", ");
        VC(next);
    }
}

void ReplicateVisitor::pi()
{
    std::cout << std::string(' ', indent);
}
