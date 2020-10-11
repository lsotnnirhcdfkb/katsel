#include "printvisitor.h"

#include <iostream>
#include <string>

// Visiting Expr {{{1
void PrintVisitor::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    phead("Binary Expr");
    ptok("Op", a->op);
    pchild("LHS", a->lhs);
    pchild("RHS", a->rhs);
    pclose();
}
void PrintVisitor::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
    phead("Ternary Expr");
    pchild("Cond", a->condition);
    pchild("Trues", a->trues);
    pchild("Falses", a->falses);
    pclose();
}
void PrintVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    phead("Unary Expr");
    ptok("Op", a->op);
    pchild("Operand", a->operand);
    pclose();
}
void PrintVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    phead("Primary Expr");
    ptok("Value", a->value);
    pclose();
}
void PrintVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    phead("Call Expr");
    pchild("Calling", a->func);
    pchild("With args", a->args);
    pclose();
}
void PrintVisitor::visitLtoRVExpr(ASTNS::LtoRVExpr *a)
{
    phead("LtoRValExpr");
    pchild("RValue", a->val);
    pclose();
}
// Visitng Decl {{{1
void PrintVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    phead("Function decl");
    pchild("Return type", a->rettype);
    ptok("Name", a->name);
    pchild("Params", a->params);
    pchild("Block", a->block);
    pclose();
}
void PrintVisitor::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    phead("GlobalVarDecl");
    pchild("Type", a->type);
    ptok("Name", a->name);
    pchild("Value", a->value);
    pclose();
}
// Visiting Type {{{1
void PrintVisitor::visitBaseType(ASTNS::BaseType *a)
{
    phead("BaseType");
    ptok("Type", a->type);
    pclose();
}
// Visiting Stmt {{{1
void PrintVisitor::visitBlockStmt(ASTNS::BlockStmt *a)
{
    phead("BlockStmt");
    for (std::unique_ptr<ASTNS::Stmt> &s : a->stmts)
    {
        s->accept(this);
    }
    pclose();
}
void PrintVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    phead("ExprStmt");
    pchild("Expr", a->expr);
    pclose();
}
void PrintVisitor::visitReturnStmt(ASTNS::ReturnStmt *a)
{
    phead("ReturnStmt");
    pchild("Expr", a->val);
    pclose();
}
void PrintVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    phead("VarStmt");
    pchild("Type", a->type);
    ptok("Name", a->name);
    pchild("Value", a->value);
    pclose();
}
// Visiting Program {{{1
void PrintVisitor::visitProgram(ASTNS::Program *a)
{
    phead("Program");

    for (std::unique_ptr<ASTNS::Decl> &d : a->decls)
    {
        d->accept(this);
    }

    pclose();
}
// Visiting Param and Args {{{1
void PrintVisitor::visitParam(ASTNS::Param *a)
{
    phead("Param");
    pchild("Type", a->type);
    ptok("Name", a->name);
    pchild("Next", a->next);
}
void PrintVisitor::visitArg(ASTNS::Arg *a)
{
    phead("Arg");
    pchild("Value", a->value);
    pchild("Next", a->next);
}
// Helper functions {{{1
void PrintVisitor::pai(std::string &s)
{
    for (auto i = s.begin(); i != s.end(); ++i)
    {
        if (pindent)
            for (int j = 0; j < indent; ++j)
                std::cout << "|   ";

        pindent = false;
        std::cout << *i;

        if (*i == '\n')
            pindent = true;
    }

    std::cout << std::flush;
}
void PrintVisitor::pai(std::string &&s)
{
    pai(s);
}

template <typename T>
void PrintVisitor::pchild(std::string &&s, const T &a)
{
    pai(s);
    if (a)
    {
        pai("\n{\n");
        ++indent;
        a->accept(this);
        --indent;
        pai("}\n");
    }
    else
    {
        pai(" {nullptr}\n");
    }
}
void PrintVisitor::ptok(std::string &&s, Token &t)
{
    pai(s);
    pai(" [");
    pai(std::string(t.start, t.end));
    pai("]\n");
}
void PrintVisitor::phead(std::string &&s)
{
    pai(s);
    pai("\n{\n");
    ++indent;
}
void PrintVisitor::pclose()
{
    --indent;
    pai("}\n");
}
