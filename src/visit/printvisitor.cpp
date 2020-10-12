#include "visit/printvisitor.h"

#include <iostream>
#include <string>

// PRINTVISITOR START
void PrintVisitor::visitProgram(ASTNS::Program *a)
{
    pai("Program\n");
    ++indent;
    pai("decls =");
    pai("\n");
    ++indent;
    for (auto &i : a->decls)
    {
        pai("- ");
        i->accept(this);
    }
    --indent;
    --indent;
}
void PrintVisitor::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    pai("BinaryExpr\n");
    ++indent;
    pai("lhs =");
    if (a->lhs)
    {
        ++indent;
        pai("\n");
        a->lhs->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("rhs =");
    if (a->rhs)
    {
        ++indent;
        pai("\n");
        a->rhs->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("op =");
    pai(" [");
    pai(std::string(a->op.start, a->op.end));
    pai("]\n");
    --indent;
}
void PrintVisitor::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
    pai("TernaryExpr\n");
    ++indent;
    pai("condition =");
    if (a->condition)
    {
        ++indent;
        pai("\n");
        a->condition->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("trues =");
    if (a->trues)
    {
        ++indent;
        pai("\n");
        a->trues->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("falses =");
    if (a->falses)
    {
        ++indent;
        pai("\n");
        a->falses->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    pai("UnaryExpr\n");
    ++indent;
    pai("operand =");
    if (a->operand)
    {
        ++indent;
        pai("\n");
        a->operand->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("op =");
    pai(" [");
    pai(std::string(a->op.start, a->op.end));
    pai("]\n");
    --indent;
}
void PrintVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    pai("PrimaryExpr\n");
    ++indent;
    pai("value =");
    pai(" [");
    pai(std::string(a->value.start, a->value.end));
    pai("]\n");
    --indent;
}
void PrintVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    pai("CallExpr\n");
    ++indent;
    pai("func =");
    if (a->func)
    {
        ++indent;
        pai("\n");
        a->func->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("args =");
    if (a->args)
    {
        ++indent;
        pai("\n");
        a->args->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitLtoRVExpr(ASTNS::LtoRVExpr *a)
{
    pai("LtoRVExpr\n");
    ++indent;
    pai("val =");
    if (a->val)
    {
        ++indent;
        pai("\n");
        a->val->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitBlockStmt(ASTNS::BlockStmt *a)
{
    pai("BlockStmt\n");
    ++indent;
    pai("stmts =");
    pai("\n");
    ++indent;
    for (auto &i : a->stmts)
    {
        pai("- ");
        i->accept(this);
    }
    --indent;
    --indent;
}
void PrintVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    pai("ExprStmt\n");
    ++indent;
    pai("expr =");
    if (a->expr)
    {
        ++indent;
        pai("\n");
        a->expr->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitReturnStmt(ASTNS::ReturnStmt *a)
{
    pai("ReturnStmt\n");
    ++indent;
    pai("val =");
    if (a->val)
    {
        ++indent;
        pai("\n");
        a->val->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    pai("VarStmt\n");
    ++indent;
    pai("type =");
    if (a->type)
    {
        ++indent;
        pai("\n");
        a->type->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("name =");
    pai(" [");
    pai(std::string(a->name.start, a->name.end));
    pai("]\n");
    pai("value =");
    if (a->value)
    {
        ++indent;
        pai("\n");
        a->value->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitBaseType(ASTNS::BaseType *a)
{
    pai("BaseType\n");
    ++indent;
    pai("type =");
    pai(" [");
    pai(std::string(a->type.start, a->type.end));
    pai("]\n");
    --indent;
}
void PrintVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    pai("FunctionDecl\n");
    ++indent;
    pai("rettype =");
    if (a->rettype)
    {
        ++indent;
        pai("\n");
        a->rettype->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("name =");
    pai(" [");
    pai(std::string(a->name.start, a->name.end));
    pai("]\n");
    pai("params =");
    if (a->params)
    {
        ++indent;
        pai("\n");
        a->params->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("block =");
    if (a->block)
    {
        ++indent;
        pai("\n");
        a->block->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    pai("GlobalVarDecl\n");
    ++indent;
    pai("type =");
    if (a->type)
    {
        ++indent;
        pai("\n");
        a->type->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("name =");
    pai(" [");
    pai(std::string(a->name.start, a->name.end));
    pai("]\n");
    pai("value =");
    if (a->value)
    {
        ++indent;
        pai("\n");
        a->value->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitParam(ASTNS::Param *a)
{
    pai("Param\n");
    ++indent;
    pai("type =");
    if (a->type)
    {
        ++indent;
        pai("\n");
        a->type->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("name =");
    pai(" [");
    pai(std::string(a->name.start, a->name.end));
    pai("]\n");
    pai("next =");
    if (a->next)
    {
        ++indent;
        pai("\n");
        a->next->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
void PrintVisitor::visitArg(ASTNS::Arg *a)
{
    pai("Arg\n");
    ++indent;
    pai("value =");
    if (a->value)
    {
        ++indent;
        pai("\n");
        a->value->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    pai("next =");
    if (a->next)
    {
        ++indent;
        pai("\n");
        a->next->accept(this);
        --indent;
    }
    else
    {
        pai(" nullptr\n");
    }
    --indent;
}
// PRINTVISITOR END

void PrintVisitor::pai(std::string &s)
{
    for (auto i = s.begin(); i != s.end(); ++i)
    {
        if (pindent)
            for (int j = 0; j < indent; ++j)
                std::cout << "  ";

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
