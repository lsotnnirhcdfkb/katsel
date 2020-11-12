#include "visit/printvisitor.h"

#include <iostream>
#include <string>

// PRINTVISITOR START

// The following code was autogenerated - see the utils/ directory
void PrintVisitor::visitAdditionexpr(ASTNS::Additionexpr *a)
{
    pai("Additionexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Additionexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitArgs(ASTNS::Args *a)
{
    pai("Args\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Args::Form::ATA:
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
            pai("comma =");
            pai(" [");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
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
            break;
        case ASTNS::Args::Form::A:
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
            break;
    }
    --indent;
}
void PrintVisitor::visitAssignmentexpr(ASTNS::Assignmentexpr *a)
{
    pai("Assignmentexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Assignmentexpr::Form::ATA:
            pai("target =");
            if (a->target)
            {
                ++indent;
                pai("\n");
                a->target->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("equal =");
            pai(" [");
            pai(std::string(a->equal.start, a->equal.end));
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBinandexpr(ASTNS::Binandexpr *a)
{
    pai("Binandexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Binandexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBinnotexpr(ASTNS::Binnotexpr *a)
{
    pai("Binnotexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Binnotexpr::Form::TA:
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBinorexpr(ASTNS::Binorexpr *a)
{
    pai("Binorexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Binorexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBitandexpr(ASTNS::Bitandexpr *a)
{
    pai("Bitandexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Bitandexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBitorexpr(ASTNS::Bitorexpr *a)
{
    pai("Bitorexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Bitorexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBitshiftexpr(ASTNS::Bitshiftexpr *a)
{
    pai("Bitshiftexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Bitshiftexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBitxorexpr(ASTNS::Bitxorexpr *a)
{
    pai("Bitxorexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Bitxorexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitBlock(ASTNS::Block *a)
{
    pai("Block\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Block::Form::TAT:
            pai("ocurb =");
            pai(" [");
            pai(std::string(a->ocurb.start, a->ocurb.end));
            pai("]\n");
            pai("stmts =");
            if (a->stmts)
            {
                ++indent;
                pai("\n");
                a->stmts->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("ccurb =");
            pai(" [");
            pai(std::string(a->ccurb.start, a->ccurb.end));
            pai("]\n");
            break;
        case ASTNS::Block::Form::TT:
            pai("ocurb =");
            pai(" [");
            pai(std::string(a->ocurb.start, a->ocurb.end));
            pai("]\n");
            pai("ccurb =");
            pai(" [");
            pai(std::string(a->ccurb.start, a->ccurb.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitCallexpr(ASTNS::Callexpr *a)
{
    pai("Callexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Callexpr::Form::ATAT:
            pai("callee =");
            if (a->callee)
            {
                ++indent;
                pai("\n");
                a->callee->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("oparn =");
            pai(" [");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
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
            pai("cparn =");
            pai(" [");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            break;
        case ASTNS::Callexpr::Form::ATT:
            pai("callee =");
            if (a->callee)
            {
                ++indent;
                pai("\n");
                a->callee->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("oparn =");
            pai(" [");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("cparn =");
            pai(" [");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitCompeqexpr(ASTNS::Compeqexpr *a)
{
    pai("Compeqexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Compeqexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitComplgtexpr(ASTNS::Complgtexpr *a)
{
    pai("Complgtexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Complgtexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitDecl(ASTNS::Decl *a)
{
    pai("Decl\n");
    ++indent;
    switch (a->form)
    {
    }
    --indent;
}
void PrintVisitor::visitDecls(ASTNS::Decls *a)
{
    pai("Decls\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Decls::Form::AA:
            pai("decls =");
            if (a->decls)
            {
                ++indent;
                pai("\n");
                a->decls->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("decl =");
            if (a->decl)
            {
                ++indent;
                pai("\n");
                a->decl->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
        case ASTNS::Decls::Form::A:
            pai("decl =");
            if (a->decl)
            {
                ++indent;
                pai("\n");
                a->decl->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
    }
    --indent;
}
void PrintVisitor::visitEmptystmt(ASTNS::Emptystmt *a)
{
    pai("Emptystmt\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Emptystmt::Form::T:
            pai("semi =");
            pai(" [");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitExpression(ASTNS::Expression *a)
{
    pai("Expression\n");
    ++indent;
    switch (a->form)
    {
    }
    --indent;
}
void PrintVisitor::visitExprstmt(ASTNS::Exprstmt *a)
{
    pai("Exprstmt\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Exprstmt::Form::AT:
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
            pai("semi =");
            pai(" [");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitFunction(ASTNS::Function *a)
{
    pai("Function\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Function::Form::TATTTA:
            pai("fun =");
            pai(" [");
            pai(std::string(a->fun.start, a->fun.end));
            pai("]\n");
            pai("retty =");
            if (a->retty)
            {
                ++indent;
                pai("\n");
                a->retty->accept(this);
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
            pai("oparn =");
            pai(" [");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("cparn =");
            pai(" [");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            pai("body =");
            if (a->body)
            {
                ++indent;
                pai("\n");
                a->body->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
        case ASTNS::Function::Form::TATTATA:
            pai("fun =");
            pai(" [");
            pai(std::string(a->fun.start, a->fun.end));
            pai("]\n");
            pai("retty =");
            if (a->retty)
            {
                ++indent;
                pai("\n");
                a->retty->accept(this);
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
            pai("oparn =");
            pai(" [");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("paramlist =");
            if (a->paramlist)
            {
                ++indent;
                pai("\n");
                a->paramlist->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("cparn =");
            pai(" [");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            pai("body =");
            if (a->body)
            {
                ++indent;
                pai("\n");
                a->body->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
    }
    --indent;
}
void PrintVisitor::visitMultexpr(ASTNS::Multexpr *a)
{
    pai("Multexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Multexpr::Form::ATA:
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
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitParamlist(ASTNS::Paramlist *a)
{
    pai("Paramlist\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Paramlist::Form::ATAT:
            pai("plist =");
            if (a->plist)
            {
                ++indent;
                pai("\n");
                a->plist->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("comma =");
            pai(" [");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
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
            break;
        case ASTNS::Paramlist::Form::AT:
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
            break;
    }
    --indent;
}
void PrintVisitor::visitPrimaryexpr(ASTNS::Primaryexpr *a)
{
    pai("Primaryexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Primaryexpr::Form::T:
            pai("value =");
            pai(" [");
            pai(std::string(a->value.start, a->value.end));
            pai("]\n");
            break;
        case ASTNS::Primaryexpr::Form::TAT:
            pai("oparn =");
            pai(" [");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
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
            pai("cparn =");
            pai(" [");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitRetstmt(ASTNS::Retstmt *a)
{
    pai("Retstmt\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Retstmt::Form::TAT:
            pai("ret =");
            pai(" [");
            pai(std::string(a->ret.start, a->ret.end));
            pai("]\n");
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
            pai("semi =");
            pai(" [");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitStmt(ASTNS::Stmt *a)
{
    pai("Stmt\n");
    ++indent;
    switch (a->form)
    {
    }
    --indent;
}
void PrintVisitor::visitStmts(ASTNS::Stmts *a)
{
    pai("Stmts\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Stmts::Form::AA:
            pai("stmts =");
            if (a->stmts)
            {
                ++indent;
                pai("\n");
                a->stmts->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("stmt =");
            if (a->stmt)
            {
                ++indent;
                pai("\n");
                a->stmt->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
    }
    --indent;
}
void PrintVisitor::visitTernaryexpr(ASTNS::Ternaryexpr *a)
{
    pai("Ternaryexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Ternaryexpr::Form::ATATA:
            pai("cond =");
            if (a->cond)
            {
                ++indent;
                pai("\n");
                a->cond->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("quest =");
            pai(" [");
            pai(std::string(a->quest.start, a->quest.end));
            pai("]\n");
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
            pai("colon =");
            pai(" [");
            pai(std::string(a->colon.start, a->colon.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitType(ASTNS::Type *a)
{
    pai("Type\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Type::Form::T:
            pai("type =");
            pai(" [");
            pai(std::string(a->type.start, a->type.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitUnaryexpr(ASTNS::Unaryexpr *a)
{
    pai("Unaryexpr\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Unaryexpr::Form::TA:
            pai("op =");
            pai(" [");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
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
            break;
    }
    --indent;
}
void PrintVisitor::visitVarstmt(ASTNS::Varstmt *a)
{
    pai("Varstmt\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Varstmt::Form::TAAT:
            pai("var =");
            pai(" [");
            pai(std::string(a->var.start, a->var.end));
            pai("]\n");
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
            pai("assignments =");
            if (a->assignments)
            {
                ++indent;
                pai("\n");
                a->assignments->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("semi =");
            pai(" [");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitVarstmtitem(ASTNS::Varstmtitem *a)
{
    pai("Varstmtitem\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Varstmtitem::Form::TTA:
            pai("name =");
            pai(" [");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            pai("equal =");
            pai(" [");
            pai(std::string(a->equal.start, a->equal.end));
            pai("]\n");
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
            break;
        case ASTNS::Varstmtitem::Form::T:
            pai("name =");
            pai(" [");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            break;
    }
    --indent;
}
void PrintVisitor::visitVarstmtitems(ASTNS::Varstmtitems *a)
{
    pai("Varstmtitems\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Varstmtitems::Form::ATA:
            pai("items =");
            if (a->items)
            {
                ++indent;
                pai("\n");
                a->items->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            pai("comma =");
            pai(" [");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("item =");
            if (a->item)
            {
                ++indent;
                pai("\n");
                a->item->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
        case ASTNS::Varstmtitems::Form::A:
            pai("item =");
            if (a->item)
            {
                ++indent;
                pai("\n");
                a->item->accept(this);
                --indent;
            }
            else
            {
                pai(" nullptr\n");
            }
            break;
    }
    --indent;
}
// This code was autogenerated - see the utils/ directory

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
