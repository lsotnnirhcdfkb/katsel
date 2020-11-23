#include "ast/printvisitor.h"

#include <iostream>
#include <string>

// PRINTVISITOR START

// The following code was autogenerated - see the utils/ directory
void ASTNS::PrintVisitor::visitAdditionExpr(ASTNS::AdditionExpr *a)
{
    pai("AdditionExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::AdditionExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitArg(ASTNS::Arg *a)
{
    pai("Arg\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Arg::Form::A:
            pai("expr = ");
            if (a->expr)
            {
                a->expr->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitArgList(ASTNS::ArgList *a)
{
    pai("ArgList\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ArgList::Form::ATA:
            pai("arglist = ");
            if (a->arglist)
            {
                a->arglist->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("arg = ");
            if (a->arg)
            {
                a->arg->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitAssignmentExpr(ASTNS::AssignmentExpr *a)
{
    pai("AssignmentExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::AssignmentExpr::Form::ATA:
            pai("target = ");
            if (a->target)
            {
                a->target->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("equal = ");
            pai("[");
            pai(std::string(a->equal.start, a->equal.end));
            pai("]\n");
            pai("value = ");
            if (a->value)
            {
                a->value->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBinandExpr(ASTNS::BinandExpr *a)
{
    pai("BinandExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BinandExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBinnotExpr(ASTNS::BinnotExpr *a)
{
    pai("BinnotExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BinnotExpr::Form::TA:
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("operand = ");
            if (a->operand)
            {
                a->operand->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBinorExpr(ASTNS::BinorExpr *a)
{
    pai("BinorExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BinorExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBitandExpr(ASTNS::BitandExpr *a)
{
    pai("BitandExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitandExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBitorExpr(ASTNS::BitorExpr *a)
{
    pai("BitorExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitorExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBitshiftExpr(ASTNS::BitshiftExpr *a)
{
    pai("BitshiftExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitshiftExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBitxorExpr(ASTNS::BitxorExpr *a)
{
    pai("BitxorExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitxorExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBlock(ASTNS::Block *a)
{
    pai("Block\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Block::Form::TAT:
            pai("ocurb = ");
            pai("[");
            pai(std::string(a->ocurb.start, a->ocurb.end));
            pai("]\n");
            pai("stmts = ");
            if (a->stmts)
            {
                a->stmts->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("ccurb = ");
            pai("[");
            pai(std::string(a->ccurb.start, a->ccurb.end));
            pai("]\n");
            break;
        case ASTNS::Block::Form::TT:
            pai("ocurb = ");
            pai("[");
            pai(std::string(a->ocurb.start, a->ocurb.end));
            pai("]\n");
            pai("ccurb = ");
            pai("[");
            pai(std::string(a->ccurb.start, a->ccurb.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBuiltinTypeNoVoid(ASTNS::BuiltinTypeNoVoid *a)
{
    pai("BuiltinTypeNoVoid\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BuiltinTypeNoVoid::Form::T:
            pai("type = ");
            pai("[");
            pai(std::string(a->type.start, a->type.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitBuiltinTypeVoid(ASTNS::BuiltinTypeVoid *a)
{
    pai("BuiltinTypeVoid\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BuiltinTypeVoid::Form::T:
            pai("type = ");
            pai("[");
            pai(std::string(a->type.start, a->type.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    pai("CallExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::CallExpr::Form::ATAT:
            pai("callee = ");
            if (a->callee)
            {
                a->callee->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("oparn = ");
            pai("[");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("args = ");
            if (a->args)
            {
                a->args->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("cparn = ");
            pai("[");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            break;
        case ASTNS::CallExpr::Form::ATT:
            pai("callee = ");
            if (a->callee)
            {
                a->callee->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("oparn = ");
            pai("[");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("cparn = ");
            pai("[");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitCompeqExpr(ASTNS::CompeqExpr *a)
{
    pai("CompeqExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::CompeqExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitComplgtExpr(ASTNS::ComplgtExpr *a)
{
    pai("ComplgtExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ComplgtExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitDeclList(ASTNS::DeclList *a)
{
    pai("DeclList\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::DeclList::Form::AA:
            pai("decllist = ");
            if (a->decllist)
            {
                a->decllist->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("decl = ");
            if (a->decl)
            {
                a->decl->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitEmptyStmt(ASTNS::EmptyStmt *a)
{
    pai("EmptyStmt\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::EmptyStmt::Form::T:
            pai("semi = ");
            pai("[");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    pai("ExprStmt\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ExprStmt::Form::AT:
            pai("expr = ");
            if (a->expr)
            {
                a->expr->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("semi = ");
            pai("[");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitFunction(ASTNS::Function *a)
{
    pai("Function\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Function::Form::TATTTA:
            pai("fun = ");
            pai("[");
            pai(std::string(a->fun.start, a->fun.end));
            pai("]\n");
            pai("retty = ");
            if (a->retty)
            {
                a->retty->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("name = ");
            pai("[");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            pai("oparn = ");
            pai("[");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("cparn = ");
            pai("[");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            pai("body = ");
            if (a->body)
            {
                a->body->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
        case ASTNS::Function::Form::TATTATA:
            pai("fun = ");
            pai("[");
            pai(std::string(a->fun.start, a->fun.end));
            pai("]\n");
            pai("retty = ");
            if (a->retty)
            {
                a->retty->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("name = ");
            pai("[");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            pai("oparn = ");
            pai("[");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("paramlist = ");
            if (a->paramlist)
            {
                a->paramlist->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("cparn = ");
            pai("[");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            pai("body = ");
            if (a->body)
            {
                a->body->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitMultExpr(ASTNS::MultExpr *a)
{
    pai("MultExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::MultExpr::Form::ATA:
            pai("lhs = ");
            if (a->lhs)
            {
                a->lhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("rhs = ");
            if (a->rhs)
            {
                a->rhs->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitParam(ASTNS::Param *a)
{
    pai("Param\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::Param::Form::AT:
            pai("type = ");
            if (a->type)
            {
                a->type->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("name = ");
            pai("[");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitParamList(ASTNS::ParamList *a)
{
    pai("ParamList\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ParamList::Form::ATA:
            pai("paramlist = ");
            if (a->paramlist)
            {
                a->paramlist->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("param = ");
            if (a->param)
            {
                a->param->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    pai("PrimaryExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::PrimaryExpr::Form::T:
            pai("value = ");
            pai("[");
            pai(std::string(a->value.start, a->value.end));
            pai("]\n");
            break;
        case ASTNS::PrimaryExpr::Form::TAT:
            pai("oparn = ");
            pai("[");
            pai(std::string(a->oparn.start, a->oparn.end));
            pai("]\n");
            pai("expr = ");
            if (a->expr)
            {
                a->expr->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("cparn = ");
            pai("[");
            pai(std::string(a->cparn.start, a->cparn.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitRetStmt(ASTNS::RetStmt *a)
{
    pai("RetStmt\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::RetStmt::Form::TAT:
            pai("ret = ");
            pai("[");
            pai(std::string(a->ret.start, a->ret.end));
            pai("]\n");
            pai("expr = ");
            if (a->expr)
            {
                a->expr->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("semi = ");
            pai("[");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
        case ASTNS::RetStmt::Form::TT:
            pai("ret = ");
            pai("[");
            pai(std::string(a->ret.start, a->ret.end));
            pai("]\n");
            pai("semi = ");
            pai("[");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitStmtList(ASTNS::StmtList *a)
{
    pai("StmtList\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::StmtList::Form::AA:
            pai("stmtlist = ");
            if (a->stmtlist)
            {
                a->stmtlist->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("stmt = ");
            if (a->stmt)
            {
                a->stmt->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
    pai("TernaryExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::TernaryExpr::Form::ATATA:
            pai("cond = ");
            if (a->cond)
            {
                a->cond->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("quest = ");
            pai("[");
            pai(std::string(a->quest.start, a->quest.end));
            pai("]\n");
            pai("trues = ");
            if (a->trues)
            {
                a->trues->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("colon = ");
            pai("[");
            pai(std::string(a->colon.start, a->colon.end));
            pai("]\n");
            pai("falses = ");
            if (a->falses)
            {
                a->falses->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    pai("UnaryExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::UnaryExpr::Form::TA:
            pai("op = ");
            pai("[");
            pai(std::string(a->op.start, a->op.end));
            pai("]\n");
            pai("operand = ");
            if (a->operand)
            {
                a->operand->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    pai("VarStmt\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::VarStmt::Form::TAAT:
            pai("var = ");
            pai("[");
            pai(std::string(a->var.start, a->var.end));
            pai("]\n");
            pai("type = ");
            if (a->type)
            {
                a->type->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("assignments = ");
            if (a->assignments)
            {
                a->assignments->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("semi = ");
            pai("[");
            pai(std::string(a->semi.start, a->semi.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitVarStmtItem(ASTNS::VarStmtItem *a)
{
    pai("VarStmtItem\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::VarStmtItem::Form::TTA:
            pai("name = ");
            pai("[");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            pai("equal = ");
            pai("[");
            pai(std::string(a->equal.start, a->equal.end));
            pai("]\n");
            pai("expr = ");
            if (a->expr)
            {
                a->expr->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
        case ASTNS::VarStmtItem::Form::T:
            pai("name = ");
            pai("[");
            pai(std::string(a->name.start, a->name.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitVarStmtItemList(ASTNS::VarStmtItemList *a)
{
    pai("VarStmtItemList\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::VarStmtItemList::Form::ATA:
            pai("varstmtitemlist = ");
            if (a->varstmtitemlist)
            {
                a->varstmtitemlist->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("varstmtitem = ");
            if (a->varstmtitem)
            {
                a->varstmtitem->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
    }
    --indent;
    pai("}\n");
}
// This code was autogenerated - see the utils/ directory

// PRINTVISITOR END

void ASTNS::PrintVisitor::pai(std::string &s)
{
    for (auto i = s.begin(); i != s.end(); ++i)
    {
        if (pindent)
            for (int j = 0; j < indent; ++j)
                ostream << "  ";

        pindent = false;
        ostream << *i;

        if (*i == '\n')
            pindent = true;
    }
}
void ASTNS::PrintVisitor::pai(std::string &&s)
{
    pai(s);
}

ASTNS::PrintVisitor::PrintVisitor(std::ostream &ostream): ostream(ostream) {}
