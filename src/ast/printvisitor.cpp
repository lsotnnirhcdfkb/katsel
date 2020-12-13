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
        case ASTNS::ArgList::Form::AT:
            pai("argsegment = ");
            if (a->argsegment)
            {
                a->argsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitArgList_OPT(ASTNS::ArgList_OPT *a)
{
    pai("ArgList_OPT\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ArgList_OPT::Form::EMPTY:
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitArgSegment(ASTNS::ArgSegment *a)
{
    pai("ArgSegment\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ArgSegment::Form::ATA:
            pai("argsegment = ");
            if (a->argsegment)
            {
                a->argsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("anotherarg = ");
            if (a->anotherarg)
            {
                a->anotherarg->accept(this);
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
void ASTNS::PrintVisitor::visitBinAndExpr(ASTNS::BinAndExpr *a)
{
    pai("BinAndExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BinAndExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitBinOrExpr(ASTNS::BinOrExpr *a)
{
    pai("BinOrExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BinOrExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitBitAndExpr(ASTNS::BitAndExpr *a)
{
    pai("BitAndExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitAndExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitBitOrExpr(ASTNS::BitOrExpr *a)
{
    pai("BitOrExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitOrExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitBitShiftExpr(ASTNS::BitShiftExpr *a)
{
    pai("BitShiftExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitShiftExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitBitXorExpr(ASTNS::BitXorExpr *a)
{
    pai("BitXorExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BitXorExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitBracedBlock(ASTNS::BracedBlock *a)
{
    pai("BracedBlock\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BracedBlock::Form::TAAT:
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
            pai("implret = ");
            if (a->implret)
            {
                a->implret->accept(this);
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
        case ASTNS::BracedBlock::Form::TTAAT:
            pai("ocurb = ");
            pai("[");
            pai(std::string(a->ocurb.start, a->ocurb.end));
            pai("]\n");
            pai("newlopt = ");
            pai("[");
            pai(std::string(a->newlopt.start, a->newlopt.end));
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
            pai("implret = ");
            if (a->implret)
            {
                a->implret->accept(this);
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
        case ASTNS::BracedBlock::Form::TTTAATT:
            pai("ocurb = ");
            pai("[");
            pai(std::string(a->ocurb.start, a->ocurb.end));
            pai("]\n");
            pai("newlopt = ");
            pai("[");
            pai(std::string(a->newlopt.start, a->newlopt.end));
            pai("]\n");
            pai("indentopt = ");
            pai("[");
            pai(std::string(a->indentopt.start, a->indentopt.end));
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
            pai("implret = ");
            if (a->implret)
            {
                a->implret->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("dedentopt = ");
            pai("[");
            pai(std::string(a->dedentopt.start, a->dedentopt.end));
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
void ASTNS::PrintVisitor::visitBuiltinType(ASTNS::BuiltinType *a)
{
    pai("BuiltinType\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::BuiltinType::Form::T:
            pai("type = ");
            pai("[");
            pai(std::string(a->type.start, a->type.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitCU(ASTNS::CU *a)
{
    pai("CU\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::CU::Form::A:
            pai("dl = ");
            if (a->dl)
            {
                a->dl->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            break;
        case ASTNS::CU::Form::EMPTY:
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
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitCastExpr(ASTNS::CastExpr *a)
{
    pai("CastExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::CastExpr::Form::TATA:
            pai("oparn = ");
            pai("[");
            pai(std::string(a->oparn.start, a->oparn.end));
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
            pai("cparn = ");
            pai("[");
            pai(std::string(a->cparn.start, a->cparn.end));
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
void ASTNS::PrintVisitor::visitCompEQExpr(ASTNS::CompEQExpr *a)
{
    pai("CompEQExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::CompEQExpr::Form::ATA:
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
void ASTNS::PrintVisitor::visitCompLGTExpr(ASTNS::CompLGTExpr *a)
{
    pai("CompLGTExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::CompLGTExpr::Form::ATA:
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
            pai("anotherdecl = ");
            if (a->anotherdecl)
            {
                a->anotherdecl->accept(this);
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
void ASTNS::PrintVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    pai("ExprStmt\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ExprStmt::Form::A:
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
void ASTNS::PrintVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    pai("FunctionDecl\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::FunctionDecl::Form::TATTATA:
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
        case ASTNS::FunctionDecl::Form::TATTATT:
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
            pai("newl = ");
            pai("[");
            pai(std::string(a->newl.start, a->newl.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitImplRet(ASTNS::ImplRet *a)
{
    pai("ImplRet\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ImplRet::Form::TAA:
            pai("leftarrow = ");
            pai("[");
            pai(std::string(a->leftarrow.start, a->leftarrow.end));
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
            pai("ending = ");
            if (a->ending)
            {
                a->ending->accept(this);
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
void ASTNS::PrintVisitor::visitImplRet_OPT(ASTNS::ImplRet_OPT *a)
{
    pai("ImplRet_OPT\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ImplRet_OPT::Form::EMPTY:
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitIndentedBlock(ASTNS::IndentedBlock *a)
{
    pai("IndentedBlock\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::IndentedBlock::Form::TTAAT:
            pai("newl = ");
            pai("[");
            pai(std::string(a->newl.start, a->newl.end));
            pai("]\n");
            pai("indent = ");
            pai("[");
            pai(std::string(a->indent.start, a->indent.end));
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
            pai("implret = ");
            if (a->implret)
            {
                a->implret->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("dedent = ");
            pai("[");
            pai(std::string(a->dedent.start, a->dedent.end));
            pai("]\n");
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
        case ASTNS::ParamList::Form::AT:
            pai("paramsegment = ");
            if (a->paramsegment)
            {
                a->paramsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitParamList_OPT(ASTNS::ParamList_OPT *a)
{
    pai("ParamList_OPT\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ParamList_OPT::Form::EMPTY:
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitParamSegment(ASTNS::ParamSegment *a)
{
    pai("ParamSegment\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::ParamSegment::Form::ATA:
            pai("paramsegment = ");
            if (a->paramsegment)
            {
                a->paramsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("anotherparam = ");
            if (a->anotherparam)
            {
                a->anotherparam->accept(this);
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
void ASTNS::PrintVisitor::visitRetExpr(ASTNS::RetExpr *a)
{
    pai("RetExpr\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::RetExpr::Form::TA:
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
            break;
        case ASTNS::RetExpr::Form::T:
            pai("ret = ");
            pai("[");
            pai(std::string(a->ret.start, a->ret.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitStmtEnding(ASTNS::StmtEnding *a)
{
    pai("StmtEnding\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::StmtEnding::Form::T:
            pai("tok = ");
            pai("[");
            pai(std::string(a->tok.start, a->tok.end));
            pai("]\n");
            break;
        case ASTNS::StmtEnding::Form::TT:
            pai("tok = ");
            pai("[");
            pai(std::string(a->tok.start, a->tok.end));
            pai("]\n");
            pai("tok2 = ");
            pai("[");
            pai(std::string(a->tok2.start, a->tok2.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitStmtEnding_OPT(ASTNS::StmtEnding_OPT *a)
{
    pai("StmtEnding_OPT\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::StmtEnding_OPT::Form::EMPTY:
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
            pai("stmtsegment = ");
            if (a->stmtsegment)
            {
                a->stmtsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("stmtending = ");
            if (a->stmtending)
            {
                a->stmtending->accept(this);
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
void ASTNS::PrintVisitor::visitStmtList_OPT(ASTNS::StmtList_OPT *a)
{
    pai("StmtList_OPT\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::StmtList_OPT::Form::EMPTY:
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitStmtSegment(ASTNS::StmtSegment *a)
{
    pai("StmtSegment\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::StmtSegment::Form::AAA:
            pai("stmtsegment = ");
            if (a->stmtsegment)
            {
                a->stmtsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("stmtending = ");
            if (a->stmtending)
            {
                a->stmtending->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("anotherstmt = ");
            if (a->anotherstmt)
            {
                a->anotherstmt->accept(this);
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
        case ASTNS::VarStmt::Form::TAA:
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
        case ASTNS::VarStmtItemList::Form::AT:
            pai("varstmtitemsegment = ");
            if (a->varstmtitemsegment)
            {
                a->varstmtitemsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            break;
    }
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visitVarStmtItemSegment(ASTNS::VarStmtItemSegment *a)
{
    pai("VarStmtItemSegment\n{\n");
    ++indent;
    switch (a->form)
    {
        case ASTNS::VarStmtItemSegment::Form::ATA:
            pai("varstmtitemsegment = ");
            if (a->varstmtitemsegment)
            {
                a->varstmtitemsegment->accept(this);
            }
            else
            {
                pai("nullptr\n");
            }
            pai("comma = ");
            pai("[");
            pai(std::string(a->comma.start, a->comma.end));
            pai("]\n");
            pai("anothervarstmtitem = ");
            if (a->anothervarstmtitem)
            {
                a->anothervarstmtitem->accept(this);
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

ASTNS::PrintVisitor::PrintVisitor(llvm::raw_ostream &ostream): ostream(ostream) {}
