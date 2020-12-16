#include "codegenlocal.h"

CodeGen::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen &cg): cg(cg) {}

void CodeGen::ParamVisitor::visitParam(ASTNS::Param *ast)
{
    IR::Type *ty (cg.typeVisitor->type(ast->type.get()));
    std::string name (ast->name.stringify());

    Param p {ty, std::move(name), ast};
    ret.push_back(p);
}

void CodeGen::ParamVisitor::visitParamList(ASTNS::ParamList *ast)
{
    ast->paramsegment->accept(this);
}
void CodeGen::ParamVisitor::visitParamSegment(ASTNS::ParamSegment *ast)
{
    ast->paramsegment->accept(this);
    ast->anotherparam->accept(this);
}
void CodeGen::ParamVisitor::visitParamList_OPT(ASTNS::ParamList_OPT *ast) {}

CodeGen::ArgVisitor::ArgVisitor::ArgVisitor(CodeGen::FunctionCodeGen &fcg): fcg(fcg) {}

void CodeGen::ArgVisitor::visitArgList(ASTNS::ArgList *ast)
{
    ast->argsegment->accept(this);
}
void CodeGen::ArgVisitor::visitArgSegment(ASTNS::ArgSegment *ast)
{
    ast->argsegment->accept(this);
    ast->anotherarg->accept(this);
}

void CodeGen::ArgVisitor::visitArg(ASTNS::Arg *ast)
{
    ret.push_back(fcg.exprCG.expr(ast->expr.get()));
}

void CodeGen::ArgVisitor::visitArgList_OPT(ASTNS::ArgList_OPT *ast) {}
