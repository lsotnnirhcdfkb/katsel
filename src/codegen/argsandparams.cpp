#include "codegenlocal.h"

CodeGen::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen &cg): cg(cg) {}

void CodeGen::ParamVisitor::visitParam(ASTNS::Param *ast) {
    IR::Type *ty (cg.typeVisitor->type(ast->type.get()));
    std::string name (ast->name.stringify());

    Param p {ty, std::move(name), ast};
    ret.push_back(p);
}

void CodeGen::ParamVisitor::visitParamList(ASTNS::ParamList *ast) {
    for (std::unique_ptr<ASTNS::Param> &p : ast->params)
        p->accept(this);
}

CodeGen::ArgVisitor::ArgVisitor::ArgVisitor(CodeGen::FunctionCodeGen &fcg): fcg(fcg) {}

void CodeGen::ArgVisitor::visitArgList(ASTNS::ArgList *ast) {
    for (std::unique_ptr<ASTNS::Arg> &a : ast->args)
        a->accept(this);
}
void CodeGen::ArgVisitor::visitArg(ASTNS::Arg *ast) {
    ret.push_back(fcg.exprCG.expr(ast->expr.get()));
}
