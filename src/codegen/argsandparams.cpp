#include "codegenlocal.h"

CodeGen::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::Param>> &params): cg(cg) {
    for (std::unique_ptr<ASTNS::Param> &p : params) {
        p->accept(this);
    }
}

void CodeGen::ParamVisitor::visitParam(ASTNS::Param *ast) {
    IR::Type *ty (cg.typeVisitor->type(ast->type.get()));
    if (!ty)
        return;

    std::string name (ast->name.stringify());

    Param p {ty, std::move(name), ast, ast->mut};
    ret.push_back(p);
}

CodeGen::ArgVisitor::ArgVisitor::ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args): fcg(fcg) {
    for (std::unique_ptr<ASTNS::Arg> &a : args)
        a->accept(this);
}

void CodeGen::ArgVisitor::visitArg(ASTNS::Arg *ast) {
    ret.push_back(fcg.exprCG.expr(ast->expr.get()));
}
