#include "codegenlocal.h"
#include "message/errmsgs.h"

CodeGen::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, IR::Type *thisType): cg(cg), thisType(thisType) {
    for (std::unique_ptr<ASTNS::ParamB> &p : params)
        p->accept(this);

    if (errored)
        ret.clear();
}

void CodeGen::ParamVisitor::visitParam(ASTNS::Param *ast) {
    IR::Type *ty (cg.typeVisitor->type(ast->type.get(), thisType));
    if (!ty) {
        errored = true;
        return;
    }

    std::string name (ast->name.stringify());

    Param p {ty, std::move(name), ast, ast->mut};
    ret.push_back(p);
}

void CodeGen::ParamVisitor::visitThisParam(ASTNS::ThisParam *ast) {
    if (!thisType) {
        errored = true;
        ERR_TYPELESS_THIS(ast);
        return;
    }

    IR::Type *ty;
    if (!ast->ptr)
        ty = thisType;
    else
        ty = cg.context->getPointerType(ast->mut, thisType);

    Param p {ty, "this", ast, false};
    ret.push_back(p);
}

CodeGen::ArgVisitor::ArgVisitor::ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args): fcg(fcg) {
    for (std::unique_ptr<ASTNS::Arg> &a : args)
        a->accept(this);
}

void CodeGen::ArgVisitor::visitArg(ASTNS::Arg *ast) {
    ret.push_back(fcg.exprCG.expr(ast->expr.get()));
}
