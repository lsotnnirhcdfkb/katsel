#include "codegenlocal.h"
#include "message/errmsgs.h"

CodeGen::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Maybe<NNPtr<IR::Type>> thisType):
    errored(false),
    isMethod(false), thisPtr(false), thisMut(false),
    cg(cg),
    thisType(thisType),
    index(0) {
    for (std::unique_ptr<ASTNS::ParamB> &p : params) {
        p->accept(*this);
        ++index;
    }
}

void CodeGen::ParamVisitor::visitParam(ASTNS::Param &ast) {
    Maybe<NNPtr<IR::Type>> ty (cg.typeVisitor->type(ast.type.get(), thisType));
    if (ty.has()) {
        std::string name (ast.name.stringify());
        Param p {ty.get(), std::move(name), ast, ast.mut};
        ret.push_back(p);
    } else {
        errored = true;
        return;
    }
}

void CodeGen::ParamVisitor::visitThisParam(ASTNS::ThisParam &ast) {
    if (!thisType.has()) {
        errored = true;
        ERR_TYPELESS_THIS(ast);
        return;
    }

    if (index != 0) {
        errored = true;
        ERR_THIS_NOT_FIRST(ast);
        return;
    }

    isMethod = true;
    thisPtr = ast.ptr;
    thisMut = ast.mut;

    NNPtr<IR::Type> ty = ast.ptr ? cg.context->getPointerType(ast.mut, thisType.get()) : thisType.get();

    Param p {ty, "this", ast, false};
    ret.push_back(p);
}

CodeGen::ArgVisitor::ArgVisitor::ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args): fcg(fcg) {
    for (std::unique_ptr<ASTNS::Arg> &a : args)
        a->accept(*this);
}

void CodeGen::ArgVisitor::visitArg(ASTNS::Arg &ast) {
    Maybe<IR::ASTValue> a = fcg.exprCG.expr(ast.expr.get());
    if (a.has())
        ret.push_back(a.get());
}
