#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"

CodeGen::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Maybe<NNPtr<IR::Type>> this_type):
    errored(false),
    is_method(false), this_ptr(false), this_mut(false),
    cg(cg),
    this_type(this_type),
    index(0) {
    for (std::unique_ptr<ASTNS::ParamB> &p : params) {
        p->accept(*this);
        ++index;
    }
}

void CodeGen::ParamVisitor::visit(ASTNS::Param &ast) {
    Maybe<NNPtr<IR::Type>> ty (cg.type_visitor->type(ast.type.get(), this_type));
    if (ty.has()) {
        std::string name (ast.name.stringify());
        Param p {ty.get(), std::move(name), ast, ast.mut};
        ret.push_back(p);
    } else {
        errored = true;
        return;
    }
}

void CodeGen::ParamVisitor::visit(ASTNS::ThisParam &ast) {
    if (!this_type.has()) {
        errored = true;
        ERR_TYPELESS_THIS(ast);
        return;
    }

    if (index != 0) {
        errored = true;
        ERR_THIS_NOT_FIRST(ast);
        return;
    }

    is_method = true;
    this_ptr = ast.ptr;
    this_mut = ast.mut;

    NNPtr<IR::Type> ty = ast.ptr ? cg.context->get_pointer_type(ast.mut, this_type.get()) : this_type.get();

    Param p {ty, "this", ast, false};
    ret.push_back(p);
}

CodeGen::ArgVisitor::ArgVisitor::ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args): fcg(fcg) {
    for (std::unique_ptr<ASTNS::Arg> &a : args)
        a->accept(*this);
}

void CodeGen::ArgVisitor::visit(ASTNS::Arg &ast) {
    Maybe<IR::ASTValue> a = fcg.expr_cg.expr(ast.expr.get());
    if (a.has())
        ret.push_back(a.get());
}
