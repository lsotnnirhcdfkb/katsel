#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"

CodeGen::Helpers::ParamVisitor::ParamVisitor::ParamVisitor(CodeGen::Context &context, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, TypeVisitor &type_visitor):
    context(context),
    errored(false),
    is_method(false), this_ptr(false), this_mut(false),
    type_visitor(type_visitor),
    index(0) {
    for (std::unique_ptr<ASTNS::ParamB> &p : params) {
        p->accept(*this);
        ++index;
    }
}

void CodeGen::Helpers::ParamVisitor::visit(ASTNS::Param &ast) {
    Maybe<IR::Type&> ty (type_visitor.type(*ast.type));
    if (ty.has()) {
        std::string name (ast.name.value.name);
        Param p {ty.get(), std::move(name), ast, ast.mut};
        ret.push_back(p);
    } else {
        errored = true;
        return;
    }
}

void CodeGen::Helpers::ParamVisitor::visit(ASTNS::ThisParam &ast) {
    if (!type_visitor.this_type.has()) {
        errored = true;
        ERR_TYPELESS_THIS(ast);
        return;
    }

    if (index != 0) {
        errored = true;
        ERR_THIS_NOT_FIRST(ast);
        return;
    }

    NNPtr<IR::Type> this_type = type_visitor.this_type.get();

    is_method = true;
    this_ptr = ast.ptr;
    this_mut = ast.mut;

    NNPtr<IR::Type> ty = ast.ptr ? context.get_pointer_type(ast.mut, *this_type) : this_type;

    Param p {ty, "this", ast, false};
    ret.push_back(p);
}

CodeGen::Helpers::ArgVisitor::ArgVisitor::ArgVisitor(ExprCodeGen &expr_cg, std::vector<std::unique_ptr<ASTNS::Arg>> &args):
    expr_cg(expr_cg) {
    for (std::unique_ptr<ASTNS::Arg> &a : args)
        a->accept(*this);
}

void CodeGen::Helpers::ArgVisitor::visit(ASTNS::Arg &ast) {
    Maybe<IR::ASTValue> a = expr_cg.expr(*ast.expr);
    if (a.has())
        ret.push_back(a.get());
}
