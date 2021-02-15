#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"

Codegen::Helpers::TypeVisitor::TypeVisitor(Codegen::Context &context, Maybe<NNPtr<IR::Type>> this_type, Helpers::PathVisitor &path_visitor):
    context(context),
    this_type(this_type),
    path_visitor(path_visitor) {}

Maybe<IR::Type &> Codegen::Helpers::TypeVisitor::type(ASTNS::Type &ast) {
    Maybe<NNPtr<IR::Type>> oldret = ret;
    ret = Maybe<NNPtr<IR::Type>>();

    ast.ast_accept(*this);

    Maybe<NNPtr<IR::Type>> newret = ret;
    ret = oldret;

    return newret.fmap([](NNPtr<IR::Type> i) -> IR::Type & { return *i; });
}

void Codegen::Helpers::TypeVisitor::ast_visit(ASTNS::PathType &ast) {
    Maybe<IR::DeclSymbol &> m_decl = path_visitor->resolve_decl_symbol(*ast.path);

    if (!m_decl.has()) {
        ret = Maybe<NNPtr<IR::Type>>();
        return;
    }
    IR::DeclSymbol &decl = m_decl.get();

    IR::Type *as_type = dynamic_cast<IR::Type *>(&decl);
    if (!as_type) {
        ret = Maybe<NNPtr<IR::Type>>();
        ERR_NOT_A_TYPE(*ast.path, decl.decl_ast());
        return;
    } else {
        ret = Maybe<NNPtr<IR::Type>>(NNPtr<IR::Type>(*as_type));
    }
}

void Codegen::Helpers::TypeVisitor::ast_visit(ASTNS::PointerType &ast) {
    Maybe<IR::Type &> ty = type(*ast.type);
    ty.match([this, &ast] (NNPtr<IR::Type const> const &ty) {
            ret = Maybe<NNPtr<IR::Type>>(static_cast<NNPtr<IR::Type>>(context->get_pointer_type(ast.mut, *ty)));
        },
        [this]  {
            ret = Maybe<NNPtr<IR::Type>>();
        });
}

void Codegen::Helpers::TypeVisitor::ast_visit(ASTNS::ThisType &ast) {
    if (this_type.has()) {
        ret = Maybe(this_type);
    } else {
        ERR_NO_THIS(ast.th.span);
        ret = Maybe<NNPtr<IR::Type>>();
        return;
    }

    ret = this_type;
}
