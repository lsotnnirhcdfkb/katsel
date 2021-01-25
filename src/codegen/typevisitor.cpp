#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"

CodeGen::TypeVisitor::TypeVisitor(CodeGen &cg): cg(cg) {}

Maybe<IR::Type &> CodeGen::TypeVisitor::type(ASTNS::Type &ast, Maybe<NNPtr<IR::Type>> this_type) {
    Maybe<NNPtr<IR::Type>> oldret = ret;
    ret = Maybe<NNPtr<IR::Type>>();
    this->this_type = this_type;

    ast.accept(*this);

    Maybe<NNPtr<IR::Type>> newret = ret;
    ret = oldret;
    this->this_type = Maybe<NNPtr<IR::Type>>();

    return newret.fmap<IR::Type &>([] (NNPtr<IR::Type> i) -> IR::Type & { return *i; });
}

void CodeGen::TypeVisitor::visit(ASTNS::PathType &ast) {
    Maybe<IR::DeclSymbol &> m_decl = cg.path_visitor->resolve_decl_symbol(*ast.path);

    if (!m_decl.has()) {
        ret = Maybe<NNPtr<IR::Type>>();
        cg.errored = true;
        return;
    }
    IR::DeclSymbol &decl = m_decl.get();

    IR::Type *as_type = dynamic_cast<IR::Type *>(&decl);
    if (!as_type) {
        ret = Maybe<NNPtr<IR::Type>>();
        ERR_NOT_A_TYPE(*ast.path, decl.decl_ast());
        cg.errored = true;
        return;
    } else {
        ret = Maybe<NNPtr<IR::Type>>(NNPtr<IR::Type>(*as_type));
    }
}

void CodeGen::TypeVisitor::visit(ASTNS::PointerType &ast) {
    Maybe<IR::Type &> ty = type(*ast.type, this_type);
    ty.match([this, &ast] (NNPtr<IR::Type const> const &ty) {
            ret = Maybe<NNPtr<IR::Type>>(static_cast<NNPtr<IR::Type>>(cg.context->get_pointer_type(ast.mut, *ty)));
        },
        [this]  {
            cg.errored = true;
            ret = Maybe<NNPtr<IR::Type>>();
        });
}

void CodeGen::TypeVisitor::visit(ASTNS::ThisType &ast) {
    if (this_type.has()) {
        ret = Maybe(this_type);
    } else {
        ERR_NO_THIS(ast.th);
        cg.errored = true;
        ret = Maybe<NNPtr<IR::Type>>();
        return;
    }

    ret = this_type;
}
