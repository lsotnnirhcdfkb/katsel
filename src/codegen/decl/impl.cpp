#include "../codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ir/function.h"
#include "ast/ast.h"

CodeGen::ImplCodeGen::ImplCodeGen(CodeGen &cg, NNPtr<ASTNS::ImplDecl> ast): cg(cg), ast(ast), impl_for(), errored(false) {}

bool CodeGen::ImplCodeGen::codegen() {
    impl_for = cg.type_visitor->type(*ast->impl_for, Maybe<NNPtr<IR::Type>>())
        .fmap<NNPtr<IR::Type>>([] (IR::Type &t) { return NNPtr(t); });
    for (std::unique_ptr<ASTNS::ImplMember> const &member : ast->members)
        member->accept(*this);
    return !errored;
}

void CodeGen::ImplCodeGen::visit(ASTNS::FunctionImplMember &ast) {
    if (!impl_for.has()) {
        errored = true;
        return;
    }

    NNPtr<IR::Type> impl_for = this->impl_for.get();

    Maybe<IR::Value&> m_val = impl_for->get_value(ast.fun->name.stringify());
    if (!m_val.has()) {
        errored = true;
        return;
    }
    NNPtr<IR::Value> val = m_val.get();

    IR::Function *fun = dynamic_cast<IR::Function*>(val.as_raw());
    if (!fun) {
        errored = true;
        return;
    }

    FunctionCodeGen fcg (cg, ast.fun.get(), fun, impl_for);
    if (!fcg.codegen())
        errored = true;
}
