#include "../codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"

CodeGen::ImplCodeGen::ImplCodeGen(CodeGen &cg, NNPtr<ASTNS::ImplDecl> ast): cg(cg), ast(ast), implFor(), errored(false) {}

bool CodeGen::ImplCodeGen::codegen() {
    implFor = cg.typeVisitor->type(ast->implFor.get(), Maybe<NNPtr<IR::Type>>());
    for (std::unique_ptr<ASTNS::ImplMember> &member : ast->members)
        member->accept(*this);
    return !errored;
}

void CodeGen::ImplCodeGen::visit_function_impl_member(ASTNS::FunctionImplMember &ast) {
    if (!implFor.has()) {
        errored = true;
        return;
    }

    NNPtr<IR::Type> implFor = this->implFor.get();

    Maybe<NNPtr<IR::Value>> m_val = implFor->getValue(ast.fun->name.stringify());
    if (!m_val.has()) {
        errored = true;
        return;
    }
    NNPtr<IR::Value> val = m_val.get();

    IR::Function *fun = dynamic_cast<IR::Function*>(val.asRaw());
    if (!fun) {
        errored = true;
        return;
    }

    FunctionCodeGen fcg (cg, ast.fun.get(), fun, implFor);
    if (!fcg.codegen())
        errored = true;
}
