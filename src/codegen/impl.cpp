#include "codegenlocal.h"
#include "ast/ast.h"

using Codegen::Impl;

Impl::Impl(IR::Unit &unit, Codegen::Context &context, ASTNS::ImplDecl &ast):
    unit(unit),
    context(context),
    ast(ast),
    visit_errored(false) {}

bool Impl::type_declare() {
    return true;
}

void Impl::visit(ASTNS::FunctionImplMember &member) {
    ASSERT(m_s1_data.has());
    auto &s1_data = m_s1_data.get();

    auto fun_cg = std::make_unique<Codegen::Function>(unit, context, *member.fun, *s1_data.impl_for, *s1_data.impl_for);
    if (fun_cg->type_declare() && fun_cg->value_declare())
        member_codegens.push_back(std::move(fun_cg)); // TODO: when type alias member items are added, this vector should go in stage 0
    else
        visit_errored = true;
}

bool Impl::value_declare() {
    auto path_visitor = std::make_unique<Helpers::PathVisitor>(Maybe<Helpers::Locals&>(), unit);
    auto type_visitor = std::make_unique<Helpers::TypeVisitor>(context, Maybe<NNPtr<IR::Type>>(), *path_visitor);

    auto impl_for = type_visitor->type(*ast.impl_for);
    if (!impl_for.has()) {
        return false;
    }

    m_s1_data = S1Data {
        std::move(path_visitor),
        std::move(type_visitor),
        impl_for.get(),
    };

    for (std::unique_ptr<ASTNS::ImplMember> &member : ast.members)
        member->accept(*this);

    return !visit_errored;
}

bool Impl::value_define() {
    bool success = true;
    for (std::unique_ptr<Codegen::CG> &s2cg : member_codegens)
        if (!s2cg->value_define()) success = false;

    return success;
}
