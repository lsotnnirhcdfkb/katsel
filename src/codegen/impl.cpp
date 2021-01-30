#include "codegenlocal.h"

using CodeGen::Stage0CG, CodeGen::Stage1CG, CodeGen::Stage2CG, CodeGen::Stage3CG;
using namespace CodeGen::Impl;

Maybe<std::unique_ptr<Stage1CG>> Stage0::type_fw_declare() const {}

Maybe<std::unique_ptr<Stage2CG>> Stage1::value_fw_declare() const {
    Maybe<IR::Type&> m_impl_for = cg.type_visitor->type(*impl.impl_for, Maybe<NNPtr<IR::Type>>());
    if (!m_impl_for.has()) {
        cg.errored = true;
        ERR_UNDECL_SYMB(*impl.impl_for);
        return;
    }

    NNPtr<IR::Type> impl_for = m_impl_for.get();

    NNPtr<IR::DeclSymbol> old_symbol = current_symbol;
    current_symbol = impl_for;
    this_type = impl_for;
    for (std::unique_ptr<ASTNS::ImplMember> &member : impl.members) {
        member->accept(*this);
    }
    current_symbol = old_symbol;
    this_type = Maybe<NNPtr<IR::Type>>();
}

Maybe<std::unique_ptr<Stage3CG>> Stage2::block_codegen() const {
    impl_for = cg.type_visitor->type(*ast->impl_for, Maybe<NNPtr<IR::Type>>())
        .fmap<NNPtr<IR::Type>>([] (IR::Type &t) { return NNPtr(t); });
    for (std::unique_ptr<ASTNS::ImplMember> const &member : ast->members)
        member->accept(*this);
    return !errored;
}
