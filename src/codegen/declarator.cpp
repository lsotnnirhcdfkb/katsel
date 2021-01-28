#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/block.h"

CodeGen::Declarator::Declarator(CodeGen &cg): cg(cg), current_symbol(&cg.unit->mod), this_type(Maybe<NNPtr<IR::Type>>()) {}

void CodeGen::Declarator::visit(ASTNS::CU &ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast.decls)
        decl->accept(*this);
}

void CodeGen::Declarator::visit(ASTNS::FunctionDecl &fun) {
    std::string fname (fun.name.value.name);
    Maybe<IR::Value &> declbefore = current_symbol->get_value(fname);

    if (declbefore.has()) {
        ERR_REDECL_SYM(fun.name.span, declbefore.get());
        cg.errored = true;
        return;
    }

    Maybe<IR::Type &> m_retty = cg.type_visitor->type(*fun.retty, this_type);

    if (!m_retty.has())
        return;

    NNPtr<IR::Type const> retty = m_retty.get();

    CodeGen::ParamVisitor p (cg, fun.params, this_type);
    std::vector<CodeGen::ParamVisitor::Param> params (std::move(p.ret));

    std::vector<NNPtr<IR::Type const>> ptys;
    for (auto const &p : params)
        ptys.push_back(p.ty);

    NNPtr<IR::FunctionType> ft = cg.context->get_function_type(*retty, ptys);

    std::unique_ptr<IR::Function> f = std::make_unique<IR::Function>(ft, fname, fun);
    NNPtr<IR::Function> fraw = f.get();
    cg.unit->functions.push_back(std::move(f));
    current_symbol->add_value(fname, *fraw);

    if (p.is_method) {
        this_type.get()->add_method(fname, IR::Type::Method { fraw, p.this_ptr, p.this_mut });
    }
}

void CodeGen::Declarator::visit(ASTNS::ImplDecl &impl) {
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

void CodeGen::Declarator::visit(ASTNS::FunctionImplMember &member) {
    member.fun->accept(*this);
}

void CodeGen::Declarator::visit(ASTNS::ImplicitDecl &) {}
