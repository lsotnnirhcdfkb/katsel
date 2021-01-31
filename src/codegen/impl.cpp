#include "codegenlocal.h"
#include "ast/ast.h"

using CodeGen::Stage0CG, CodeGen::Stage1CG, CodeGen::Stage2CG, CodeGen::Stage3CG;
using namespace CodeGen::Impl;

// TODO: this will not work due to the lifetimes of previous stage's helper instances being deleted while being passed to the next stage's references
Stage0::Stage0(IR::Unit &unit, CodeGen::Context &context, ASTNS::ImplDecl &ast):
    unit(unit),
    context(context),
    ast(ast) {}
Stage1::Stage1(IR::Unit &unit, CodeGen::Context &context, ASTNS::ImplDecl &ast, Helpers::PathVisitor path_visitor, Helpers::TypeVisitor type_visitor):
    unit(unit),
    context(context),
    ast(ast),
    path_visitor(path_visitor),
    type_visitor(type_visitor),
    impl_for(type_visitor.type(*ast.impl_for)),
    errored(false) {}
Stage2::Stage2(IR::Unit &unit, CodeGen::Context &context, ASTNS::ImplDecl &ast, Helpers::PathVisitor path_visitor, Helpers::TypeVisitor type_visitor, IR::Type &impl_for):
    unit(unit),
    context(context),
    ast(ast),
    path_visitor(path_visitor),
    type_visitor(type_visitor),
    impl_for(impl_for) {}

Maybe<std::unique_ptr<Stage1CG>> Stage0::type_fw_declare() {
    Helpers::PathVisitor path_visitor (Maybe<Helpers::Locals&>(), unit);
    Helpers::TypeVisitor type_visitor (context, Maybe<NNPtr<IR::Type>>(), path_visitor);
    return std::make_unique<Stage1>(unit, context, ast, path_visitor, type_visitor);
}

Maybe<std::unique_ptr<Stage2CG>> Stage1::value_fw_declare() {
    if (!impl_for.has()) {
        return Maybe<std::unique_ptr<Stage2CG>>();
    }

    for (std::unique_ptr<ASTNS::ImplMember> &member : ast.members) {
        member->accept(*this);
    }

    if (errored)
        return Maybe<std::unique_ptr<Stage2CG>>();
    else
        return std::make_unique<Stage2>(unit, context, ast, path_visitor, type_visitor, impl_for.get());
}

void Stage1::visit(ASTNS::FunctionImplMember &member) {
    auto m_s2 = CodeGen::Function::Stage0(unit, context, *member.fun, impl_for.get(), impl_for.get()).type_fw_declare();
    if (m_s2.has())
        item_codegens.push_back(std::move(m_s2.get())); // TODO: when type alias member items are added, this vector should go in stage 0
    else
        errored = true;
}

Maybe<std::unique_ptr<Stage3CG>> Stage2::block_codegen() {
    for (std::unique_ptr<Stage2CG> &s2cg : item_codegens)
        s2cg->block_codegen();

    return std::make_unique<Stage3>();
}
