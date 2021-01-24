#include "codegenlocal.h"
#include "ir/unit.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/block.h"
#include "ast/ast.h"

CodeGen::CodeGen(File const &file, NNPtr<ASTNS::CUB> cub):
    unit(std::make_unique<IR::Unit>(file)),
    context(std::make_unique<Context>(file, *this)),
    type_visitor(std::make_unique<TypeVisitor>(*this)),
    path_visitor(std::make_unique<PathVisitor>(*this)),
    errored(false),
    cub(cub) {}
CodeGen::~CodeGen() = default;

void CodeGen::forwdecl() {
    unit->mod.add_decl_symbol("void", context->get_void_type());
    unit->mod.add_decl_symbol("float", context->get_float_type(32));
    unit->mod.add_decl_symbol("double", context->get_float_type(64));
    unit->mod.add_decl_symbol("bool", context->get_bool_type());
    unit->mod.add_decl_symbol("char", context->get_char_type());
    unit->mod.add_decl_symbol("uint8", context->get_int_type(8, false));
    unit->mod.add_decl_symbol("uint16", context->get_int_type(16, false));
    unit->mod.add_decl_symbol("uint32", context->get_int_type(32, false));
    unit->mod.add_decl_symbol("uint64", context->get_int_type(64, false));
    unit->mod.add_decl_symbol("sint8", context->get_int_type(8, true));
    unit->mod.add_decl_symbol("sint16", context->get_int_type(16, true));
    unit->mod.add_decl_symbol("sint32", context->get_int_type(32, true));
    unit->mod.add_decl_symbol("sint64", context->get_int_type(64, true));

    ForwDecl f (*this);
    cub->accept(f);
}

void CodeGen::declarate() {
    Declarator d (*this);
    cub->accept(d);
}

void CodeGen::codegen() {
    cub->accept(*this);
}

// visiting {{{1
void CodeGen::visit(ASTNS::CU &ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast.decls)
        decl->accept(*this);
}

void CodeGen::visit(ASTNS::FunctionDecl &ast) {
    Maybe<NNPtr<IR::Value>> val = unit->mod.get_value(ast.name.stringify());
    IR::Function *fun;
    if (!val.has() || !(fun = dynamic_cast<IR::Function*>(val.get().as_raw()))) {
        errored = true;
        return;
    }

    FunctionCodeGen fcg (*this, ast, fun, Maybe<NNPtr<IR::Type>>());
    if (!fcg.codegen())
        errored = true;
}

void CodeGen::visit(ASTNS::ImplDecl &ast) {
    ImplCodeGen icg (*this, ast);
    if (!icg.codegen())
        errored = true;
}

void CodeGen::visit(ASTNS::ImplicitDecl &) {}
