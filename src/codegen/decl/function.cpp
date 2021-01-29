#include "../codegenlocal.h"
#include "utils/format.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/block.h"

CodeGen::FunctionCodeGen::FunctionCodeGen(CodeGen &cg, NNPtr<ASTNS::FunctionDecl> ast, NNPtr<IR::Function> fun, Maybe<NNPtr<IR::Type>> this_type):
    cur_scope(0),
    cg(cg),
    ast(ast),
    expr_cg(cg, *this),
    stmt_cg(cg, *this),
    fun(fun),
    register_block(fun->add_block("registers")),
    entry_block(fun->add_block("entry")),
    exit_block(fun->add_block("exit")),
    cur_block(entry_block),
    ret(register_block->add<IR::Instrs::Register>(cg.unit->implicit_decl_ast.get(), fun->ty->ret, true)),
    type_visitor(cg, this_type),
    path_visitor(cg, *this),
    errored(false) {}

bool CodeGen::FunctionCodeGen::codegen() {
}

void CodeGen::FunctionCodeGen::add_local(std::string const &name, IR::Instrs::Register &val) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name && last->scopenum == cur_scope)
            report_abort_noh(format("duplicate local added: \"{}\"", name));

    Local l {cur_scope, val, name};
    locals.push_back(l);
}

Maybe<CodeGen::FunctionCodeGen::Local&> CodeGen::FunctionCodeGen::get_local(std::string const &name) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return *last;

    return Maybe<Local&>();
}

void CodeGen::FunctionCodeGen::inc_scope() {
    ++cur_scope;
}
void CodeGen::FunctionCodeGen::dec_scope() {
    --cur_scope;
    while (locals.size() && locals.back().scopenum > cur_scope) locals.pop_back();
}
