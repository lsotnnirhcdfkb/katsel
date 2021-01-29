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
    if (!ast->body) {
        fun->prototypeonly = true;
        return true;
    }

    register_block->branch(std::make_unique<IR::Instrs::GotoBr>(entry_block));

    inc_scope();
    ParamVisitor pv (cg, ast->params, this_type);
    std::vector<ParamVisitor::Param> params (pv.ret);

    for (auto const &param : params) {
        std::string pname = param.name;
        IR::Instrs::Register &reg = register_block->add<IR::Instrs::Register>(param.ast, param.ty, param.mut);

        Maybe<Local&> foundparam = get_local(pname);
        if (foundparam.has()) {
            ERR_REDECL_PARAM(*param.ast, *foundparam.get().v);
            errored = true;
        } else
            add_local(pname, reg);
    }

    Maybe<IR::ASTValue> m_retval = expr_cg.expr(*ast->body);

    dec_scope();

    if (!errored) {
        NNPtr<IR::Instrs::Instruction> deref_ret_reg = exit_block->add<IR::Instrs::DerefPtr>(IR::ASTValue(*ret, *ast->retty));
        exit_block->branch(std::make_unique<IR::Instrs::Return>(IR::ASTValue(*deref_ret_reg, *ast->retty)));

        if (!m_retval.has()) {
            errored = true;
        } else {
            IR::ASTValue retval = m_retval.get();

            retval = fun->ty->ret->impl_cast(*cg.context, *fun, cur_block, retval);
            if (fun->ty->ret.as_raw() != &retval.type()) {
                ERR_CONFLICT_RET_TY(retval, *fun);
                errored = true;
            } else {
                cur_block->add<IR::Instrs::Store>(IR::ASTValue(*ret, *ast->retty), retval, false);
                cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(exit_block));
            }
        }
    }

    if (cur_scope != 0 && !errored)
        report_abort_noh("At the end of FunctionCodeGen::codegen, cur_scope != 0");

    return !errored;
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
