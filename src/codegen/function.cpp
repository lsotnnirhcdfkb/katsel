#include "codegenlocal.h"
#include "ast/ast.h"
#include "message/errmsgs.h"

using CodeGen::Stage0CG, CodeGen::Stage1CG, CodeGen::Stage2CG, CodeGen::Stage3CG;
using namespace CodeGen::Function;

Stage0::Stage0(IR::Unit &unit, CodeGen::Context &context, ASTNS::FunctionDecl &ast, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol):
    unit(unit),
    context(context),
    ast(ast),
    this_type(this_type),
    parent_symbol(parent_symbol) {}
Stage1::Stage1(IR::Unit &unit, CodeGen::Context &context, ASTNS::FunctionDecl &ast, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol):
    unit(unit),
    context(context),
    ast(ast),
    path_visitor(),
    type_visitor(context, this_type, path_visitor),
    this_type(this_type),
    parent_symbol(parent_symbol) {}
Stage2::Stage2(IR::Unit &unit, CodeGen::Context &context, ASTNS::FunctionDecl &ast, IR::Function &fun, Helpers::PathVisitor &path_visitor, Helpers::TypeVisitor &type_visitor, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol, std::vector<Helpers::ParamVisitor::Param> &params):
    unit(unit),
    context(context),
    ast(ast),
    fun(fun),
    path_visitor(path_visitor),
    type_visitor(type_visitor),
    this_type(this_type),
    parent_symbol(parent_symbol),
    params(params),
    register_block(fun.add_block("registers")),
    entry_block(fun.add_block("entry")),
    exit_block(fun.add_block("exit")),
    cur_block(entry_block),
    ret(register_block->add<IR::Instrs::Register>(context.implicit_decl_ast.get(), fun.ty->ret, true)) {}

Maybe<std::unique_ptr<Stage1CG>> Stage0::type_fw_declare() {
    return std::make_unique<Stage1>(unit, context, ast, this_type, parent_symbol);
}

Maybe<std::unique_ptr<Stage2CG>> Stage1::value_fw_declare() {
    std::string fname (ast.name.value.name);

    Maybe<IR::Value &> declbefore = parent_symbol.get_value(fname);
    if (declbefore.has()) {
        ERR_REDECL_SYM(ast.name.span, declbefore.get());
        return Maybe<std::unique_ptr<Stage2CG>>();
    }

    Maybe<IR::Type &> m_retty = type_visitor.type(*ast.retty);

    if (!m_retty.has())
        return Maybe<std::unique_ptr<Stage2CG>>();

    IR::Type const &retty = m_retty.get();

    Helpers::ParamVisitor param_visitor (ast.params, type_visitor);
    std::vector<Helpers::ParamVisitor::Param> params (std::move(param_visitor.ret));

    std::vector<NNPtr<IR::Type const>> ptys;
    for (auto const &p : params)
        ptys.push_back(p.ty);

    NNPtr<IR::FunctionType> ft = context.get_function_type(retty, ptys);

    std::unique_ptr<IR::Function> f = std::make_unique<IR::Function>(ft, fname, ast);
    IR::Function &f_ref = *f;
    unit.functions.push_back(std::move(f));
    parent_symbol.add_value(fname, f_ref);

    if (param_visitor.is_method) {
        this_type.get()->add_method(fname, IR::Type::Method { f_ref, param_visitor.this_ptr, param_visitor.this_mut });
    }
    return std::make_unique<Stage2>(unit, context, ast, f_ref, path_visitor, type_visitor, this_type, parent_symbol, params);
}

Maybe<std::unique_ptr<Stage3CG>> Stage2::block_codegen() {
    bool errored = false;
    if (!ast.body) {
        fun.prototypeonly = true;
        return std::make_unique<Stage3>();
    }

    register_block->branch(std::make_unique<IR::Instrs::GotoBr>(entry_block));

    locals.inc_scope();

    for (auto const &param : params) {
        std::string pname = param.name;
        IR::Instrs::Register &reg = register_block->add<IR::Instrs::Register>(param.ast, param.ty, param.mut);

        Maybe<Local> foundparam = locals.get_local(pname);
        if (foundparam.has()) {
            ERR_REDECL_PARAM(*param.ast, *foundparam.get().v);
            errored = true;
        } else
            locals.add_local(pname, reg);
    }

    Maybe<IR::ASTValue> m_retval = expr_cg.expr(*ast.body);

    locals.dec_scope();

    if (m_retval.has()) {
        NNPtr<IR::Instrs::Instruction> deref_ret_reg = exit_block->add<IR::Instrs::DerefPtr>(IR::ASTValue(*ret, *ast.retty));
        exit_block->branch(std::make_unique<IR::Instrs::Return>(IR::ASTValue(*deref_ret_reg, *ast.retty)));

        IR::ASTValue retval = m_retval.get();

        retval = fun.ty->ret->impl_cast(context, fun, cur_block, retval);
        if (fun.ty->ret.as_raw() != &retval.type()) {
            ERR_CONFLICT_RET_TY(retval, fun);
            errored = true;
        } else {
            cur_block->add<IR::Instrs::Store>(IR::ASTValue(*ret, *ast.retty), retval, false);
            cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(exit_block));
        }
    }

    if (locals.cur_scope != 0 && m_retval.has())
        report_abort_noh("At the end of FunctionCodeGen::codegen, cur_scope != 0");

    if (!errored)
        return std::make_unique<Stage3>();
    else
        return Maybe<std::unique_ptr<Stage3CG>>();
}
