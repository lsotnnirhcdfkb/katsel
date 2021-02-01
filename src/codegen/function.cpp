#include "codegenlocal.h"
#include "ast/ast.h"
#include "message/errmsgs.h"

using CodeGen::Function;

Function::Function(IR::Unit &unit, CodeGen::Context &context, ASTNS::FunctionDecl &ast, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol):
    unit(unit),
    context(context),
    ast(ast),
    this_type(this_type),
    parent_symbol(parent_symbol) {}

bool Function::type_declare() {
    return true;
}

bool Function::value_declare() {
    auto path_visitor = std::make_unique<Helpers::PathVisitor>(Maybe<Helpers::Locals&>(), unit);
    auto type_visitor = std::make_unique<Helpers::TypeVisitor>(context, this_type, *path_visitor);

    std::string fname (ast.name.value.name);

    Maybe<IR::Value &> declbefore = parent_symbol.get_value(fname);
    if (declbefore.has()) {
        ERR_REDECL_SYM(ast.name.span, declbefore.get());
        return false;
    }

    Maybe<IR::Type &> m_retty = type_visitor->type(*ast.retty);
    if (!m_retty.has())
        return false;

    IR::Type const &retty = m_retty.get();

    Helpers::ParamVisitor param_visitor (context, ast.params, *type_visitor);
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

    m_s1_data = S1Data {
        std::move(path_visitor),
        std::move(type_visitor),
        f_ref,
        params,
    };

    return true;
}

bool Function::value_define() {
    ASSERT(m_s1_data.has());
    S1Data &s1_data = m_s1_data.get();

    bool errored = false;
    if (!ast.body) {
        s1_data.fun->prototypeonly = true;
        return true;
    }

    IR::Block &register_block = s1_data.fun->add_block("registers");
    IR::Block &exit_block = s1_data.fun->add_block("exit");
    IR::Block &entry_block = s1_data.fun->add_block("entry");
    IR::Instrs::Register &ret_reg = register_block.add<IR::Instrs::Register>(*ast.retty, *s1_data.fun->ty->ret, true);
    auto ir_builder (std::make_unique<IR::Builder>(*s1_data.fun, register_block, exit_block, ret_reg, entry_block, context));
    auto locals(std::make_unique<Helpers::Locals>());

    ir_builder->register_block().branch(std::make_unique<IR::Instrs::GotoBr>(entry_block));

    locals->inc_scope();

    for (auto const &param : s1_data.params) {
        std::string pname = param.name;
        IR::Instrs::Register &reg = ir_builder->register_block().add<IR::Instrs::Register>(param.ast, param.ty, param.mut);

        Maybe<Helpers::Local> foundparam = locals->get_local(pname);
        if (foundparam.has()) {
            ERR_REDECL_PARAM(*param.ast, *foundparam.get().v);
            errored = true;
        } else
            locals->add_local(pname, reg);
    }

    auto expr_cg (std::make_unique<CodeGen::Helpers::ExprCodeGen>(*ir_builder, *locals, *s1_data.type_visitor, *s1_data.path_visitor));
    Maybe<IR::ASTValue> m_retval = expr_cg->expr(*ast.body);

    locals->dec_scope();

    if (m_retval.has()) {
        NNPtr<IR::Instrs::Instruction> deref_ret_reg = ir_builder->exit_block().add<IR::Instrs::DerefPtr>(IR::ASTValue(ir_builder->ret_reg(), *ast.retty));
        ir_builder->exit_block().branch(std::make_unique<IR::Instrs::Return>(IR::ASTValue(*deref_ret_reg, *ast.retty)));

        IR::ASTValue retval = m_retval.get();

        retval = s1_data.fun->ty->ret->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), retval);
        if (s1_data.fun->ty->ret.as_raw() != &retval.type()) {
            ERR_CONFLICT_RET_TY(retval, *s1_data.fun);
            errored = true;
        } else {
            ir_builder->cur_block()->add<IR::Instrs::Store>(IR::ASTValue(ir_builder->ret_reg(), *ast.retty), retval, false);
            ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(ir_builder->exit_block()));
        }
    }

    if (locals->cur_scope != 0 && m_retval.has())
        report_abort_noh("At the end of FunctionCodeGen::codegen, cur_scope != 0");

    m_s2_data = {

    };

    return !errored;
}
