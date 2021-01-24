#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

void CodeGen::FunctionCodeGen::StmtCodeGen::visit(ASTNS::VarStmtItem &ast) {
    std::string varname = ast.name.stringify();
    Maybe<NNPtr<CodeGen::FunctionCodeGen::Local>> var = fcg.get_local(varname);
    if (var.has() && var.get()->scopenum == fcg.cur_scope) {
        ERR_REDECL_VAR(ast.name, var.get()->v);
        cg.errored = true;
        return;
    }

    Maybe<NNPtr<IR::Type>> m_var_type = cg.type_visitor->type(ast.type.get(), fcg.this_type);
    if (!m_var_type.has()) {
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Type> var_type = m_var_type.get();

    IR::Instrs::Register &reg = fcg.register_block->add<IR::Instrs::Register>(ast, var_type, ast.mut);

    if (ast.expr) {
        Maybe<IR::ASTValue> m_val = fcg.expr_cg.expr(ast.expr.get());
        if (!m_val.has())
            return;

        IR::ASTValue val = m_val.get();

        val = var_type->impl_cast(*cg.context, *fcg.fun, fcg.cur_block, val);
        if (val.type() != var_type) {
            ERR_CONFLICT_VAR_INIT_TY(ast.equal, ast.name, ast.type.get(), val, var_type);
            fcg.errored = true;
            return;
        }
        fcg.cur_block->add<IR::Instrs::Store>(IR::ASTValue(reg, ast), val, true);
    } else if (!ast.mut) // no initializer, not mutable
        WARN_IMMUT_NOINIT(ast);

    fcg.add_local(varname, reg);
}
