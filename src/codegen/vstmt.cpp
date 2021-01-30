#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::VarStmtItem &ast) {
    std::string varname = ast.name.value.name;
    Maybe<CodeGen::Helpers::Local> var = locals.get_local(varname);
    if (var.has() && var.get().scopenum == locals.cur_scope) {
        ERR_REDECL_VAR(ast.name.span, *var.get().v);
        return;
    }

    Maybe<IR::Type&> m_var_type = type_visitor.type(*ast.type);
    if (!m_var_type.has()) {
        return;
    }

    NNPtr<IR::Type> var_type = m_var_type.get();

    IR::Instrs::Register &reg = ir_builder.register_block().add<IR::Instrs::Register>(ast, var_type, ast.mut);

    if (ast.expr) {
        Maybe<IR::ASTValue> m_val = expr_cg.expr(*ast.expr);
        if (!m_val.has())
            return;

        IR::ASTValue val = m_val.get();

        val = var_type->impl_cast(ir_builder.context(), ir_builder.fun(), ir_builder.cur_block(), val);
        if (&val.type() != var_type.as_raw()) {
            ERR_CONFLICT_VAR_INIT_TY(ast.equal.get().span, ast.name.span, *ast.type, val, *var_type);
            return;
        }
        ir_builder.cur_block()->add<IR::Instrs::Store>(IR::ASTValue(reg, ast), val, true);
    } else if (!ast.mut) // no initializer, not mutable
        WARN_IMMUT_NOINIT(ast);

    locals.add_local(varname, reg);
}
