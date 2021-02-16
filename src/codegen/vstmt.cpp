#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

void Codegen::Helpers::StmtCodegen::ast_visit(ASTNS::VarStmtItem &ast) {
    std::string const &varname = ast.name.value.name;
    Maybe<Codegen::Helpers::Local> var = locals->get_local(varname);
    if (var.has() && var.get().scopenum == locals->cur_scope) {
        ERR_REDECL_VAR(ast.name.span, *var.get().v);
        success = false;
        return;
    }

    Maybe<IR::Type&> m_var_type = type_visitor->type(*ast.type);
    if (!m_var_type.has()) {
        success = false;
        return;
    }

    NNPtr<IR::Type> var_type = m_var_type.get();

    IR::Register &reg = ir_builder->fun().add_register(*var_type, ast, ast.mut);

    if (ast.expr) {
        Maybe<Located<NNPtr<IR::Value>>> m_val = expr_cg->expr(*ast.expr);
        if (!m_val.has()) {
            success = false;
            return;
        }

        Located<NNPtr<IR::Value>> val = m_val.get();

        val = var_type->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), val);
        if (&val.value->type() != var_type.as_raw()) {
            ERR_CONFLICT_VAR_INIT_TY(ast.equal.get().span, ast.name.span, *ast.type, val, *var_type);
            success = false;
            return;
        }
        ir_builder->cur_block()->add<IR::Instrs::Copy>(reg, val);
    } else if (!ast.mut) // no initializer, not mutable
        WARN_IMMUT_NOINIT(ast);

    locals->add_local(varname, reg);
}
