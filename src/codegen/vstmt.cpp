#include "codegenlocal.h"
#include "message/errmsgs.h"

void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmtItem(ASTNS::VarStmtItem *ast) {
    std::string varname = ast->name.stringify();
    CodeGen::FunctionCodeGen::Local *var = fcg.getLocal(varname);
    if (var && var->scopenum == fcg.curScope) {
        ERR_REDECL_VAR(ast->name, var->v);
        cg.errored = true;
        return;
    }

    IR::Type *varType = cg.typeVisitor->type(ast->type.get());
    if (!varType) {
        fcg.errored = true;
        return;
    }

    IR::Instrs::Register *reg = static_cast<IR::Instrs::Register*>(fcg.entryBlock->add(std::make_unique<IR::Instrs::Register>(ast, varType, ast->mut)));

    if (ast->expr) {
        IR::ASTValue val = fcg.exprCG.expr(ast->expr.get());
        if (!val)
            return;

        val = varType->implCast(*cg.context, *fcg.fun, fcg.curBlock, val);
        if (val.type() != varType) {
            ERR_CONFLICT_VAR_INIT_TY(ast->equal, ast->name, ast->type.get(), val, varType);
            fcg.errored = true;
            return;
        }
        fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(reg, ast), val, true));
    }

    fcg.addLocal(varname, reg);
}
