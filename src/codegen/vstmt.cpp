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

    IR::Instrs::Register *reg = static_cast<IR::Instrs::Register*>(fcg.curBlock->add(std::make_unique<IR::Instrs::Register>(ast, varty)));

    if (ast->expr) {
        IR::ASTValue val = fcg.exprCG.expr(ast->expr.get());
        if (!val)
            return;

        val = varty->implCast(*cg.context, *fcg.fun, fcg.curBlock, val);
        if (val.type() != varty) {
            ERR_CONFLICT_VAR_INIT_TY(ast->equal, ast->name, val, reg);
            cg.errored = true;
            return;
        }
        fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(reg, ast), val));
    }

    fcg.addLocal(varname, reg);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmtItemList(ASTNS::VarStmtItemList *ast) {
    for (std::unique_ptr<ASTNS::VarStmtItem> &item : ast->items)
        item->accept(this);
}
