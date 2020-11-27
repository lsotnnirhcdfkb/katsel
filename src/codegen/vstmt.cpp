#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/errmsgs.h"

void CodeGenNS::StmtCodeGen::visitVarStmtItem(ASTNS::VarStmtItem *ast)
{
    std::string varname = ast->name.stringify();
    CodeGenNS::Context::Local *var = cg.context.findLocal(varname);
    if (var && var->scopenum == cg.context.curScope)
    {
        ERR_REDECL_VAR(ast->name, var->v);
        cg.errored = true;
        return;
    }

    IR::Register *reg = cg.context.curFunc->addRegister(varty, ast, false);
    cg.context.addLocal(varname, reg);

    if (ast->expr)
    {
        IR::ASTValue val = cg.exprCodeGen.expr(ast->expr.get());
        if (!val)
            return;

        if (val.type() != varty)
        {
            ERR_CONFLICT_VAR_INIT_TY(ast->equal, ast->name, val, reg);
            cg.errored = true;
            return;
        }
        cg.context.curBlock->add(std::make_unique<IR::Instrs::Store>(reg, val));
    }
}
void CodeGenNS::StmtCodeGen::visitVarStmtItemList(ASTNS::VarStmtItemList *ast)
{
    ast->varstmtitemlist->accept(this);
    ast->varstmtitem->accept(this);
}
