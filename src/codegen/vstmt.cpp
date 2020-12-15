#include "codegenlocal.h"
#include "message/errors.h"
#include "message/errmsgs.h"

void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmtItem(ASTNS::VarStmtItem *ast)
{
    std::string varname = ast->name.stringify();
    CodeGen::FunctionCodeGen::Local *var = fcg.getLocal(varname);
    if (var && var->scopenum == fcg.curScope)
    {
        ERR_REDECL_VAR(ast->name, var->v);
        cg.errored = true;
        return;
    }

    IR::Register *reg = fcg.fun->addRegister(varty, ast);

    if (ast->expr)
    {
        IR::ASTValue val = fcg.exprCG.expr(ast->expr.get());
        if (!val)
            return;

        if (val.type() != varty)
        {
            ERR_CONFLICT_VAR_INIT_TY(ast->equal, ast->name, val, reg);
            cg.errored = true;
            return;
        }
        fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(reg, val));
    }

    fcg.addLocal(varname, reg);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmtItemList(ASTNS::VarStmtItemList *ast)
{
    ast->varstmtitemsegment->accept(this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmtItemSegment(ASTNS::VarStmtItemSegment *ast)
{
    ast->varstmtitemsegment->accept(this);
    ast->anothervarstmtitem->accept(this);
}
