#include "codegen/codegen.h"
#include "message/errors.h"

void CodeGenNS::StmtCodeGen::visitVarStmtItem(ASTNS::VarStmtItem *ast)
{
    std::string varname = ast->name.stringify();
    Local *var = cg.context.findLocal(varname);
    if (var && var->scopenum == cg.context.curScope)
    {
        Error(Error::MsgType::ERROR, ast->name, "duplicate variable")
            .underline(Error::Underline(ast->name, '^')
                .error("duplicate variable"))
            .underline(Error::Underline(var->v->ast(), '-')
                .note("previous declaration is here"))
            .report();
        return;
    }

    Register *reg = cg.context.curFunc->addRegister(varty, ast);
    cg.context.addLocal(varname, reg);

    if (ast->expr)
    {
        Value *val = cg.exprCodeGen.expr(ast->expr.get());
        if (!val)
            return;

        if (val->type() != varty)
        {
            Error(Error::MsgType::ERROR, ast->equal, concatMsg("cannot initialize variable of type \"", varty->stringify(), "\" with value of type \"", val->type()->stringify(), "\""))
                .underline(Error::Underline(val, '^')
                    .note(val->type()->stringify()))
                .underline(Error::Underline(ast->name, '^')
                    .note(varty->stringify()))
                .underline(Error::Underline(ast->equal, '-'))
                .report();
            return;
        }
        // TODO: assign
    }
}
void CodeGenNS::StmtCodeGen::visitVarStmtItemList(ASTNS::VarStmtItemList *ast)
{
    ast->varstmtitemlist->accept(this);
    ast->varstmtitem->accept(this);
}
