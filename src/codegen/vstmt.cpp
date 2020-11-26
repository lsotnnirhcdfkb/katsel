#include "codegen/codegen.h"
#include "message/errors.h"

void CodeGenNS::StmtCodeGen::visitVarStmtItem(ASTNS::VarStmtItem *ast)
{
    std::string varname = ast->name.stringify();
    CodeGenNS::Context::Local *var = cg.context.findLocal(varname);
    if (var && var->scopenum == cg.context.curScope)
    {
        Error(Error::MsgType::ERROR, ast->name, "duplicate variable")
            .underline(Error::Underline(ast->name, '^')
                .error("duplicate variable"))
            .underline(Error::Underline(var->v->defAST(), '-')
                .note("previous declaration"))
            .report();
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
            Error(Error::MsgType::ERROR, ast->equal, concatMsg("cannot initialize variable of type \"", varty->stringify(), "\" with value of type \"", val.type()->stringify(), "\""))
                .underline(Error::Underline(val, '^')
                    .note(val.type()->stringify()))
                .underline(Error::Underline(ast->name, '^')
                    .note(varty->stringify()))
                .underline(Error::Underline(ast->equal, '-'))
                .report();
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
