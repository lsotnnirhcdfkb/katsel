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
            .underline(Error::Underline(var->v.ast, '-')
                .note("previous declaration is here"))
            .report();
        return;
    }

    llvm::Function *f = cg.context.builder.GetInsertBlock()->getParent();
    llvm::AllocaInst *alloca = cg.context.createEntryAlloca(f, varty->toLLVMType(cg.context.context), varname);
    cg.context.addLocal(varname, varty, alloca, ast);

    if (ast->expr)
    {
        Value val = cg.exprCodeGen.expr(ast->expr.get());
        if (val.type != varty)
        {
            Error(Error::MsgType::ERROR, ast->equal, "assignment operands are of different types")
                .underline(Error::Underline(val, '^')
                    .note(val.type->stringify()))
                .underline(Error::Underline(ast->name, '^')
                    .note(varty->stringify()))
                .underline(Error::Underline(ast->equal, '-'))
                .report();
            return;
        }
        cg.context.builder.CreateStore(val.val, alloca);
    }
}
void CodeGenNS::StmtCodeGen::visitVarStmtItems(ASTNS::VarStmtItems *ast)
{
    ast->items->accept(this);
    ast->item->accept(this);
}
