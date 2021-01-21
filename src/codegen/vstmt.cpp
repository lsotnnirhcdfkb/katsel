#include "codegenlocal.h"
#include "message/errmsgs.h"

void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmtItem(NNPtr<ASTNS::VarStmtItem> ast) {
    std::string varname = ast->name.stringify();
    Maybe<NNPtr<CodeGen::FunctionCodeGen::Local>> var = fcg.getLocal(varname);
    if (var.has() && var.get()->scopenum == fcg.curScope) {
        ERR_REDECL_VAR(ast->name, var.get()->v);
        cg.errored = true;
        return;
    }

    Maybe<NNPtr<IR::Type>> m_varType = cg.typeVisitor->type(ast->type.get(), fcg.thisType);
    if (!m_varType.has()) {
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Type> varType = m_varType.get();

    NNPtr<IR::Instrs::Register> reg = NNPtr<IR::Instrs::Register>(static_cast<IR::Instrs::Register*>(fcg.registerBlock->add(std::make_unique<IR::Instrs::Register>(ast, varType, ast->mut)).asRaw()));

    if (ast->expr) {
        Maybe<IR::ASTValue> m_val = fcg.exprCG.expr(ast->expr.get());
        if (!m_val.has())
            return;

        IR::ASTValue val = m_val.get();

        val = varType->implCast(*cg.context, *fcg.fun, fcg.curBlock, val);
        if (val.type() != varType) {
            ERR_CONFLICT_VAR_INIT_TY(ast->equal, ast->name, ast->type.get(), val, varType);
            fcg.errored = true;
            return;
        }
        fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(reg, ast), val, true));
    } else if (!ast->mut) // no initializer, not mutable
        WARN_IMMUT_NOINIT(ast);

    fcg.addLocal(varname, reg);
}
