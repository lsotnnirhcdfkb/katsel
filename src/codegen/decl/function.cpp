#include "../codegenlocal.h"
#include "utils/format.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::FunctionCodeGen::FunctionCodeGen(CodeGen &cg, ASTNS::FunctionDecl *ast, IR::Function *fun): curScope(0), cg(cg), ast(ast), exprCG(cg, *this), stmtCG(cg, *this), fun(fun), errored(false) {}

bool CodeGen::FunctionCodeGen::codegen() {
    std::string name = ast->name.stringify();

    IR::FunctionType *fty = fun->ty;

    if (!ast->body) {
        fun->prototypeonly = true;
        return true;
    }

    IR::Block *entryBlock = fun->addBlock("entry");
    exitBlock = fun->addBlock("exit");

    incScope();
    ret = static_cast<IR::Instrs::Register*>(entryBlock->add(std::make_unique<IR::Instrs::Register>(cg.unit->implicitDeclAST.get(), fty->ret)));
    if (ast->params) {
        ParamVisitor pv (cg);
        ast->params->accept(&pv);
        std::vector<ParamVisitor::Param> params (pv.ret);

        for (auto const &param : params) {
            std::string pname = param.name;
            IR::Instrs::Register *reg = static_cast<IR::Instrs::Register*>(entryBlock->add(std::make_unique<IR::Instrs::Register>(param.ast, param.ty)));

            Local *foundparam = getLocal(pname);
            if (foundparam) {
                ERR_REDECL_PARAM(param.ast->name, foundparam->v);
                errored = true;
            } else
                addLocal(pname, reg);
        }
    }

    curBlock = entryBlock;

    IR::ASTValue retval = exprCG.expr(ast->body.get());

    decScope();

    if (!errored) {
        IR::Instrs::Instruction *derefRetReg = exitBlock->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(ret, ast->retty.get())));
        exitBlock->branch(std::make_unique<IR::Instrs::Return>(IR::ASTValue(derefRetReg, ast->retty.get())));

        retval = fun->ty->ret->implCast(*cg.context, *fun, curBlock, retval);
        if (fun->ty->ret != retval.type()) {
            ERR_CONFLICT_RET_TY(retval, fun);
            errored = true;
        } else {
            curBlock->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(ret, ast->retty.get()), retval));
            curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(exitBlock));
        }
    }

    if (curScope != 0 && !errored)
        reportAbortNoh("At the end of FunctionCodeGen::codegen, curScope != 0");

    return !errored;
}

void CodeGen::FunctionCodeGen::addLocal(std::string const &name, IR::Instrs::Register *val) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name && last->scopenum == curScope)
            reportAbortNoh(format("duplicate local added: \"%\"", name));

    Local l {curScope, val, name};
    locals.push_back(l);
}

CodeGen::FunctionCodeGen::Local* CodeGen::FunctionCodeGen::getLocal(std::string const &name) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return &*last;

    return nullptr;
}

void CodeGen::FunctionCodeGen::incScope() {
    ++curScope;
}
void CodeGen::FunctionCodeGen::decScope() {
    --curScope;
    while (locals.size() && locals.back().scopenum > curScope) locals.pop_back();
}
