#include "../codegenlocal.h"
#include "utils/format.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"

CodeGen::FunctionCodeGen::FunctionCodeGen(CodeGen &cg, NNPtr<ASTNS::FunctionDecl> ast, NNPtr<IR::Function> fun, Maybe<NNPtr<IR::Type>> thisType):
    curScope(0),
    cg(cg),
    ast(ast),
    exprCG(cg, *this),
    stmtCG(cg, *this),
    fun(fun),
    registerBlock(fun->addBlock("registers")),
    entryBlock(fun->addBlock("entry")),
    exitBlock(fun->addBlock("exit")),
    curBlock(entryBlock),
    ret(static_cast<IR::Instrs::Register*>(registerBlock->add(std::make_unique<IR::Instrs::Register>(cg.unit->implicitDeclAST.get(), fun->ty->ret, true)).asRaw())),
    thisType(thisType),
    errored(false) {}

bool CodeGen::FunctionCodeGen::codegen() {
    if (!ast->body) {
        fun->prototypeonly = true;
        return true;
    }

    registerBlock->branch(std::make_unique<IR::Instrs::GotoBr>(entryBlock));

    incScope();
    ParamVisitor pv (cg, ast->params, thisType);
    std::vector<ParamVisitor::Param> params (pv.ret);

    for (auto const &param : params) {
        std::string pname = param.name;
        NNPtr<IR::Instrs::Register> reg = static_cast<IR::Instrs::Register*>(registerBlock->add(std::make_unique<IR::Instrs::Register>(param.ast, param.ty, param.mut)).asRaw());

        Maybe<NNPtr<Local>> foundparam = getLocal(pname);
        if (foundparam.has()) {
            ERR_REDECL_PARAM(param.ast, foundparam.get()->v);
            errored = true;
        } else
            addLocal(pname, reg);
    }

    Maybe<IR::ASTValue> m_retval = exprCG.expr(ast->body.get());

    decScope();

    if (!errored) {
        NNPtr<IR::Instrs::Instruction> derefRetReg = exitBlock->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(ret, ast->retty.get())));
        exitBlock->branch(std::make_unique<IR::Instrs::Return>(IR::ASTValue(derefRetReg, ast->retty.get())));

        if (!m_retval.has()) {
            errored = true;
        } else {
            IR::ASTValue retval = m_retval.get();

            retval = fun->ty->ret->implCast(*cg.context, *fun, curBlock, retval);
            if (fun->ty->ret != retval.type()) {
                ERR_CONFLICT_RET_TY(retval, fun);
                errored = true;
            } else {
                curBlock->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(ret, ast->retty.get()), retval, false));
                curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(exitBlock));
            }
        }
    }

    if (curScope != 0 && !errored)
        reportAbortNoh("At the end of FunctionCodeGen::codegen, curScope != 0");

    return !errored;
}

void CodeGen::FunctionCodeGen::addLocal(std::string const &name, NNPtr<IR::Instrs::Register> val) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name && last->scopenum == curScope)
            reportAbortNoh(format("duplicate local added: \"{}\"", name));

    Local l {curScope, val, name};
    locals.push_back(l);
}

Maybe<NNPtr<CodeGen::FunctionCodeGen::Local>> CodeGen::FunctionCodeGen::getLocal(std::string const &name) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return *last;

    return Maybe<NNPtr<Local>>();
}

void CodeGen::FunctionCodeGen::incScope() {
    ++curScope;
}
void CodeGen::FunctionCodeGen::decScope() {
    --curScope;
    while (locals.size() && locals.back().scopenum > curScope) locals.pop_back();
}
