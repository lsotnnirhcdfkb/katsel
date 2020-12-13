#include "codegen/codegen.h"
#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/errmsgs.h"
#include "utils/format.h"

CodeGenNS::DeclCodeGen::DeclCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::DeclCodeGen::visitCU(ASTNS::CU *ast)
{
    if (ast->dl)
        ast->dl->accept(this);
}

void CodeGenNS::DeclCodeGen::visitDeclList(ASTNS::DeclList *ast)
{
    ast->decllist->accept(this);
    ast->anotherdecl->accept(this);
}

void CodeGenNS::DeclCodeGen::visitFunctionDecl(ASTNS::FunctionDecl *ast)
{
    std::string name = ast->name.stringify();
    IR::Value *function = cg.context.findGlobal(name);
    IR::FunctionType *fty;
    if (!(fty = dynamic_cast<IR::FunctionType*>(function->type())))
        reportAbortNoh(format("DeclCodeGen::visitFunction(): context.getGlobal(\"%\") returned non-function", name));

    IR::Function *f = static_cast<IR::Function*>(function);

    if (f->blocks.size() > 0)
        return;

    if (!ast->body)
    {
        f->prototypeonly = true;
        return;
    }

    IR::Block *entryBlock = f->addBlock("entry");
    IR::Block *exitBlock = f->addBlock("exit");

    cg.context.incScope();
    IR::Register *retReg = f->addRegister(fty->ret, ast);

    if (ast->paramlist)
    {
        std::vector<CodeGenNS::ParamVisitor::Param> params = cg.paramVisitor.params(ast->paramlist.get());

        for (auto const &param : params)
        {
            std::string pname = param.name;
            IR::Register *reg = f->addRegister(param.ty, param.ast);

            CodeGenNS::Context::Local *foundparam = cg.context.findLocal(pname);
            if (foundparam)
            {
                ERR_REDECL_PARAM(param.ast->name, foundparam->v);
                cg.errored = true;
            }
            else
                cg.context.addLocal(pname, reg);
        }
    }

    cg.context.curFunc = f;
    cg.context.curBlock = entryBlock;
    cg.context.exitBlock = exitBlock;
    cg.context.retReg = retReg;

    IR::ASTValue ret = cg.exprCodeGen.expr(ast->body.get());

    cg.context.decScope();

    if (!cg.errored)
    {
        cg.context.exitBlock->add(std::make_unique<IR::Instrs::Return>(retReg));

        if (cg.context.curBlock != cg.context.blackHoleBlock.get())
        {
            cg.context.curBlock->add(std::make_unique<IR::Instrs::Store>(retReg, ret));
            cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(cg.context.exitBlock));
        }
    }

    cg.context.curFunc = nullptr;
    cg.context.curBlock = nullptr;
    cg.context.exitBlock = nullptr;
    cg.context.retReg = nullptr;
}
