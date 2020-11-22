#include "codegen/codegen.h"
#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::DeclCodeGen::DeclCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::DeclCodeGen::visitDeclList(ASTNS::DeclList *ast)
{
    ast->decllist->accept(this);
    ast->decl->accept(this);
}

void CodeGenNS::DeclCodeGen::visitFunction(ASTNS::Function *ast)
{
    std::string name = ast->name.stringify();
    Value *function = cg.context.findGlobal(name);
    if (!dynamic_cast<FunctionType*>(function->type()))
        reportAbortNoh(concatMsg("DeclCodeGen::visitFunction(): context.getGlobal\"", name, "\") returned non-function"));

    Function *f = static_cast<Function*>(function);

    if (f->blocks.size() > 0)
        return;

    Block *entryBlock = f->addBlock("entry");
    Block *exitBlock = f->addBlock("exit");

    cg.context.incScope();

    if (ast->paramlist)
    {
        std::vector<Param> params = cg.paramVisitor.params(ast->paramlist.get());

        for (auto const &param : params)
        {
            std::string pname = param.name;
            Register *reg = f->addRegister(param.ty, param.ast);
            cg.context.addLocal(pname, reg);
        }
    }

    cg.context.curFunc = f;
    cg.context.curBlock = entryBlock;
    cg.context.exitBlock = exitBlock;

    cg.stmtCodeGen.stmt(ast->body.get());

    cg.context.decScope();
    cg.context.curFunc = nullptr;
    cg.context.exitBlock = nullptr;
}
