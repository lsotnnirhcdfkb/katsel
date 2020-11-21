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

    Block *block = f->addBlock("entry");

    cg.context.incScope();

    // if (ast->paramlist) TODO!
    // {
    //     std::vector<CodeGenNS::ParamVisitor::Param> params = cg.paramVisitor.params(ast->paramlist.get());
    //     auto cparam = params.begin();
    //     for (auto &param : f->args())
    //     {
    //         std::string pname = cparam->name;
    //         llvm::AllocaInst *alloca = cg.context.createEntryAlloca(f, param.getType(), pname);

    //         cg.context.builder.CreateStore(&param, alloca);

    //         cg.context.addLocal(pname, cparam->ty, alloca, cparam->ast);

    //         ++cparam;
    //     }
    // }

    cg.context.curFunc = f;

    cg.stmtCodeGen.stmt(ast->body.get());

    cg.context.decScope();
    cg.context.curFunc = nullptr;
}
