#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::DeclCodeGen::DeclCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::DeclCodeGen::visitDecl(ASTNS::Decl *) {}
void CodeGenNS::DeclCodeGen::visitDeclList(ASTNS::DeclList *ast)
{
    ast->decllist->accept(this);
    ast->decl->accept(this);
}

void CodeGenNS::DeclCodeGen::visitFunction(ASTNS::Function *ast)
{
    std::string name = ast->name.stringify();
    Value function = cg.context.findGlobal(name);
    if (!dynamic_cast<FunctionType*>(function.type))
        reportAbortNoh(concatMsg("DeclCodeGen::visitFunction: context.getGlobal\"", name, "\") returned non-function"));

    llvm::Value *fv = function.val;
    llvm::Function *f = static_cast<llvm::Function*>(fv);

    if (!f->empty())
        return;

    llvm::BasicBlock *block = llvm::BasicBlock::Create(cg.context.context, name + "Entry", f);
    cg.context.builder.SetInsertPoint(block);

    cg.context.incScope();

    if (ast->paramlist)
    {
        std::vector<CodeGenNS::ParamVisitor::Param> params = cg.paramVisitor.params(ast->paramlist.get());
        auto cparam = params.begin();
        for (auto &param : f->args())
        {
            std::string pname = cparam->name;
            llvm::AllocaInst *alloca = cg.context.createEntryAlloca(f, param.getType(), pname);

            cg.context.builder.CreateStore(&param, alloca);

            cg.context.addLocal(pname, cparam->ty, alloca, cparam->ast);

            ++cparam;
        }
    }

    cg.context.curFunc = function;
    cg.stmtCodeGen.stmt(ast->body.get()); // TODO
    llvm::verifyFunction(*f);

    cg.context.decScope();
    cg.context.curFunc = Value();
}
