#include "codegen/globalsassembler.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include "message/errors.h"

#include <iostream>

GlobalsAssembler::GlobalsAssembler(CodeGenContext &con, CodeGen &codeGen): context(con), codeGen(codeGen) {}

void GlobalsAssembler::visitProgram(ASTNS::Program *a)
{
    for (std::unique_ptr<ASTNS::Decl> &d : a->decls)
        d->accept(this);
}

void GlobalsAssembler::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    std::string fnamestr (a->name.stringify());
    Value declbefore = context.getGlobal(fnamestr);
    if (declbefore.val)
    {
        Error(Error::MsgType::ERROR, a->name, "Duplicate function")
            .primary(Error::Primary(a->name)
                .error("Duplciate function"))
            .primary(Error::Primary(declbefore)
                .note("Previous declaration is here"))
            .report();
        return;
    }

    std::vector<Type*> paramtys;
    std::vector<llvm::Type*> paramtysllvm;
    Type *ret = codeGen.evalType(a->rettype.get());

    {
        ASTNS::Param *p = a->params.get();
        while (p)
        {
            Type *ty = codeGen.evalType(p->type.get());

            paramtys.push_back(ty);
            paramtysllvm.push_back(ty->toLLVMType(context.context));

            p = p->next.get();
        }
    }

    llvm::Type *retTyllvm = ret->toLLVMType(context.context);
    llvm::FunctionType *ft = llvm::FunctionType::get(retTyllvm, paramtysllvm, false);
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, a->name.stringify(), context.mod.get());

    context.globalSymbolTable[fnamestr] = Value(context.getFunctionType(ret, paramtys), f, a);
}

void GlobalsAssembler::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    Error(Error::MsgType::INTERR, a, "Global Variable Declarations are not supported yet")
        .primary(Error::Primary(a)
                .error("global variable declaration")
                .note("coming soon!"))
        .reportAbort();;
    // TODO: this
}
