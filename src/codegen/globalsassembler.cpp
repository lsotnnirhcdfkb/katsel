#include "codegen/globalsassembler.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include "message/errors.h"

#include <iostream>

GlobalsAssembler::GlobalsAssembler(CodeGenContext &con, CodeGen &codeGen): context(con), codeGen(codeGen) {}

void GlobalsAssembler::visitDecls(ASTNS::Decls *a)
{
    a->decls->accept(this);
    a->decl->accept(this);
}

void GlobalsAssembler::visitFunction(ASTNS::Function *a)
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

    Type *ret = codeGen.evalType(a->retty.get());

    std::vector<CodeGen::Param> params;
    if (a->form == ASTNS::Function::Form::FUN_RETTY_NAME_OPARN_PARAMLIST_CPARN_BODY)
        params = codeGen.evalParams(a->paramlist.get());

    std::vector<llvm::Type*> paramtysasllvm;
    std::vector<Type*> paramtys;

    for (CodeGen::Param p : params)
    {
        paramtys.push_back(p.ty);
        paramtysasllvm.push_back(p.ty->toLLVMType(context.context));
    }

    llvm::Type *retTyllvm = ret->toLLVMType(context.context);
    llvm::FunctionType *ft = llvm::FunctionType::get(retTyllvm, paramtysasllvm, false);
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, a->name.stringify(), context.mod.get());

    context.globalSymbolTable[fnamestr] = Value(context.getFunctionType(ret, paramtys), f, a);
}

