#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::Declarator::Declarator(CodeGen &cg): cg(cg) {}

void CodeGenNS::Declarator::visitDeclList(ASTNS::DeclList *ast)
{
    ast->decllist->accept(this);
    ast->decl->accept(this);
}

void CodeGenNS::Declarator::visitDecl(ASTNS::Decl *) {}

void CodeGenNS::Declarator::visitFunction(ASTNS::Function *fun)
{
    std::string fname (fun->name.stringify());
    Value declbefore = cg.context.findGlobal(fname);

    if (declbefore.val)
    {
        Error(Error::MsgType::ERROR, fun->name, "Duplicate function")
            .underline(Error::Underline(fun->name, '^')
                .error("Duplciate function"))
            .underline(Error::Underline(declbefore, '-')
                .note("Previous declaration is here"))
            .report();
        return;
    }

    Type *retty = cg.typeResolver.type(fun->retty.get());
    if (!retty)
        return;

    llvm::Type *retl = retty->toLLVMType(cg.context.context);

    std::vector<CodeGenNS::ParamVisitor::Param> params;
    if (fun->paramlist)
        params = cg.paramVisitor.params(fun->paramlist.get());

    std::vector<Type*> ptys;
    std::vector<llvm::Type*> ptyls;
    for (CodeGenNS::ParamVisitor::Param const &p : params)
    {
        Type *ty (p.ty);
        llvm::Type *tyl (p.ty->toLLVMType(cg.context.context));

        ptys.push_back(ty);
        ptyls.push_back(tyl);
    }

    llvm::FunctionType *ft = llvm::FunctionType::get(retl, ptyls, false);
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, fname, cg.context.mod.get());

    cg.context.globalSymbolTable[fname] = Value(cg.context.getFunctionType(retty, ptys), f, fun);
}
