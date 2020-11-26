#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::Declarator::Declarator(CodeGen &cg): cg(cg) {}

void CodeGenNS::Declarator::visitDeclList(ASTNS::DeclList *ast)
{
    ast->decllist->accept(this);
    ast->decl->accept(this);
}

void CodeGenNS::Declarator::visitFunction(ASTNS::Function *fun)
{
    std::string fname (fun->name.stringify());
    IR::Value *declbefore = cg.context.findGlobal(fname);

    if (declbefore)
    {
        Error(Error::MsgType::ERROR, fun->name, "redeclaration of symbol")
            .underline(Error::Underline(fun->name, '^')
                .error("redeclaration of symbol"))
            // .underline(Error::Underline(f->defAST(), '-')
                // .note("previous declaration")) // TODO: highlight this, hint make "declaration" class
            .report();
        cg.errored = true;
        return;
    }

    IR::Type *retty = cg.typeResolver.type(fun->retty.get());
    if (!retty)
        return;

    std::vector<CodeGenNS::ParamVisitor::Param> params;
    if (fun->paramlist)
        params = cg.paramVisitor.params(fun->paramlist.get());

    std::vector<IR::Type*> ptys;
    for (CodeGenNS::ParamVisitor::Param const &p : params)
        ptys.push_back(p.ty);

    IR::FunctionType *ft = cg.context.getFunctionType(retty, ptys);
    IR::Function *f = cg.context.unit.addFunction(ft, fname, fun);

    cg.context.globalSymbolTable[fname] = f;
}
