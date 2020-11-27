#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/errmsgs.h"

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

    ASTNS::AST *prevdeclast;
    IR::Function *asf = dynamic_cast<IR::Function*>(declbefore);

    if (asf)
        prevdeclast = asf->defAST();

    if (declbefore)
    {
        ERR_REDECL_SYM(fun->name, declbefore);
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
