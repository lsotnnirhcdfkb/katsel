#include "codegen/codegen.h"
#include "utils/assert.h"

#include <iostream>

CodeGenNS::CodeGen::CodeGen(File const &file) : context(file), declarator(*this), typeResolver(*this), paramVisitor(*this), argsVisitor(*this), declCodeGen(*this), stmtCodeGen(*this), exprCodeGen(*this), errored(false) {}

void CodeGenNS::CodeGen::declarate(ASTNS::CUB *decls)
{
    decls->accept(&declarator);
}

void CodeGenNS::CodeGen::codegen(ASTNS::CUB *decls)
{
    decls->accept(&declCodeGen);
}

void CodeGenNS::CodeGen::printUnit(llvm::raw_ostream &ostream)
{
    context.unit.print(ostream);
}

CodeGenNS::ParamVisitor::ParamVisitor::ParamVisitor(CodeGenNS::CodeGen &cg): cg(cg) {}

std::vector<CodeGenNS::ParamVisitor::Param> CodeGenNS::ParamVisitor::params(ASTNS::PListB *ast)
{
    ret.clear();
    ast->accept(this);

    return ret;
}

void CodeGenNS::ParamVisitor::visitParam(ASTNS::Param *ast)
{
    IR::Type *ty (cg.typeResolver.type(ast->type.get()));
    std::string name (ast->name.stringify());
    Param p {ty, std::move(name), ast};
    ret.push_back(p);
}

void CodeGenNS::ParamVisitor::visitParamList(ASTNS::ParamList *ast)
{
    ast->paramlist->accept(this);
    ast->anotherparam->accept(this);
}
void CodeGenNS::ParamVisitor::visitParamList_OPT(ASTNS::ParamList_OPT *ast) {}

CodeGenNS::ArgsVisitor::ArgsVisitor::ArgsVisitor(CodeGenNS::CodeGen &cg): cg(cg) {}

std::vector<IR::ASTValue> CodeGenNS::ArgsVisitor::args(ASTNS::ArgB *ast)
{
    ret.clear();
    ast->accept(this);

    return ret;
}

void CodeGenNS::ArgsVisitor::visitArgList(ASTNS::ArgList *ast)
{
    std::vector<IR::ASTValue> cret (args(ast->arglist.get()));

    std::vector<IR::ASTValue> a (args(ast->anotherarg.get()));
    ASSERT(a.size() == 1);
    cret.push_back(a[0]);

    ret = std::move(cret);
}

void CodeGenNS::ArgsVisitor::visitArg(ASTNS::Arg *ast)
{
    IR::ASTValue v = cg.exprCodeGen.expr(ast->expr.get());
    ret = {v};
}

void CodeGenNS::ArgsVisitor::visitArgList_OPT(ASTNS::ArgList_OPT *ast) {}
