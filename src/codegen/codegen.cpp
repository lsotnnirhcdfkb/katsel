#include "codegen/codegen.h"

#include <iostream>
#include "llvm/Support/raw_ostream.h"

CodeGenNS::CodeGen::CodeGen(std::string const &name) : context(name), declarator(*this), typeResolver(*this), paramVisitor(*this), argsVisitor(*this), declCodeGen(*this), stmtCodeGen(*this), exprCodeGen(*this) {}

void CodeGenNS::CodeGen::declarate(ASTNS::DeclB *decls)
{
    decls->accept(&declarator);
}

void CodeGenNS::CodeGen::codegen(ASTNS::DeclB *decls)
{
    decls->accept(&declCodeGen);
}

void CodeGenNS::CodeGen::printMod(std::ostream &ostream)
{
    context.mod->print(llvm::outs(), nullptr);
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
    Type *ty (cg.typeResolver.type(ast->type.get()));
    std::string name (ast->name.stringify());
    CodeGenNS::ParamVisitor::Param p {ty, std::move(name), ast};
    ret.push_back(p);
}

void CodeGenNS::ParamVisitor::visitParamList(ASTNS::ParamList *ast)
{
    ast->paramlist->accept(this);
    ast->param->accept(this);
}

CodeGenNS::ArgsVisitor::ArgsVisitor::ArgsVisitor(CodeGenNS::CodeGen &cg): cg(cg) {}

std::vector<Value> CodeGenNS::ArgsVisitor::args(ASTNS::ArgB *ast)
{
    ret.clear();
    ast->accept(this);

    return ret;
}

void CodeGenNS::ArgsVisitor::visitArgList(ASTNS::ArgList *ast)
{
    std::vector<Value> cret (args(ast->arglist.get())); // cannot do like params becasue args can be nested (through nested function calls) and that messes things up

    ast->arg->accept(this);
    cret.reserve(cret.size() + ret.size());
    cret.insert(cret.end(), ret.begin(), ret.end());

    ret = std::move(cret);
}

void CodeGenNS::ArgsVisitor::visitArg(ASTNS::Arg *ast)
{
    Value v = cg.exprCodeGen.expr(ast->expr.get());
    ret = {v};
}
