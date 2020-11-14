#include "codegen/codegen.h"

#include "llvm/Support/raw_ostream.h"

CodeGenNS::CodeGen::CodeGen(std::string const &name) : context(name), declarator(*this), typeResolver(*this), paramVisitor(*this), argsVisitor(*this), declCodeGen(*this), stmtCodeGen(*this), exprCodeGen(*this) {}

void CodeGenNS::CodeGen::declarate(ASTNS::Decls *decls)
{
    decls->accept(&declarator);
}

void CodeGenNS::CodeGen::codegen(ASTNS::Decls *decls)
{
    decls->accept(&declCodeGen);
}

void CodeGenNS::CodeGen::printMod()
{
    context.mod->print(llvm::outs(), nullptr);
}

CodeGenNS::ParamVisitor::ParamVisitor::ParamVisitor(CodeGenNS::CodeGen &cg): cg(cg) {}

std::vector<CodeGenNS::ParamVisitor::Param> CodeGenNS::ParamVisitor::params(ASTNS::PListB *ast)
{
    ret = {};
    ast->accept(this);

    return ret;
}

void CodeGenNS::ParamVisitor::visitParamList(ASTNS::ParamList *ast)
{
    if (ast->plist)
        ast->plist->accept(this);

    Type *ty (cg.typeResolver.type(ast->type.get()));
    std::string name (ast->name.stringify());
    CodeGenNS::ParamVisitor::Param p {ty, std::move(name), ast};
    ret.push_back(p);
}

CodeGenNS::ArgsVisitor::ArgsVisitor::ArgsVisitor(CodeGenNS::CodeGen &cg): cg(cg) {}

std::vector<Value> CodeGenNS::ArgsVisitor::args(ASTNS::ArgsB *ast)
{
    ret = {};
    ast->accept(this);

    return ret;
}

void CodeGenNS::ArgsVisitor::visitArgs(ASTNS::Args *ast)
{
    if (ast->args)
        ast->args->accept(this);


    Value v = cg.exprCodeGen.expr(ast->expr.get());
    ret.push_back(v);
}
