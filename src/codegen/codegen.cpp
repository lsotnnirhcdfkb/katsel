#include "codegen/codegen.h"

#include "llvm/Support/raw_ostream.h"

CodeGenNS::CodeGen::CodeGen(std::string const &name) : context(name), declarator(*this), typeResolver(*this), paramVisitor(*this), declCodeGen(*this) {}

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
    Type *ty (cg.typeResolver.type(ast->type.get()));
    std::string name (ast->name.stringify());
    CodeGenNS::ParamVisitor::Param p {ty, std::move(name), ast};
    ret.push_back(p);
}
