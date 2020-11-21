#include "codegen/codegen.h"
#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::DeclCodeGen::DeclCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::DeclCodeGen::visitDecl(ASTNS::Decl *) {}
void CodeGenNS::DeclCodeGen::visitDeclList(ASTNS::DeclList *ast) {}

void CodeGenNS::DeclCodeGen::visitFunction(ASTNS::Function *ast) {}
