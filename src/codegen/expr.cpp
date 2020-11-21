#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

Value* CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    return nullptr;
}
