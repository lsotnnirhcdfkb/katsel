#include "codegen/codegen.h"

CodeGen::CodeGen(CodeGenContext &con): context(con) {}

llvm::Value* CodeGen::evalExpr(ASTNS::Expr *a)
{
    a->accept(this);
    return exprRetVal;
}
