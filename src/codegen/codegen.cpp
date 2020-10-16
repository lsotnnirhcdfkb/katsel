#include "codegen/codegen.h"

CodeGen::CodeGen(CodeGenContext &con): context(con) {}

Value CodeGen::evalExpr(ASTNS::Expr *a)
{
    a->accept(this);
    return exprRetVal;
}

Type* CodeGen::evalType(ASTNS::Type *a)
{
    a->accept(this);
    return typeRetVal;
}
