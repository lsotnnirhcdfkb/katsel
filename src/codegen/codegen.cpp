#include "codegen/codegen.h"

CodeGen::CodeGen(CodeGenContext &con): context(con) {}

Value CodeGen::evalExpr(ASTNS::Expr *a)
{
    exprRetVal = Value {nullptr, nullptr};
    a->accept(this);
    return exprRetVal;
}

Type* CodeGen::evalType(ASTNS::Type *a)
{
    typeRetVal = nullptr;
    a->accept(this);
    return typeRetVal;
}
