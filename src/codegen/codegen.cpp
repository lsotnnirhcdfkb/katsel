#include "codegen/codegen.h"

CodeGen::CodeGen(CodeGenContext &con): context(con) {}

Value CodeGen::evalExpr(ASTNS::AST *a)
{
    exprRetVal = Value();
    a->accept(this);
    return exprRetVal;
}

Type* CodeGen::evalType(ASTNS::AST *a)
{
    typeRetVal = nullptr;
    a->accept(this);
    return typeRetVal;
}

