#pragma once

#include "visit/visitor.h"
#include "parse/ast.h"

#include "codegen/context.h"
#include "value/value.h"
#include "typing/type.h"

#include "llvm/IR/Type.h"

#define CG_RETURNNULL() do \
    { \
        exprRetVal = Value(); \
        typeRetVal = nullptr; \
        return; \
    } while (false)

class CodeGen
{
public:
    CodeGen(CodeGenContext &context);

    Value evalExpr(ASTNS::AST *a);
    Type* evalType(ASTNS::AST *a);

    struct Param
    {
        Type *ty;
        Token name;
    };

    Param evalParam(ASTNS::AST *a);
    std::vector<Param> evalParams(ASTNS::AST *a);

private:
    CodeGenContext &context;

    Value exprRetVal;
    Type *typeRetVal;
    Param paramRetVal;
    std::vector<Param> paramsRetVal;
};

