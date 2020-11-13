#pragma once

#include "visit/visitor.h"
#include "parse/ast.h"

#include "codegen/context.h"
#include "codegen/codegen.h"

class GlobalsAssembler
{
public:
    GlobalsAssembler(CodeGenContext &con, CodeGen &codeGen);

    // GLOBALS METHODS START


    // GLOBALS METHODS END
    
private:
    CodeGenContext &context;
    CodeGen &codeGen;
};
