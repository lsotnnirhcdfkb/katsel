#pragma once

#include "visit/visitor.h"
#include "parse/ast.h"

#include "codegen/context.h"

class GlobalsAssembler : public ProgramVisitor
{
public:
    GlobalsAssembler(CodeGenContext &con);
    virtual void visitProgram(ASTNS::Program *a) override;
    
private:
    CodeGenContext &context;
};
