#pragma once

#include "visit/visitor.h"
#include "parse/ast.h"

#include "codegen/context.h"

class GlobalsAssembler : public ProgramVisitor, public DeclVisitor
{
public:
    GlobalsAssembler(CodeGenContext &con);
    virtual void visitProgram(ASTNS::Program *a) override;

    virtual void visitFunctionDecl(ASTNS::FunctionDecl *a) override;
    virtual void visitGlobalVarDecl(ASTNS::GlobalVarDecl *a) override;
    
private:
    CodeGenContext &context;
};
