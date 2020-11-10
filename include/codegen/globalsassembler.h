#pragma once

#include "visit/visitor.h"
#include "parse/ast.h"

#include "codegen/context.h"
#include "codegen/codegen.h"

class GlobalsAssembler : public ProgramVisitor, public DeclVisitor
{
public:
    GlobalsAssembler(CodeGenContext &con, CodeGen &codeGen);
    virtual void visitProgram(ASTNS::Program *a) override;

    virtual void visitFunctionDecl(ASTNS::FunctionDecl *a) override;
    virtual void visitGlobalVarDecl(ASTNS::GlobalVarDecl *a) override;
    
private:
    CodeGenContext &context;
    CodeGen &codeGen;
};
