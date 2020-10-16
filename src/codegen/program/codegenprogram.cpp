#include "codegen/codegen.h"

void CodeGen::visitProgram(ASTNS::Program *a)
{
    for (std::unique_ptr<ASTNS::Decl> &d : a->decls)
        d->accept(this);
}
