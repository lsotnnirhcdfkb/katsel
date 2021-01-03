#include "../codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ImplCodeGen::ImplCodeGen(CodeGen &cg, ASTNS::ImplDecl *ast): cg(cg), ast(ast), errored(false) {}

bool CodeGen::ImplCodeGen::codegen() {
    // TODO
}
