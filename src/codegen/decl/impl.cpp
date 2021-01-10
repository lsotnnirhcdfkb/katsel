#include "../codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ImplCodeGen::ImplCodeGen(CodeGen &cg, ASTNS::ImplDecl *ast): cg(cg), ast(ast), errored(false) {}

bool CodeGen::ImplCodeGen::codegen() {
    implFor = cg.typeVisitor->type(ast->implFor.get(), nullptr);
    for (std::unique_ptr<ASTNS::ImplItem> &item : ast->items)
        item->accept(this);
    return !errored;
}

void CodeGen::ImplCodeGen::visitFunctionImplItem(ASTNS::FunctionImplItem *ast) {
    if (!implFor) {
        errored = true;
        return;
    }

    IR::Value *val = implFor->getValue(ast->fun->name.stringify());
    IR::Function *fun = dynamic_cast<IR::Function*>(val);
    if (!fun) {
        errored = true;
        return;
    }
    FunctionCodeGen fcg (cg, ast->fun.get(), fun, implFor);
    if (!fcg.codegen())
        errored = true;
}
