#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"
#include "ir/type.h"

#include "llvm/IR/Constants.h"

void Lower::Lowerer::visitGotoBr(NNPtr<IR::Instrs::GotoBr> instr) {
    builder.CreateBr(blocks.at(instr->to));
}
void Lower::Lowerer::visitCondBr(NNPtr<IR::Instrs::CondBr> instr) {
    builder.CreateCondBr(lower(instr->v).asRaw(), blocks.at(instr->trueB), blocks.at(instr->falseB));
}
void Lower::Lowerer::visitReturn(NNPtr<IR::Instrs::Return> instr) {
    if (!dynamic_cast<IR::VoidType*>(instr->value.type().asRaw()))
        builder.CreateRet(lower(instr->value).asRaw());
    else
        builder.CreateRet(llvm::ConstantStruct::get(llvm::StructType::get(context)));
}
