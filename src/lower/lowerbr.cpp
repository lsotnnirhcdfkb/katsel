#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"
#include "ir/type.h"

#include "llvm/IR/Constants.h"

void Lower::Lowerer::visitGotoBr(IR::Instrs::GotoBr *instr) {
    builder.CreateBr(blocks.at(instr->to));
}
void Lower::Lowerer::visitCondBr(IR::Instrs::CondBr *instr) {
    builder.CreateCondBr(lower(instr->v), blocks.at(instr->trueB), blocks.at(instr->falseB));
}
void Lower::Lowerer::visitReturn(IR::Instrs::Return *instr) {
    if (!dynamic_cast<IR::VoidType*>(instr->value.type()))
        builder.CreateRet(lower(instr->value));
    else
        builder.CreateRet(llvm::ConstantStruct::get(llvm::StructType::get(context)));
}
