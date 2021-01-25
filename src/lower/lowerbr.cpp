#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"
#include "ir/type.h"

#include "llvm/IR/Constants.h"

void Lower::Lowerer::visit(IR::Instrs::GotoBr const &instr) {
    builder.CreateBr(blocks.at(instr.to));
}
void Lower::Lowerer::visit(IR::Instrs::CondBr const &instr) {
    builder.CreateCondBr(&lower(instr.v), blocks.at(instr.true_b), blocks.at(instr.false_b));
}
void Lower::Lowerer::visit(IR::Instrs::Return const &instr) {
    if (!dynamic_cast<IR::VoidType const *>(&instr.value.type()))
        builder.CreateRet(&lower(instr.value));
    else
        builder.CreateRet(llvm::ConstantStruct::get(llvm::StructType::get(context)));
}
