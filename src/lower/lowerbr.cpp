#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"
#include "ir/type.h"

#include "llvm/IR/Constants.h"

void Lower::Lowerer::visit_goto_br(IR::Instrs::GotoBr &instr) {
    builder.CreateBr(blocks.at(instr.to));
}
void Lower::Lowerer::visit_cond_br(IR::Instrs::CondBr &instr) {
    builder.CreateCondBr(lower(instr.v).as_raw(), blocks.at(instr.true_b), blocks.at(instr.false_b));
}
void Lower::Lowerer::visit_return(IR::Instrs::Return &instr) {
    if (!dynamic_cast<IR::VoidType*>(instr.value.type().as_raw()))
        builder.CreateRet(lower(instr.value).as_raw());
    else
        builder.CreateRet(llvm::ConstantStruct::get(llvm::StructType::get(context)));
}
