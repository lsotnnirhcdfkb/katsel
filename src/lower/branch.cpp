#include "lower/lowerer.h"
#include "lowererlocal.h"

#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"

#include "ir/block.h"

#include "llvm/IR/Constants.h"

void Lower::LowerInstr::visit(IR::Instrs::GotoBr const &instr) {
    fl.builder.CreateBr(&fl.get_block(*instr.to));
}
void Lower::LowerInstr::visit(IR::Instrs::CondBr const &instr) {
    fl.builder.CreateCondBr(&fl.value_ref.lower(*instr.v.val), &fl.get_block(*instr.true_b), &fl.get_block(*instr.false_b));
}
void Lower::LowerInstr::visit(IR::Instrs::Return const &instr) {
    fl.builder.CreateRet(&fl.value_ref.lower(*instr.value.val));
}
