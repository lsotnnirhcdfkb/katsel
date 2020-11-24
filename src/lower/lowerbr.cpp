#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"

void Lower::Lowerer::visitGotoBr(IR::Instrs::GotoBr *instr)
{
    builder.CreateBr(blocks.at(instr->to));
}
void Lower::Lowerer::visitCondBr(IR::Instrs::CondBr *instr)
{
    builder.CreateCondBr(lower(instr->v), blocks.at(instr->trueB), blocks.at(instr->falseB));
}
