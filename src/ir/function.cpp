#include "ir/function.h"

Function::Function(FunctionType *ty, std::string name): ty(ty), name(name) {}

void Function::add(std::unique_ptr<Instruction> instr)
{
    instructions.push_back(std::move(instr));
}
