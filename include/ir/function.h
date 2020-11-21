#pragma once

#include "ir/value/value.h"
#include "ir/type.h"
#include "ir/instruction.h"

#include <string>
#include <vector>
#include <memory>

class Function
{
public:
    Function(FunctionType *ty, std::string name);

    void add(std::unique_ptr<Instruction> instr);

private:
    std::vector<std::unique_ptr<Instruction>> instructions;
    FunctionType *ty;
    std::string name;
};
