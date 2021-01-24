#pragma once

#include <vector>
#include <memory>
#include <string>

#include "utils/ptr.h"

namespace llvm { class raw_ostream; }
namespace IR {
    namespace Instrs {
        class Instruction;
        class Br;
    }
    class Function;

    class Block {
    public:
        Block(NNPtr<IR::Function> fun, std::string name, size_t num);
        NNPtr<Instrs::Instruction> add(std::unique_ptr<Instrs::Instruction> instr);
        void branch(std::unique_ptr<Instrs::Br> br);

        std::string name;
        size_t num;

        std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
        std::unique_ptr<Instrs::Br> br;

        NNPtr<IR::Function> fun;
    };
}
