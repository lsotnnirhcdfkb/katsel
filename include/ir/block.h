#pragma once

#include <vector>
#include <memory>
#include <string>

namespace llvm { class raw_ostream; }
namespace IR {
    namespace Instrs {
        class Instruction;
        class Br;
    }
    class Function;

    class Block {
    public:
        Block(IR::Function *fun, std::string name, size_t num);
        Instrs::Instruction* add(std::unique_ptr<Instrs::Instruction> instr);
        void branch(std::unique_ptr<Instrs::Br> br);

        std::string name;
        size_t num;

        std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
        std::unique_ptr<Instrs::Br> br;

        IR::Function *fun;
    };
}
