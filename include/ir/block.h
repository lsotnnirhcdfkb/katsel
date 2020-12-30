#pragma once

#include <vector>
#include <memory>
#include <string>
#include "llvm/Support/raw_ostream.h"


namespace IR
{
    namespace Instrs
    {
        class Instruction;
        class Br;
    }

    class Block
    {
    public:
        Block(std::string name, size_t num);
        Instrs::Instruction* add(std::unique_ptr<Instrs::Instruction> instr);
        void branch(std::unique_ptr<Instrs::Br> br);

        void stringify(llvm::raw_ostream &os);
        void definition(llvm::raw_ostream &os);
        void cfgDot(llvm::raw_ostream &os);

        std::string name;
        size_t num;

        std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
        std::unique_ptr<Instrs::Br> br;
    };
}
