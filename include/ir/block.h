#pragma once

#include <vector>
#include <memory>
#include <string>
#include <ostream>


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
        void add(std::unique_ptr<Instrs::Instruction> instr);
        void branch(std::unique_ptr<Instrs::Br> br);

        void stringify(std::ostream &os);
        void definition(std::ostream &os);
        void cfgDot(std::ostream &os);

        std::string name;
        size_t num;

        std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
        std::unique_ptr<Instrs::Br> br;
    };
}
