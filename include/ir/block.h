#pragma once

#include <vector>
#include <memory>
#include <string>

#include "utils/ptr.h"
#include "ir/function.h"
#include "ir/instruction.h"

namespace llvm { class raw_ostream; }
namespace IR {
    namespace Instrs {
        class Instruction;
        class Br;
    }

    class Function;

    class Block {
    public:
        Block(NNPtr<Function> fun, std::string name, size_t num);

        template <typename T, typename = std::enable_if_t<std::is_base_of_v<Instrs::Instruction, T>>>
        T& add(std::unique_ptr<T> instr) {
            instr->id = fun->curindex++;
            T& raw = *instr;
            instructions.push_back(std::move(instr));
            return raw;
        }

        void branch(std::unique_ptr<Instrs::Br> br);

        std::string name;
        size_t num;

        std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
        std::unique_ptr<Instrs::Br> br;

        NNPtr<IR::Function> fun;
    };
}
