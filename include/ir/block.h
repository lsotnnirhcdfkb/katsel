#pragma once

#include <vector>
#include <memory>
#include <string>

#include "utils/ptr.h"
#include "ir/instruction.h"

namespace llvm { class raw_ostream; }
namespace IR {
    class Register;
    class Function;

    class Block {
    public:
        Block(NNPtr<Function> fun, std::string name, size_t num);

        template <typename I, typename ... Args,
                  typename = std::enable_if_t<std::is_base_of_v<Instruction, I>>>
        I& add(Args && ...args) {
            std::unique_ptr<I> instr = std::make_unique<I>(std::forward<Args>(args)...);
            I& raw = *instr;
            raw.id = instr_index++;
            instructions.push_back(std::move(instr));
            return raw;
        }
        void branch(std::unique_ptr<Br> br);

        std::string name;
        size_t num;

        std::vector<std::unique_ptr<Instruction>> instructions;
        std::unique_ptr<Br> br;

        NNPtr<IR::Function> fun;

    private:
        size_t instr_index;
    };
}
