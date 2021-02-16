#pragma once

#include <memory>

#include "ast/astfwd.h"
#include "utils/ptr.h"
#include "ir/value.h"

#include "utils/location.h"

namespace llvm { class raw_ostream; }

namespace IR {
    class Block;
    class FunctionType;
    class Register;
    class Type;

    class Function : public Value, public DeclaredValue {
    public:
        struct Param {
            NNPtr<IR::Type const> ty;
            std::string name;
            NNPtr<ASTNS::ParamB> ast;
            bool mut;
        };

        Function(NNPtr<FunctionType> ty, std::string const &name, Span const &def_span, std::vector<Param> const &params);

        void add(std::unique_ptr<Block> block);

        void definition(llvm::raw_ostream &os) const;
        Span const &def_span() const override;

        Type const &type() const override;

        std::vector<std::unique_ptr<Block>> blocks;
        std::vector<std::unique_ptr<Register>> registers;

        Block &add_block(std::string const &name);
        Register &add_register(IR::Type const &ty, Span const &def_span, bool mut);

    private:
         // because initialization order
        uint64_t register_id;

    public:
        NNPtr<IR::Register> ret_reg;
        std::vector<IR::Register> param_regs;

        NNPtr<FunctionType> ty;
        std::string name;

        bool prototypeonly;

        void value_accept(ValueVisitor &v) const override;

    private:
        Span const _def_span;

        uint64_t block_i;
    };
}
