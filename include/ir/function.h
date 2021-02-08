#pragma once

#include <memory>

#include "ast/astfwd.h"
#include "utils/ptr.h"
#include "ir/value.h"

namespace llvm { class raw_ostream; }

namespace IR {
    class Block;
    class FunctionType;
    class Register;
    class Type;

    class Function : public Value, public DeclaredValue {
    public:
        Function(NNPtr<FunctionType> ty, std::string name, NNPtr<ASTNS::FunctionDecl> def_ast);

        void add(std::unique_ptr<Block> block);

        void definition(llvm::raw_ostream &os) const;
        ASTNS::AST const &def_ast() const override;

        Type const &type() const override;

        std::vector<std::unique_ptr<Block>> blocks;
        std::vector<std::unique_ptr<Register>> registers;

        Block &add_block(std::string name);
        Register &add_register(IR::Type const &ty, ASTNS::AST const &def_ast);

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

        NNPtr<ASTNS::FunctionDecl> _def_ast;

    private:
        uint64_t block_i;
    };
}
