#pragma once

#include "ir/unit.h"
#include "mangle/mangler.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"

namespace Lower {
    class Lowerer {
    public:
        Lowerer(IR::Unit const &unit);

        bool lower();
        bool objectify(llvm::raw_fd_ostream &ostream);
        void print_mod(llvm::raw_ostream &ostream);

        llvm::Function &get_function(IR::Function const &fun);

    private:
        IR::Unit const &unit;

        llvm::LLVMContext context;
        llvm::Module mod;

        llvm::legacy::FunctionPassManager fpm;

        Mangle::NameMangler mangler;

        friend class LowerValueRef;
        friend class LowerValueDef;
        friend class LowerDeclSym;
        friend class LowerInstr;
        friend class LowerFunction;

        std::unordered_map<NNPtr<IR::Function const>, llvm::Function *> functions;
    };
}
