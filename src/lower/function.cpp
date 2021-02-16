#include "lower/lowerer.h"
#include "lowererlocal.h"
#include "ir/function.h"
#include "ir/block.h"
#include "ir/instruction.h"

#include "llvm/IR/Verifier.h"

using namespace Lower;

LowerFunction::LowerFunction(Lowerer &lowerer, IR::Function const &fun):
    lowerer(lowerer),
    value_ref(*this),
    instr(*this),
    builder(lowerer.context),
    fun(fun) {}

void LowerFunction::lower() {
    if (fun.prototypeonly)
        return;

    llvm::Function *llvm_fun = &lowerer.get_function(fun);
    entry_block = llvm::BasicBlock::Create(lowerer.context, "lower_entry", llvm_fun);

    builder.SetInsertPoint(entry_block);
    undef_alloca = builder.CreateAlloca(builder.getInt8Ty(), nullptr, "undef_alloca");

    unsigned int param_ind = 0;
    while (param_ind < fun.param_regs.size()) {
        llvm::Argument *llvm_param = llvm_fun->getArg(param_ind);
        IR::Register const &param_reg = fun.param_regs[param_ind];

        llvm::AllocaInst *alloca = builder.CreateAlloca(&param_reg.type().to_llvm_type(lowerer.context));
        builder.CreateStore(llvm_param, alloca);
        registers[param_reg] = alloca;

        ++param_ind;
    }

    for (std::unique_ptr<IR::Block> const &block : fun.blocks) {
        builder.SetInsertPoint(&get_block(*block));
        for (std::unique_ptr<IR::Instruction> const &i : block->instructions)
            i->instr_accept(instr);

        if (block->br)
            block->br->instr_accept(instr);
    }

    builder.SetInsertPoint(entry_block);
    builder.CreateBr(&get_block(*fun.blocks[0]));

    undef_alloca->removeFromParent();

    llvm::verifyFunction(*llvm_fun);

    // fpm.run(*fasllvm);

    return;
}

llvm::AllocaInst &LowerFunction::get_register(IR::Register const &reg) {
    auto found_alloca = registers.find(reg);
    if (found_alloca == registers.end()) {
        llvm::IRBuilder<> entry_builder (entry_block);
        llvm::AllocaInst *alloca = entry_builder.CreateAlloca(&reg.type().to_llvm_type(lowerer.context));

        registers[reg] = alloca;
        return *alloca;
    } else
        return *found_alloca->second;
}

llvm::BasicBlock &LowerFunction::get_block(IR::Block const &block) {
    auto found_llvm_block = blocks.find(block);
    if (found_llvm_block == blocks.end()) {
        llvm::BasicBlock *llvm_block = llvm::BasicBlock::Create(lowerer.context, block.name, &lowerer.get_function(fun));

        blocks[block] = llvm_block;
        return *llvm_block;
    } else
        return *found_llvm_block->second;
}

llvm::Value &LowerFunction::get_instruction(IR::Instruction const &instr) {
    auto found_llvm_value = instructions.find(instr);
    if (found_llvm_value == instructions.end()) {
        llvm::Value *llvm_value = builder.CreateLoad(undef_alloca);
        instructions[instr] = llvm_value;
        return *llvm_value;
    } else
        return *found_llvm_value->second;
}

void LowerFunction::set_instruction(IR::Instruction const &instr, llvm::Value *new_value) {
    llvm::Value *old_value = &get_instruction(instr);
    old_value->replaceAllUsesWith(new_value);

    if (auto *old_instr = llvm::dyn_cast<llvm::Instruction>(old_value))
        old_instr->removeFromParent();

    instructions[instr] = new_value;
}
