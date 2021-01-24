#include "lower/lowerer.h"
#include "ir/type.h"
#include "utils/assert.h"
#include <memory>
#include "utils/file.h"
#include "ir/instruction.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

Lower::Lowerer::Lowerer(IR::Unit const &unit): errored(false), unit(unit), builder(context), mod(unit.file.filename, context), fpm(&mod) {
    fpm.add(llvm::create_promote_memory_to_register_pass());
    fpm.add(llvm::create_instruction_combining_pass());
    fpm.add(llvm::create_reassociate_pass());
    fpm.add(llvm::create_gvnpass());
    fpm.add(llvm::create_cfgsimplification_pass());

    fpm.do_initialization();
}

void Lower::Lowerer::print_mod(llvm::raw_ostream &ostream) {
    mod.print(ostream, nullptr);
}

bool Lower::Lowerer::objectify(llvm::raw_fd_ostream &ostream) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto target_triple = llvm::sys::getDefaultTargetTriple();

    std::string err;
    auto target = llvm::TargetRegistry::lookup_target(target_triple, err);

    if (!target) {
        llvm::errs() << err << "\n";
        return false;
    }

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto target_machine = target->create_target_machine(target_triple, "generic", "", opt, rm);

    mod.set_data_layout(target_machine->create_data_layout());
    mod.set_target_triple(target_triple);

    llvm::legacy::PassManager pm;
    auto fty = llvm::CodeGenFileType::CGFT_ObjectFile;
    if (target_machine->add_passes_to_emit_file(pm, ostream, nullptr, fty)) {
        llvm::errs() << "Target machine cannot emit object file\n";
        return false;
    }

    pm.run(mod);
    return true;
}

void Lower::Lowerer::lower() {
    // TODO
    // for (std::unique_ptr<IR::Function> const &f : unit.functions) {
        // auto *fty = static_cast<llvm::FunctionType*>(f->ty->to_llvmtype(context));
        // std::string fname = f->name == "main" ? f->name : mangler.mangle_name(*f);
        // auto *fllvm = llvm::Function::Create(fty, llvm::Function::ExternalLinkage, fname, &mod);
        // functions[f.get()] = fllvm;
    // }
    // for (std::unique_ptr<IR::Function> const &f : unit.functions)
        // lower(*f);
}


void Lower::Lowerer::lower(IR::Function const &f) {
    if (f.prototypeonly)
        return;

    llvm::Function *fasllvm = functions.at(&f);
    for (std::unique_ptr<IR::Block> const &b : f.blocks) {
        blocks[b.get()] = llvm::BasicBlock::Create(context, b->name, fasllvm);
    }

    alloca_index = 0;
    cur_function = fasllvm;

    for (std::unique_ptr<IR::Block> const &b : f.blocks)
        lower(*b);

    for (std::unique_ptr<IR::Block> const &b : f.blocks) {
        builder.SetInsertPoint(blocks[b.get()]);
        if (b->br)
            b->br->accept(*this);
    }

    blocks.clear();
    values.clear();

    llvm::verify_function(*fasllvm);
    // fpm.run(*fasllvm);
}

void Lower::Lowerer::lower(IR::Block const &b) {
    builder.SetInsertPoint(blocks[&b]);
    for (std::unique_ptr<IR::Instrs::Instruction> const &i : b.instructions)
        i->accept(*this);
}

NNPtr<llvm::Value> Lower::Lowerer::lower(NNPtr<IR::Value> v) {
    lvret = nullptr;
    v->value_accept(*this);
    return lvret;
}

NNPtr<llvm::Value> Lower::Lowerer::lower(IR::ASTValue &v) {
    return lower(v.val);
}

void Lower::Lowerer::value_visit_const_bool(IR::ConstBool &v) {
    lvret = llvm::ConstantInt::get(v.type()->to_llvmtype(context).as_raw(), v.val);
}
void Lower::Lowerer::value_visit_const_float(IR::ConstFloat &v) {
    lvret = llvm::ConstantFP::get(v.type()->to_llvmtype(context).as_raw(), v.val);
}
void Lower::Lowerer::value_visit_const_int(IR::ConstInt &v) {
    lvret = llvm::ConstantInt::get(v.type()->to_llvmtype(context).as_raw(), v.val);
}
void Lower::Lowerer::value_visit_const_char(IR::ConstChar &v) {
    lvret = llvm::ConstantInt::get(v.type()->to_llvmtype(context).as_raw(), v.val);
}
void Lower::Lowerer::value_visit_function(IR::Function &v) {
    lvret = functions.at(v);
}
void Lower::Lowerer::value_visit_void(IR::Void &v) {
    report_abort_noh("lower_value called with v = Void");
}
void Lower::Lowerer::value_visit_instruction(IR::Instrs::Instruction &v) {
    lvret = values.at(v);
}
