#include "lower/lowerer.h"
#include "ir/type.h"
#include "utils/assert.h"
#include <memory>
#include "utils/file.h"

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
    fpm.add(llvm::createPromoteMemoryToRegisterPass());
    fpm.add(llvm::createInstructionCombiningPass());
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());

    fpm.doInitialization();
}

void Lower::Lowerer::printMod(llvm::raw_ostream &ostream) {
    mod.print(ostream, nullptr);
}

bool Lower::Lowerer::objectify(llvm::raw_fd_ostream &ostream) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto targetTriple = llvm::sys::getDefaultTargetTriple();

    std::string err;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, err);

    if (!target) {
        llvm::errs() << err << "\n";
        return false;
    }

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, "generic", "", opt, rm);

    mod.setDataLayout(targetMachine->createDataLayout());
    mod.setTargetTriple(targetTriple);

    llvm::legacy::PassManager pm;
    auto fty = llvm::CodeGenFileType::CGFT_ObjectFile;
    if (targetMachine->addPassesToEmitFile(pm, ostream, nullptr, fty)) {
        llvm::errs() << "Target machine cannot emit object file\n";
        return false;
    }

    pm.run(mod);
    return true;
}

void Lower::Lowerer::lower() {
    // TODO
    // for (std::unique_ptr<IR::Function> const &f : unit.functions) {
        // auto *fty = static_cast<llvm::FunctionType*>(f->ty->toLLVMType(context));
        // std::string fname = f->name == "main" ? f->name : mangler.mangleName(*f);
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

    allocaIndex = 0;
    curFunction = fasllvm;

    for (std::unique_ptr<IR::Block> const &b : f.blocks)
        lower(*b);

    for (std::unique_ptr<IR::Block> const &b : f.blocks) {
        builder.SetInsertPoint(blocks[b.get()]);
        if (b->br)
            b->br->accept(*this);
    }

    blocks.clear();
    values.clear();

    llvm::verifyFunction(*fasllvm);
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

void Lower::Lowerer::value_visitConstBool(IR::ConstBool &v) {
    lvret = llvm::ConstantInt::get(v.type()->toLLVMType(context).asRaw(), v.val);
}
void Lower::Lowerer::value_visitConstFloat(IR::ConstFloat &v) {
    lvret = llvm::ConstantFP::get(v.type()->toLLVMType(context).asRaw(), v.val);
}
void Lower::Lowerer::value_visitConstInt(IR::ConstInt &v) {
    lvret = llvm::ConstantInt::get(v.type()->toLLVMType(context).asRaw(), v.val);
}
void Lower::Lowerer::value_visitConstChar(IR::ConstChar &v) {
    lvret = llvm::ConstantInt::get(v.type()->toLLVMType(context).asRaw(), v.val);
}
void Lower::Lowerer::value_visitFunction(IR::Function &v) {
    lvret = functions.at(v);
}
void Lower::Lowerer::value_visitVoid(IR::Void &v) {
    reportAbortNoh("lowerValue called with v = Void");
}
void Lower::Lowerer::value_visitInstruction(IR::Instrs::Instruction &v) {
    lvret = values.at(v);
}
