#include "lower/lowerer.h"
#include "lowererlocal.h"

#include "ir/function.h"

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

using namespace Lower;

Lowerer::Lowerer(IR::Unit const &unit): unit(unit), mod(unit.file->filename, context), fpm(&mod) {
    fpm.add(llvm::createPromoteMemoryToRegisterPass());
    fpm.add(llvm::createInstructionCombiningPass());
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());

    fpm.doInitialization();
}

bool Lowerer::lower() {
    LowerDeclSym lower_decl_sym (*this);

    unit.mod.declsym_accept(lower_decl_sym);

    return true;
}

bool Lowerer::objectify(llvm::raw_fd_ostream &ostream) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto target_triple = llvm::sys::getDefaultTargetTriple();

    std::string err;
    auto target = llvm::TargetRegistry::lookupTarget(target_triple, err);

    if (!target) {
        llvm::errs() << err << "\n";
        return false;
    }

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto target_machine = target->createTargetMachine(target_triple, "generic", "", opt, rm);

    mod.setDataLayout(target_machine->createDataLayout());
    mod.setTargetTriple(target_triple);

    llvm::legacy::PassManager pm;
    auto fty = llvm::CodeGenFileType::CGFT_ObjectFile;
    if (target_machine->addPassesToEmitFile(pm, ostream, nullptr, fty)) {
        llvm::errs() << "Target machine cannot emit object file\n";
        return false;
    }

    pm.run(mod);
    return true;
}

void Lowerer::print_mod(llvm::raw_ostream &ostream) {
    mod.print(ostream, nullptr);
}

llvm::Function &Lowerer::get_function(IR::Function const &fun) {
    auto found_llvm_fun = functions.find(fun);
    if (found_llvm_fun == functions.end()) {
        auto *fun_ty = static_cast<llvm::FunctionType*>(&fun.ty->to_llvmtype(context));
        std::string fun_name = fun.name == "main" ? fun.name : mangler.mangle_name(fun);

        llvm::Function *llvm_fun = llvm::Function::Create(fun_ty, llvm::Function::ExternalLinkage, fun_name, &mod);

        functions[fun] = llvm_fun;
        return *llvm_fun;
    } else
        return *found_llvm_fun->second;
}

