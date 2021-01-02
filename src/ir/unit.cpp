#include "ir/unit.h"
#include "ir/value.h"
#include "ir/instruction.h"

IR::Unit::Unit(File const &file): file(file), mod(file) {}

void IR::Unit::print(llvm::raw_ostream &ostream) const {
    ostream << "> Unit \"" << file.filename << "\"\n";
    // for (std::unique_ptr<IR::Function> const &f : functions)
        // f->definition(ostream);
}

void IR::Unit::cfgDot(llvm::raw_ostream &ostream) const {
    ostream << "strict digraph \"CFG for unit " << file.filename << "\" {\n";
    ostream << "    graph [label=\"CFG for unit " << file.filename << "\"]\n";
    // for (std::unique_ptr<IR::Function> const &f : functions)
        // f->cfgDot(ostream);
    ostream << "}\n";
}

/*
IR::Function* IR::Unit::addFunction(FunctionType *type, std::string name, ASTNS::FunctionDecl *ast) {
    std::unique_ptr<IR::Function> f (std::make_unique<IR::Function>(type, name, ast));
    IR::Function *fraw = f.get();
    functions.push_back(std::move(f));
    return fraw;
}
*/
