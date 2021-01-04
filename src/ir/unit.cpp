#include "ir/unit.h"
#include "ir/value.h"
#include "ir/instruction.h"

IR::Unit::Unit(File const &file): implicitDeclAST(std::make_unique<ASTNS::ImplicitDecl>(file, Location(), Location(), 0)), file(file), mod("", implicitDeclAST.get()) {}

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

