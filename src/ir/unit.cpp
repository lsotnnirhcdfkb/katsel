#include "ir/unit.h"
#include "ir/printer.h"
#include "ir/instruction.h"

IR::Unit::Unit(File const &file): implicitDeclAST(std::make_unique<ASTNS::ImplicitDecl>(file, Location(), Location(), 0)), file(file), mod("", implicitDeclAST.get()) {}

void IR::Unit::print(llvm::raw_ostream &ostream) const {
    ostream << "> Unit '" << file.filename << "'\n";
    IR::Printer p (*this, ostream);
    p.print();
}


