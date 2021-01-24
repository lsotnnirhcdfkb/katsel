#include "ir/unit.h"
#include "ir/printer.h"
#include "ir/instruction.h"
#include "llvm/Support/raw_ostream.h"
#include "utils/file.h"

IR::Unit::Unit(File const &file): implicitDeclAST(std::make_unique<ASTNS::ImplicitDecl>(file, Maybe<Location const>(), Maybe<Location const>(), 0)), file(file), mod("", *implicitDeclAST) {}

void IR::Unit::print(llvm::raw_ostream &ostream) {
    ostream << "> Unit '" << file.filename << "'\n";
    IR::Printer p (*this, ostream);
    p.print();
}


