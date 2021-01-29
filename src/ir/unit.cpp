#include "ir/unit.h"
#include "ir/printer.h"
#include "llvm/Support/raw_ostream.h"
#include "utils/file.h"
#include "utils/location.h"
#include "ast/ast.h"
#include "ir/function.h"
#include "ir/block.h"

IR::Unit::Unit(File const &file, ASTNS::ImplicitDecl &implicit): file(file), mod("", implicit) {}

void IR::Unit::print(llvm::raw_ostream &ostream) const {
    ostream << "> Unit '" << file.filename << "'\n";
    IR::Printer p (*this, ostream);
    p.print();
}


