#include "ir/unit.h"
#include "ir/printer.h"
#include "llvm/Support/raw_ostream.h"
#include "utils/file.h"
#include "utils/location.h"
#include "ast/ast.h"
#include "ir/function.h"
#include "ir/block.h"

IR::Unit::Unit(File const &file): context(), file(file), mod("", Maybe<ASTNS::AST const &>()) {}
IR::Unit::~Unit() = default;

void IR::Unit::print(llvm::raw_ostream &ostream) const {
    ostream << "> Unit '" << file->filename << "'\n";
    IR::Printer p (*this, ostream);
    p.print();
}


