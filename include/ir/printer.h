#pragma once

namespace llvm { class raw_ostream; }

namespace IR {
    class Unit;

    class Printer {
    public:
        Printer(IR::Unit &unit, llvm::raw_ostream &ostream);
    private:
        IR::Unit &unit;
        llvm::raw_ostream &ostream;
    };
}
