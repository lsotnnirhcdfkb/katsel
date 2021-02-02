#pragma once

namespace llvm { class raw_ostream; }

namespace IR {
    class Unit;

    class Printer {
    public:
        Printer(IR::Unit const &unit, llvm::raw_ostream &ostream);
        void print();

    private:
        IR::Unit const &unit;
        llvm::raw_ostream &ostream;
    };
}
