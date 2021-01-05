#include "ir/printer.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/unit.h"
#include "utils/format.h"
#include "ir/instruction.h"

IR::Printer::Printer(IR::Unit &unit, llvm::raw_ostream &ostream): unit(unit), ostream(ostream) {}

namespace {
    class VDPrinter;
    class VRPrinter;
    class DSDPrinter;
    class _Printer {
    public:
        _Printer(IR::Unit &unit, llvm::raw_ostream &ostream);

        void print();

        IR::Unit &unit;
        llvm::raw_ostream &ostream;

        std::unique_ptr<VDPrinter> vdp;
        std::unique_ptr<VRPrinter> vrp;
        std::unique_ptr<DSDPrinter> dsdp;
    };

    // Value Decl Printer
    class VDPrinter final : public IR::ValueVisitor {
    public:
        VDPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;
        void value_visitConstBool(IR::ConstBool *)   { reportAbortNoh("print declaratino of ConstBool"); }
        void value_visitConstChar(IR::ConstChar *)   { reportAbortNoh("print declaration of ConstChar"); }
        void value_visitConstInt(IR::ConstInt *)     { reportAbortNoh("print declaration of ConstInt"); }
        void value_visitConstFloat(IR::ConstFloat *) { reportAbortNoh("print declaration of ConstFloat"); }
        void value_visitVoid(IR::Void *)             { reportAbortNoh("print declaration of Void"); }
        void value_visitFunction(IR::Function *v) {
            // TODO:
        }
        void value_visitInstruction(IR::Instrs::Instruction *v) {
            // TODO: instruction visitor
        }
    };
    // Value Ref Printer
    class VRPrinter final : public IR::ValueVisitor {
    public:
        VRPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;
        void value_visitConstBool(IR::ConstBool *v) {
            pr.ostream << (v->val ? "true" : "false");
        }
        void value_visitConstChar(IR::ConstChar *v) {
            pr.ostream << "'" << v->val << "'";
        }
        void value_visitConstInt(IR::ConstInt *v) {
            pr.ostream << v->val;
        }
        void value_visitConstFloat(IR::ConstFloat *v) {
            pr.ostream << v->val;
        }
        void value_visitFunction(IR::Function *v) {
            pr.ostream << v->name;
        }
        void value_visitInstruction(IR::Instrs::Instruction *v) {
            pr.ostream << v; // TODO: assign instruction an id
        }
        void value_visitVoid(IR::Void *v) {
            pr.ostream << "'void'";
        }
    };
    // Decl Symbol Decl Printer
    class DSDPrinter final : public IR::DeclSymbolVisitor {
    public:
        DSDPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;
        void declsym_visitType(IR::Type *ty) {
            // TODO: inherit from type visitor
            pr.ostream << "type " << ty->name() << "\n";
            walk(ty);
        }
        void declsym_visitModule(IR::Module *mod) {
            pr.ostream << "mod " << mod->name() << "\n";
            walk(mod);
        }

        void walk(IR::DeclSymbol *ds) {
            for (auto _ds : ds->getDeclSymbols()) {
                std::string name = _ds.first;
                IR::DeclSymbol *pds = _ds.second;

                pds->declsym_accept(pr.dsdp.get());
            }
            for (auto v : ds->getValues()) {
                std::string name = v.first;
                IR::Value *val = v.second;

                val->value_accept(pr.vdp.get());
            }
        }
    };

    _Printer::_Printer(IR::Unit &unit, llvm::raw_ostream &ostream):
            unit(unit),
            ostream(ostream),
            vdp(std::make_unique<VDPrinter>(*this)),
            vrp(std::make_unique<VRPrinter>(*this)),
            dsdp(std::make_unique<DSDPrinter>(*this)) {}

    void _Printer::print() {
        unit.mod.declsym_accept(dsdp.get());
    }
}

void IR::Printer::print() {
    _Printer p (unit, ostream);
    p.print();
}

/*
   void IR::Printer::visitStore(IR::Instrs::Store *i) {
// ostream << format("store % --> %", i->value, i->target);
}
void IR::Printer::visitPhi(IR::Instrs::Phi *i) {
// ostream << format("phi [");
// bool first = true;
// for (auto &p : i->prevs) {
// if (!first)
// ostream << ", ";
//
// p.first->stringify(ostream);
// ostream << format(": %", p.second);
//
// first = false;
// }
// ostream << format("] -> %", i);
}
void IR::Printer::visitRegister(IR::Instrs::Register *i) {
// ostream << format("register % -> %", i->ty, i);
}
void IR::Printer::visitOr(IR::Instrs::Or *i) {
// ostream << format("or % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitAnd(IR::Instrs::And *i) {
// ostream << format("and % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitNot(IR::Instrs::Not *i) {
// ostream << format("not % -> %", i->op, i);
}
void IR::Printer::visitICmpNE(IR::Instrs::ICmpNE *i) {
// ostream << format("icmpne % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitICmpEQ(IR::Instrs::ICmpEQ *i) {
// ostream << format("icmpeq % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitICmpLT(IR::Instrs::ICmpLT *i) {
// ostream << format("icmplt % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitICmpGT(IR::Instrs::ICmpGT *i) {
// ostream << format("icmpgt % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitICmpLE(IR::Instrs::ICmpLE *i) {
// ostream << format("icmple % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitICmpGE(IR::Instrs::ICmpGE *i) {
// ostream << format("icmpge % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFCmpNE(IR::Instrs::FCmpNE *i) {
// ostream << format("fcmpne % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFCmpEQ(IR::Instrs::FCmpEQ *i) {
// ostream << format("fcmpeq % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFCmpLT(IR::Instrs::FCmpLT *i) {
// ostream << format("fcmplt % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFCmpGT(IR::Instrs::FCmpGT *i) {
// ostream << format("fcmpgt % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFCmpLE(IR::Instrs::FCmpLE *i) {
// ostream << format("fcmple % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFCmpGE(IR::Instrs::FCmpGE *i) {
// ostream << format("fcmpge % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitBitXor(IR::Instrs::BitXor *i) {
// ostream << format("bitxor % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitBitOr(IR::Instrs::BitOr *i) {
// ostream << format("bitor % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitBitAnd(IR::Instrs::BitAnd *i) {
    // ostream << format("bitand % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitBitNot(IR::Instrs::BitNot *i) {
    // ostream << format("bitnot % -> %", i->op, i);
}
void IR::Printer::visitShiftR(IR::Instrs::ShiftR *i) {
    // ostream << format("shiftr % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitShiftL(IR::Instrs::ShiftL *i) {
    // ostream << format("shiftl % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitIAdd(IR::Instrs::IAdd *i) {
    // ostream << format("iadd % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitISub(IR::Instrs::ISub *i) {
    // ostream << format("isub % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitIMult(IR::Instrs::IMult *i) {
    // ostream << format("imult % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitIDiv(IR::Instrs::IDiv *i) {
    // ostream << format("idiv % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitIMod(IR::Instrs::IMod *i) {
    // ostream << format("imod % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitINeg(IR::Instrs::INeg *i) {
    // ostream << format("ineg % -> %", i->op, i);
}
void IR::Printer::visitFAdd(IR::Instrs::FAdd *i) {
    // ostream << format("fadd % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFSub(IR::Instrs::FSub *i) {
    // ostream << format("fsub % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFMult(IR::Instrs::FMult *i) {
    // ostream << format("fmult % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFDiv(IR::Instrs::FDiv *i) {
    // ostream << format("fdiv % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFMod(IR::Instrs::FMod *i) {
    // ostream << format("fmod % % -> %", i->lhs, i->rhs, i);
}
void IR::Printer::visitFNeg(IR::Instrs::FNeg *i) {
    // ostream << format("fneg % -> %", i->op, i);
}
void IR::Printer::visitNoOpCast(IR::Instrs::NoOpCast *i) {
    // ostream << format("noopcast % > % -> %", i->op, i->newt, i);
}
void IR::Printer::visitFloatToFloat(IR::Instrs::FloatToFloat *i) {
    // ostream << format("ftof % > % -> %", i->op, i->newt, i);
}
void IR::Printer::visitIntToInt(IR::Instrs::IntToInt *i) {
    // ostream << format("itoi % > % -> %", i->op, i->newt, i);
}
void IR::Printer::visitIntToFloat(IR::Instrs::IntToFloat *i) {
    // ostream << format("itof % > % -> %", i->op, i->newt, i);
}
void IR::Printer::visitFloatToInt(IR::Instrs::FloatToInt *i) {
    // ostream << format("ftoi % > % -> %", i->op, i->newt, i);
}
void IR::Printer::visitDerefPtr(IR::Instrs::DerefPtr *i) {
    // ostream << format("derefptr % -> %", i->ptr, i);
}
void IR::Printer::visitPtrArith(IR::Instrs::PtrArith *i) {
    // ostream << format("ptrarith % % -> %", i->ptr, i->offset, i);
}
void IR::Printer::visitReturn(IR::Instrs::Return *i) {
    // ostream << format("return %", i->value ? i->value.stringify() : "void");
}
void IR::Printer::visitCall(IR::Instrs::Call *i) {
    // ostream << format("call % ( ", i->f);
    // for (IR::ASTValue const &v : i->args)
    // ostream << v.stringify() << " ";
    // ostream << format(") -> %", i);
}
void IR::Printer::visitGotoBr(IR::Instrs::GotoBr *i) {
    // ostream << format("gotobr ==> ");
    // i->to->stringify(ostream);
}
void IR::Printer::visitCondBr(IR::Instrs::CondBr *i) {
    // ostream << format("condbr % ? ==> ", i->v);
    // i->trueB->stringify(ostream);
    // ostream << " : ==> ";
    // i->falseB->stringify(ostream);
}
*/
