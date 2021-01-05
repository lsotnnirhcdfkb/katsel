#include "ir/printer.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/unit.h"
#include "ir/instruction.h"
#include "utils/format.h"
#include "ir/visitor.h"

IR::Printer::Printer(IR::Unit &unit, llvm::raw_ostream &ostream): unit(unit), ostream(ostream) {}

namespace {
    // fw decls {{{
    class VDPrinter;
    class VRPrinter;
    class DSDPrinter;
    // }}}
    // _Printer {{{
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
    // }}}

    // Value Ref Printer {{{
    class VRPrinter final : public IR::ValueVisitor {
    public:
        VRPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;

        void value_visitConstBool(IR::ConstBool *v) override {
            pr.ostream << (v->val ? "true" : "false");
        }
        void value_visitConstChar(IR::ConstChar *v) override {
            pr.ostream << "'" << v->val << "'";
        }
        void value_visitConstInt(IR::ConstInt *v) override {
            pr.ostream << v->val;
        }
        void value_visitConstFloat(IR::ConstFloat *v) override {
            pr.ostream << v->val;
        }
        void value_visitFunction(IR::Function *v) override {
            pr.ostream << v->name;
        }
        void value_visitInstruction(IR::Instrs::Instruction *v) override {
            pr.ostream << v; // TODO: assign instruction an id
        }
        void value_visitVoid(IR::Void *v) override {
            pr.ostream << "'void'";
        }
    };
    // }}}
    // Value Decl Printer {{{
    class VDPrinter final : public IR::ValueVisitor, public IR::InstructionVisitor, public IR::BrVisitor {
    public:
        VDPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;

        // Const Values (all abort) {{{
        void value_visitConstBool(IR::ConstBool *)   override { reportAbortNoh("print declaratino of ConstBool"); }
        void value_visitConstChar(IR::ConstChar *)   override { reportAbortNoh("print declaration of ConstChar"); }
        void value_visitConstInt(IR::ConstInt *)     override { reportAbortNoh("print declaration of ConstInt"); }
        void value_visitConstFloat(IR::ConstFloat *) override { reportAbortNoh("print declaration of ConstFloat"); }
        void value_visitVoid(IR::Void *)             override { reportAbortNoh("print declaration of Void"); }
        // }}}
        // visit Instruction {{{
        void value_visitInstruction(IR::Instrs::Instruction *v) override {
            v->accept(this);
        }
        // }}}
        // Function {{{
        void value_visitFunction(IR::Function *fun) override {
            pr.ostream << format("fun %: %", fun->name, fun->type());
            if (fun->prototypeonly) {
                pr.ostream << " (prototype);\n";
                return;
            } else {
                pr.ostream << " {\n";
            }

            for (std::unique_ptr<IR::Block> &block : fun->blocks)
                printBlock(*block);
            pr.ostream << "}\n";
        }

        void printBlock(IR::Block &b) {
            stringifyBlock(b);

            pr.ostream << ": {\n";

            for (std::unique_ptr<IR::Instrs::Instruction> const &instr : b.instructions) {
                instr->accept(this);
                pr.ostream << "\n";
            }
            if (b.br) {
                pr.ostream << "----";
                b.br->accept(this);
                pr.ostream << "\n";
            }
            pr.ostream << "}\n";
        }
        // }}}
        // all the instructions {{{
        // helpers {{{
        void instrName(std::string const &s) {
            pr.ostream << s << " ";
        }
        void toTemp(IR::Instrs::Instruction *i) {
            pr.ostream << " -> " << i;
        }
        void stringifyBlock(IR::Block const &b) {
            pr.ostream << format("%(%)", b.name, b.num);
        }
        void binaryInstruction(IR::Instrs::Instruction *i, std::string const &name, IR::ASTValue const &lhs, IR::ASTValue const &rhs) {
            instrName(name);
            lhs.val->value_accept(pr.vrp.get());
            pr.ostream << " ";
            rhs.val->value_accept(pr.vrp.get());
            toTemp(i);
        }
        void unaryInstruction(IR::Instrs::Instruction *i, std::string const &name, IR::ASTValue const &op) {
            instrName(name);
            op.val->value_accept(pr.vrp.get());
            toTemp(i);
        }
        void castInstruction(IR::Instrs::Instruction *i, std::string const &name, IR::ASTValue const &op, IR::Type const *to) {
            instrName(name);
            op.val->value_accept(pr.vrp.get());
            pr.ostream << " > " << to->name();
            toTemp(i);
        }
        void brArrow() {
            pr.ostream << "==> ";
        }
        // }}}

        void visitStore(IR::Instrs::Store *i) override {
            instrName("store");
            i->value.val->value_accept(pr.vrp.get());
            pr.ostream << " --> ";
            i->target.val->value_accept(pr.vrp.get());
        }
        void visitPhi(IR::Instrs::Phi *i) override {
            instrName("phi");
            pr.ostream << "[";
            bool first = true;
            for (auto &p : i->prevs) {
                if (!first)
                    pr.ostream << ", ";

                stringifyBlock(*p.first);
                pr.ostream << ": ";
                p.second.val->value_accept(pr.vrp.get());

                first = false;
            }
            pr.ostream << "]";
            toTemp(i);
        }
        void visitRegister(IR::Instrs::Register *i) override {
            instrName("register");
            pr.ostream << i->ty->name();
            toTemp(i);
        }

        // binary instructions {{{
        void visitOr(IR::Instrs::Or *i) override         { binaryInstruction(i, "or", i->lhs, i->rhs); }
        void visitAnd(IR::Instrs::And *i) override       { binaryInstruction(i, "and", i->lhs, i->rhs); }
        void visitICmpNE(IR::Instrs::ICmpNE *i) override { binaryInstruction(i, "icmpne", i->lhs, i->rhs); }
        void visitICmpEQ(IR::Instrs::ICmpEQ *i) override { binaryInstruction(i, "icmpeq", i->lhs, i->rhs); }
        void visitICmpLT(IR::Instrs::ICmpLT *i) override { binaryInstruction(i, "icmplt", i->lhs, i->rhs); }
        void visitICmpGT(IR::Instrs::ICmpGT *i) override { binaryInstruction(i, "icmpgt", i->lhs, i->rhs); }
        void visitICmpLE(IR::Instrs::ICmpLE *i) override { binaryInstruction(i, "icmple", i->lhs, i->rhs); }
        void visitICmpGE(IR::Instrs::ICmpGE *i) override { binaryInstruction(i, "icmpge", i->lhs, i->rhs); }
        void visitFCmpNE(IR::Instrs::FCmpNE *i) override { binaryInstruction(i, "fcmpne", i->lhs, i->rhs); }
        void visitFCmpEQ(IR::Instrs::FCmpEQ *i) override { binaryInstruction(i, "fcmpeq", i->lhs, i->rhs); }
        void visitFCmpLT(IR::Instrs::FCmpLT *i) override { binaryInstruction(i, "fcmplt", i->lhs, i->rhs); }
        void visitFCmpGT(IR::Instrs::FCmpGT *i) override { binaryInstruction(i, "fcmpgt", i->lhs, i->rhs); }
        void visitFCmpLE(IR::Instrs::FCmpLE *i) override { binaryInstruction(i, "fcmple", i->lhs, i->rhs); }
        void visitFCmpGE(IR::Instrs::FCmpGE *i) override { binaryInstruction(i, "fcmpge", i->lhs, i->rhs); }
        void visitBitXor(IR::Instrs::BitXor *i) override { binaryInstruction(i, "bitxor", i->lhs, i->rhs); }
        void visitBitOr(IR::Instrs::BitOr *i) override   { binaryInstruction(i, "bitor", i->lhs, i->rhs); }
        void visitBitAnd(IR::Instrs::BitAnd *i) override { binaryInstruction(i, "bitand", i->lhs, i->rhs); }
        void visitShiftR(IR::Instrs::ShiftR *i) override { binaryInstruction(i, "shiftr", i->lhs, i->rhs); }
        void visitShiftL(IR::Instrs::ShiftL *i) override { binaryInstruction(i, "shiftl", i->lhs, i->rhs); }
        void visitIAdd(IR::Instrs::IAdd *i) override     { binaryInstruction(i, "iadd", i->lhs, i->rhs); }
        void visitISub(IR::Instrs::ISub *i) override     { binaryInstruction(i, "isub", i->lhs, i->rhs); }
        void visitIMult(IR::Instrs::IMult *i) override   { binaryInstruction(i, "imult", i->lhs, i->rhs); }
        void visitIDiv(IR::Instrs::IDiv *i) override     { binaryInstruction(i, "idiv", i->lhs, i->rhs); }
        void visitIMod(IR::Instrs::IMod *i) override     { binaryInstruction(i, "imod", i->lhs, i->rhs); }
        void visitFAdd(IR::Instrs::FAdd *i) override     { binaryInstruction(i, "fadd", i->lhs, i->rhs); }
        void visitFSub(IR::Instrs::FSub *i) override     { binaryInstruction(i, "fsub", i->lhs, i->rhs); }
        void visitFMult(IR::Instrs::FMult *i) override   { binaryInstruction(i, "fmult", i->lhs, i->rhs); }
        void visitFDiv(IR::Instrs::FDiv *i) override     { binaryInstruction(i, "fdiv", i->lhs, i->rhs); }
        void visitFMod(IR::Instrs::FMod *i) override     { binaryInstruction(i, "fmod", i->lhs, i->rhs); }
        // }}}
        // unary instructions {{{
        void visitNot(IR::Instrs::Not *i) override       { unaryInstruction(i, "not", i->op); }
        void visitBitNot(IR::Instrs::BitNot *i) override { unaryInstruction(i, "bitnot", i->op); }
        void visitINeg(IR::Instrs::INeg *i) override     { unaryInstruction(i, "ineg", i->op); }
        void visitFNeg(IR::Instrs::FNeg *i) override     { unaryInstruction(i, "fneg", i->op); }
        // }}}
        // cast instructions {{{
        void visitNoOpCast(IR::Instrs::NoOpCast *i) override         { castInstruction(i, "noopcast", i->op, i->newt); }
        void visitFloatToFloat(IR::Instrs::FloatToFloat *i) override { castInstruction(i, "ftof", i->op, i->newt); }
        void visitIntToInt(IR::Instrs::IntToInt *i) override         { castInstruction(i, "itoi", i->op, i->newt); }
        void visitIntToFloat(IR::Instrs::IntToFloat *i) override     { castInstruction(i, "itof", i->op, i->newt); }
        void visitFloatToInt(IR::Instrs::FloatToInt *i) override     { castInstruction(i, "ftoi", i->op, i->newt); }
        // }}}
        // pointer instructions {{{
        void visitDerefPtr(IR::Instrs::DerefPtr *i) override {
            unaryInstruction(i, "derefptr", i->ptr);
        }
        void visitPtrArith(IR::Instrs::PtrArith *i) override {
            binaryInstruction(i, "ptrarith", i->ptr, i->offset);
        }
        // }}}

        void visitCall(IR::Instrs::Call *i) override {
            instrName("call");
            i->f->value_accept(pr.vrp.get());
            pr.ostream << " ( ";
            for (IR::ASTValue const &v : i->args) {
                v.val->value_accept(pr.vrp.get());
                pr.ostream << " ";
            }
            pr.ostream << ")";
            toTemp(i);
        }

        // branches {{{
        void visitReturn(IR::Instrs::Return *i) override {
            instrName("return");
            if (i->value) {
                i->value.val->value_accept(pr.vrp.get());
            } else {
                pr.ostream << "void";
            }
        }
        void visitGotoBr(IR::Instrs::GotoBr *i) override {
            instrName("gotobr"); brArrow();
            stringifyBlock(*i->to);
        }
        void visitCondBr(IR::Instrs::CondBr *i) override {
            instrName("condbr");
            i->v.val->value_accept(pr.vrp.get());
            brArrow(); stringifyBlock(*i->trueB);
            brArrow(); stringifyBlock(*i->falseB);
        }
        // }}}
        // }}}
    };
    // }}}
    // Decl Symbol Decl Printer {{{
    class DSDPrinter final : public IR::DeclSymbolVisitor {
    public:
        DSDPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;
        // declsym {{{
        void declsym_visitType(IR::Type *ty) override {
            // TODO: inherit from type visitor
            pr.ostream << "type " << ty->name() << " {\n";
            walk(ty);
            pr.ostream << "}\n";
        }
        void declsym_visitModule(IR::Module *mod) override {
            pr.ostream << "mod " << mod->name() << " {\n";
            walk(mod);
            pr.ostream << "}\n";
        }
        // }}}
        // walk {{{
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
        // }}}
    };
    // }}}

    // _Printer impl {{{1
    _Printer::_Printer(IR::Unit &unit, llvm::raw_ostream &ostream):
            unit(unit),
            ostream(ostream),
            vdp(std::make_unique<VDPrinter>(*this)),
            vrp(std::make_unique<VRPrinter>(*this)),
            dsdp(std::make_unique<DSDPrinter>(*this)) {}

    void _Printer::print() {
        unit.mod.declsym_accept(dsdp.get());
    }
    // }}}
}

void IR::Printer::print() {
    _Printer p (unit, ostream);
    p.print();
}
