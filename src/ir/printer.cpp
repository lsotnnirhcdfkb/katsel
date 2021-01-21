#include "ir/printer.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/unit.h"
#include "ir/instruction.h"
#include "utils/format.h"
#include "ir/visitor.h"

IR::Printer::Printer(IR::Unit &unit, llvm::raw_ostream &ostream): unit(unit), ostream(ostream) {}

namespace {
    // idToStr {{{
    std::string idToStr(uint64_t id) {
        constexpr int base = 26;
        std::string str;
        do {
            int rem = id % base;
            id /= base;
            str.insert(str.begin(), 'a' + rem);
        } while (id);
        return str;
    }
    // }}}
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

        template <typename T>
        _Printer& operator()(T const &t) {
            std::stringstream ss;
            ss << t;
            std::string s (ss.str());
            for (std::string::const_iterator i = s.cbegin(); i != s.cend(); ++i) {
                if (*i == '}')
                    --indent;

                if (lastnl) {
                    for (int i = 0; i < indent; ++i)
                        ostream << "    ";
                    lastnl = false;
                }

                ostream << *i;

                if (*i == '\n')
                    lastnl = true;
                else if (*i == '{')
                    ++indent;
            }
            return *this;
        }

        int indent;
        bool lastnl;

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

        void value_visitConstBool(NNPtr<IR::ConstBool> v) override {
            pr(v->val ? "true" : "false");
        }
        void value_visitConstChar(NNPtr<IR::ConstChar> v) override {
            pr("'")(v->val)("'");
        }
        void value_visitConstInt(NNPtr<IR::ConstInt> v) override {
            pr(v->val);
        }
        void value_visitConstFloat(NNPtr<IR::ConstFloat> v) override {
            pr(v->val);
        }
        void value_visitFunction(NNPtr<IR::Function> v) override {
            pr(v->name);
        }
        void value_visitInstruction(NNPtr<IR::Instrs::Instruction> v) override {
            pr(idToStr(v->id));
        }
        void value_visitVoid(NNPtr<IR::Void> v) override {
            pr("void");
        }
    };
    // }}}
    // Value Decl Printer {{{
    class VDPrinter final : public IR::ValueVisitor, public IR::InstructionVisitor, public IR::BrVisitor {
    public:
        VDPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;

        // Const Values (all abort) {{{
        void value_visitConstBool (NNPtr<IR::ConstBool>)  override { reportAbortNoh("print declaratino of ConstBool"); }
        void value_visitConstChar (NNPtr<IR::ConstChar>)  override { reportAbortNoh("print declaration of ConstChar"); }
        void value_visitConstInt  (NNPtr<IR::ConstInt>)   override { reportAbortNoh("print declaration of ConstInt"); }
        void value_visitConstFloat(NNPtr<IR::ConstFloat>) override { reportAbortNoh("print declaration of ConstFloat"); }
        void value_visitVoid      (NNPtr<IR::Void>)       override { reportAbortNoh("print declaration of Void"); }
        // }}}
        // visit Instruction {{{
        void value_visitInstruction(NNPtr<IR::Instrs::Instruction> v) override {
            pr("(")(idToStr(v->id))(" :: ")(v->type()->name())(") = ");
            v->accept(this);
            pr(";\n");
        }
        // }}}
        // Function {{{
        void value_visitFunction(NNPtr<IR::Function> fun) override {
            pr(format("fun %", fun->type()));
            if (fun->prototypeonly) {
                pr(" (prototype);\n");
                return;
            } else {
                pr(" {\n");
            }

            for (std::unique_ptr<IR::Block> &block : fun->blocks)
                printBlock(*block);
            pr("}\n");
        }

        void printBlock(IR::Block &b) {
            stringifyBlock(b);

            pr(": {\n");

            for (std::unique_ptr<IR::Instrs::Instruction> const &instr : b.instructions) {
                instr->value_accept(this);
            }
            if (b.br) {
                pr("=>: ");
                b.br->accept(this);
                pr(";\n");
            }
            pr("}\n");
        }
        // }}}
        // all the instructions {{{
        // helpers {{{
        void instrName(std::string const &s) {
            pr(s)("(");
        }
        void stringifyBlock(IR::Block const &b) {
            pr(format("%(%)", b.name, b.num));
        }
        void binaryInstruction(NNPtr<IR::Instrs::Instruction> i, std::string const &name, IR::ASTValue const &lhs, IR::ASTValue const &rhs) {
            instrName(name);
            lhs.val->value_accept(pr.vrp.get());
            pr(", ");
            rhs.val->value_accept(pr.vrp.get());
            pr(")");
        }
        void unaryInstruction(NNPtr<IR::Instrs::Instruction> i, std::string const &name, IR::ASTValue const &op) {
            instrName(name);
            op.val->value_accept(pr.vrp.get());
            pr(")");
        }
        void castInstruction(NNPtr<IR::Instrs::Instruction> i, std::string const &name, IR::ASTValue const &op, NNPtr<IR::Type const> const to) {
            instrName(name);
            op.val->value_accept(pr.vrp.get());
            pr(", ")(to->name());
            pr(")");
        }
        // }}}

        void visitStore(NNPtr<IR::Instrs::Store> i) override {
            instrName("store");
            i->value.val->value_accept(pr.vrp.get());

            pr(", ");
            if (i->init)
                pr("init");
            else
                pr("noinit");
            pr(", ");

            i->target.val->value_accept(pr.vrp.get());
            pr(")");
        }
        void visitPhi(NNPtr<IR::Instrs::Phi> i) override {
            instrName("phi");
            bool first = true;
            for (auto &p : i->prevs) {
                if (!first)
                    pr(", ");

                stringifyBlock(*p.first);
                pr(": ");
                p.second.val->value_accept(pr.vrp.get());

                first = false;
            }
            pr(")");
        }
        void visitRegister(NNPtr<IR::Instrs::Register> i) override {
            instrName("register");
            pr(i->ty->name());
            if (i->mut) pr(", mut");
            else        pr(", const");
            pr(")");
        }

        // binary instructions {{{
        void visitOr(NNPtr<IR::Instrs::Or> i) override         { binaryInstruction(i, "or", i->lhs, i->rhs); }
        void visitAnd(NNPtr<IR::Instrs::And> i) override       { binaryInstruction(i, "and", i->lhs, i->rhs); }
        void visitICmpNE(NNPtr<IR::Instrs::ICmpNE> i) override { binaryInstruction(i, "icmpne", i->lhs, i->rhs); }
        void visitICmpEQ(NNPtr<IR::Instrs::ICmpEQ> i) override { binaryInstruction(i, "icmpeq", i->lhs, i->rhs); }
        void visitICmpLT(NNPtr<IR::Instrs::ICmpLT> i) override { binaryInstruction(i, "icmplt", i->lhs, i->rhs); }
        void visitICmpGT(NNPtr<IR::Instrs::ICmpGT> i) override { binaryInstruction(i, "icmpgt", i->lhs, i->rhs); }
        void visitICmpLE(NNPtr<IR::Instrs::ICmpLE> i) override { binaryInstruction(i, "icmple", i->lhs, i->rhs); }
        void visitICmpGE(NNPtr<IR::Instrs::ICmpGE> i) override { binaryInstruction(i, "icmpge", i->lhs, i->rhs); }
        void visitFCmpNE(NNPtr<IR::Instrs::FCmpNE> i) override { binaryInstruction(i, "fcmpne", i->lhs, i->rhs); }
        void visitFCmpEQ(NNPtr<IR::Instrs::FCmpEQ> i) override { binaryInstruction(i, "fcmpeq", i->lhs, i->rhs); }
        void visitFCmpLT(NNPtr<IR::Instrs::FCmpLT> i) override { binaryInstruction(i, "fcmplt", i->lhs, i->rhs); }
        void visitFCmpGT(NNPtr<IR::Instrs::FCmpGT> i) override { binaryInstruction(i, "fcmpgt", i->lhs, i->rhs); }
        void visitFCmpLE(NNPtr<IR::Instrs::FCmpLE> i) override { binaryInstruction(i, "fcmple", i->lhs, i->rhs); }
        void visitFCmpGE(NNPtr<IR::Instrs::FCmpGE> i) override { binaryInstruction(i, "fcmpge", i->lhs, i->rhs); }
        void visitBitXor(NNPtr<IR::Instrs::BitXor> i) override { binaryInstruction(i, "bitxor", i->lhs, i->rhs); }
        void visitBitOr(NNPtr<IR::Instrs::BitOr> i) override   { binaryInstruction(i, "bitor", i->lhs, i->rhs); }
        void visitBitAnd(NNPtr<IR::Instrs::BitAnd> i) override { binaryInstruction(i, "bitand", i->lhs, i->rhs); }
        void visitShiftR(NNPtr<IR::Instrs::ShiftR> i) override { binaryInstruction(i, "shiftr", i->lhs, i->rhs); }
        void visitShiftL(NNPtr<IR::Instrs::ShiftL> i) override { binaryInstruction(i, "shiftl", i->lhs, i->rhs); }
        void visitIAdd(NNPtr<IR::Instrs::IAdd> i) override     { binaryInstruction(i, "iadd", i->lhs, i->rhs); }
        void visitISub(NNPtr<IR::Instrs::ISub> i) override     { binaryInstruction(i, "isub", i->lhs, i->rhs); }
        void visitIMult(NNPtr<IR::Instrs::IMult> i) override   { binaryInstruction(i, "imult", i->lhs, i->rhs); }
        void visitIDiv(NNPtr<IR::Instrs::IDiv> i) override     { binaryInstruction(i, "idiv", i->lhs, i->rhs); }
        void visitIMod(NNPtr<IR::Instrs::IMod> i) override     { binaryInstruction(i, "imod", i->lhs, i->rhs); }
        void visitFAdd(NNPtr<IR::Instrs::FAdd> i) override     { binaryInstruction(i, "fadd", i->lhs, i->rhs); }
        void visitFSub(NNPtr<IR::Instrs::FSub> i) override     { binaryInstruction(i, "fsub", i->lhs, i->rhs); }
        void visitFMult(NNPtr<IR::Instrs::FMult> i) override   { binaryInstruction(i, "fmult", i->lhs, i->rhs); }
        void visitFDiv(NNPtr<IR::Instrs::FDiv> i) override     { binaryInstruction(i, "fdiv", i->lhs, i->rhs); }
        void visitFMod(NNPtr<IR::Instrs::FMod> i) override     { binaryInstruction(i, "fmod", i->lhs, i->rhs); }
        // }}}
        // unary instructions {{{
        void visitNot(NNPtr<IR::Instrs::Not> i) override       { unaryInstruction(i, "not", i->op); }
        void visitBitNot(NNPtr<IR::Instrs::BitNot> i) override { unaryInstruction(i, "bitnot", i->op); }
        void visitINeg(NNPtr<IR::Instrs::INeg> i) override     { unaryInstruction(i, "ineg", i->op); }
        void visitFNeg(NNPtr<IR::Instrs::FNeg> i) override     { unaryInstruction(i, "fneg", i->op); }
        // }}}
        // cast instructions {{{
        void visitNoOpCast(NNPtr<IR::Instrs::NoOpCast> i) override         { castInstruction(i, "noopcast", i->op, i->newt); }
        void visitFloatToFloat(NNPtr<IR::Instrs::FloatToFloat> i) override { castInstruction(i, "ftof", i->op, i->newt); }
        void visitIntToInt(NNPtr<IR::Instrs::IntToInt> i) override         { castInstruction(i, "itoi", i->op, i->newt); }
        void visitIntToFloat(NNPtr<IR::Instrs::IntToFloat> i) override     { castInstruction(i, "itof", i->op, i->newt); }
        void visitFloatToInt(NNPtr<IR::Instrs::FloatToInt> i) override     { castInstruction(i, "ftoi", i->op, i->newt); }
        // }}}
        // pointer instructions {{{
        void visitDerefPtr(NNPtr<IR::Instrs::DerefPtr> i) override {
            unaryInstruction(i, "derefptr", i->ptr);
        }
        void visitAddrof(NNPtr<IR::Instrs::Addrof> i) override {
            instrName("addrof");
            i->deref->value_accept(pr.vrp.get());
            pr(", ");
            if (i->mut) pr("mut");
            else        pr("const");
            pr(")");
        }
        void visitPtrArith(NNPtr<IR::Instrs::PtrArith> i) override {
            binaryInstruction(i, "ptrarith", i->ptr, i->offset);
        }
        // }}}

        void visitCall(NNPtr<IR::Instrs::Call> i) override {
            instrName("call");
            i->f->value_accept(pr.vrp.get());
            for (IR::ASTValue const &v : i->args) {
                pr(", ");
                v.val->value_accept(pr.vrp.get());
            }
            pr(")");
        }

        // branches {{{
        void visitReturn(NNPtr<IR::Instrs::Return> i) override {
            instrName("return");
            i->value.val->value_accept(pr.vrp.get());
            pr(")");
        }
        void visitGotoBr(NNPtr<IR::Instrs::GotoBr> i) override {
            instrName("gotobr");
            stringifyBlock(*i->to);
            pr(")");
        }
        void visitCondBr(NNPtr<IR::Instrs::CondBr> i) override {
            instrName("condbr");
            i->v.val->value_accept(pr.vrp.get());
            pr(", true="); stringifyBlock(*i->trueB);
            pr(", false="); stringifyBlock(*i->falseB);
            pr(")");
        }
        // }}}
        // }}}
    };
    // }}}
    // Decl Symbol Decl Printer {{{
    class DSDPrinter final : public IR::DeclSymbolVisitor, public IR::TypeVisitor {
    public:
        DSDPrinter(_Printer &pr): pr(pr) {}
        _Printer &pr;
        // declsym {{{
        void declsym_visitType(NNPtr<IR::Type> ty) override {
            ty->type_accept(this);
        }
        void declsym_visitModule(NNPtr<IR::Module> mod) override {
            pr("mod {\n");
            walk(mod);
            pr("}\n");
        }
        // }}}
        // types {{{
        void type_visitFloatType(NNPtr<IR::FloatType> ty) override {
            pr("type builtin float ")(ty->size)(" {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visitIntType(NNPtr<IR::IntType> ty) override {
            pr("type builtin int ")(ty->isSigned ? "signed " : "unsigned ")(ty->size)(" {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visitCharType(NNPtr<IR::CharType> ty) override {
            pr("type builtin char {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visitBoolType(NNPtr<IR::BoolType> ty) override {
            pr("type builtin bool {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visitVoidType(NNPtr<IR::VoidType> ty) override {
            pr("type builtin void {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visitGenericIntType(NNPtr<IR::GenericIntType> ty) override {
            pr("type builtin generic int {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visitGenericFloatType(NNPtr<IR::GenericFloatType> ty) override {
            pr("type builtin generic float {\n");
            walk(ty);
            pr("}\n");
        }

        void type_visitPointerType(NNPtr<IR::PointerType> ty) override {
            pr("type pointer to ")(ty->ty->name())(" {\n");
            walk(ty);
            pr("}\n");
        }

        void type_visitFunctionType(NNPtr<IR::FunctionType> ty) override {
            pr("type functiontype ")((void*) ty.asRaw())(" ")(ty->name())(" {\n");
            walk(ty);
            pr("}\n");
        }
        // }}}
        // walk {{{
        void walk(NNPtr<IR::DeclSymbol> ds) {
            for (auto _ds : ds->getDeclSymbols()) {
                std::string name = _ds.first;
                NNPtr<IR::DeclSymbol> pds = _ds.second;

                pr(name)(" = ");
                pds->declsym_accept(pr.dsdp.get());
            }
            for (auto v : ds->getValues()) {
                std::string name = v.first;
                NNPtr<IR::Value> val = v.second;

                pr(name)(" = ");
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
            indent(0),
            lastnl(false),
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
