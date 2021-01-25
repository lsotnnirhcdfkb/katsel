#include "ir/printer.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/unit.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/block.h"
#include "utils/format.h"
#include "ir/visitor.h"

IR::Printer::Printer(IR::Unit &unit, llvm::raw_ostream &ostream): unit(unit), ostream(ostream) {}

namespace {
    // id_to_str {{{
    std::string id_to_str(uint64_t id) {
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

        void value_visit(IR::ConstBool const &v) override {
            pr(v.val ? "true" : "false");
        }
        void value_visit(IR::ConstChar const &v) override {
            pr("'")(v.val)("'");
        }
        void value_visit(IR::ConstInt const &v) override {
            pr(v.val);
        }
        void value_visit(IR::ConstFloat const &v) override {
            pr(v.val);
        }
        void value_visit(IR::Function const &v) override {
            pr(v.name);
        }
        void value_visit(IR::Instrs::Instruction const &v) override {
            pr(id_to_str(v.id));
        }
        void value_visit(IR::Void const &v) override {
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
        void value_visit(IR::ConstBool const &)  override { report_abort_noh("print declaratino of ConstBool"); }
        void value_visit(IR::ConstChar const &)  override { report_abort_noh("print declaration of ConstChar"); }
        void value_visit(IR::ConstInt const &)   override { report_abort_noh("print declaration of ConstInt"); }
        void value_visit(IR::ConstFloat const &) override { report_abort_noh("print declaration of ConstFloat"); }
        void value_visit(IR::Void const &)       override { report_abort_noh("print declaration of Void"); }
        // }}}
        // visit Instruction {{{
        void value_visit(IR::Instrs::Instruction const &v) override {
            pr("(")(id_to_str(v.id))(" :: ")(v.type().name())(") = ");
            v.accept(*this);
            pr(";\n");
        }
        // }}}
        // Function {{{
        void value_visit(IR::Function const &fun) override {
            pr(format("fun {}", fun.type()));
            if (fun.prototypeonly) {
                pr(" (prototype);\n");
                return;
            } else {
                pr(" {\n");
            }

            for (std::unique_ptr<IR::Block> const &block : fun.blocks)
                print_block(*block);
            pr("}\n");
        }

        void print_block(IR::Block &b) {
            stringify_block(b);

            pr(": {\n");

            for (std::unique_ptr<IR::Instrs::Instruction> const &instr : b.instructions) {
                instr->value_accept(*this);
            }
            if (b.br) {
                pr("=>: ");
                b.br->accept(*this);
                pr(";\n");
            }
            pr("}\n");
        }
        // }}}
        // all the instructions {{{
        // helpers {{{
        void instr_name(std::string const &s) {
            pr(s)("(");
        }
        void stringify_block(IR::Block const &b) {
            pr(format("{}({})", b.name, b.num));
        }
        void binary_instruction(IR::Instrs::Instruction const &i, std::string const &name, IR::ASTValue const &lhs, IR::ASTValue const &rhs) {
            instr_name(name);
            lhs.val->value_accept(*pr.vrp);
            pr(", ");
            rhs.val->value_accept(*pr.vrp);
            pr(")");
        }
        void unary_instruction(IR::Instrs::Instruction const &i, std::string const &name, IR::ASTValue const &op) {
            instr_name(name);
            op.val->value_accept(*pr.vrp);
            pr(")");
        }
        void cast_instruction(IR::Instrs::Instruction const &i, std::string const &name, IR::ASTValue const &op, NNPtr<IR::Type const> const to) {
            instr_name(name);
            op.val->value_accept(*pr.vrp);
            pr(", ")(to->name());
            pr(")");
        }
        // }}}

        void visit(IR::Instrs::Store const &i) override {
            instr_name("store");
            i.value.val->value_accept(*pr.vrp);

            pr(", ");
            if (i.init)
                pr("init");
            else
                pr("noinit");
            pr(", ");

            i.target.val->value_accept(*pr.vrp);
            pr(")");
        }
        void visit(IR::Instrs::Phi const &i) override {
            instr_name("phi");
            bool first = true;
            for (auto &p : i.prevs) {
                if (!first)
                    pr(", ");

                stringify_block(*p.first);
                pr(": ");
                p.second.val->value_accept(*pr.vrp);

                first = false;
            }
            pr(")");
        }
        void visit(IR::Instrs::Register const &i) override {
            instr_name("register");
            pr(i.ty->name());
            if (i.mut) pr(", mut");
            else        pr(", const");
            pr(")");
        }

        // binary instructions {{{
        void visit(IR::Instrs::Or const &i)     override { binary_instruction(i, "or", i.lhs, i.rhs); }
        void visit(IR::Instrs::And const &i)    override { binary_instruction(i, "and", i.lhs, i.rhs); }
        void visit(IR::Instrs::ICmpNE const &i) override { binary_instruction(i, "icmpne", i.lhs, i.rhs); }
        void visit(IR::Instrs::ICmpEQ const &i) override { binary_instruction(i, "icmpeq", i.lhs, i.rhs); }
        void visit(IR::Instrs::ICmpLT const &i) override { binary_instruction(i, "icmplt", i.lhs, i.rhs); }
        void visit(IR::Instrs::ICmpGT const &i) override { binary_instruction(i, "icmpgt", i.lhs, i.rhs); }
        void visit(IR::Instrs::ICmpLE const &i) override { binary_instruction(i, "icmple", i.lhs, i.rhs); }
        void visit(IR::Instrs::ICmpGE const &i) override { binary_instruction(i, "icmpge", i.lhs, i.rhs); }
        void visit(IR::Instrs::FCmpNE const &i) override { binary_instruction(i, "fcmpne", i.lhs, i.rhs); }
        void visit(IR::Instrs::FCmpEQ const &i) override { binary_instruction(i, "fcmpeq", i.lhs, i.rhs); }
        void visit(IR::Instrs::FCmpLT const &i) override { binary_instruction(i, "fcmplt", i.lhs, i.rhs); }
        void visit(IR::Instrs::FCmpGT const &i) override { binary_instruction(i, "fcmpgt", i.lhs, i.rhs); }
        void visit(IR::Instrs::FCmpLE const &i) override { binary_instruction(i, "fcmple", i.lhs, i.rhs); }
        void visit(IR::Instrs::FCmpGE const &i) override { binary_instruction(i, "fcmpge", i.lhs, i.rhs); }
        void visit(IR::Instrs::BitXor const &i) override { binary_instruction(i, "bitxor", i.lhs, i.rhs); }
        void visit(IR::Instrs::BitOr const &i)  override { binary_instruction(i, "bitor", i.lhs, i.rhs); }
        void visit(IR::Instrs::BitAnd const &i) override { binary_instruction(i, "bitand", i.lhs, i.rhs); }
        void visit(IR::Instrs::ShiftR const &i) override { binary_instruction(i, "shiftr", i.lhs, i.rhs); }
        void visit(IR::Instrs::ShiftL const &i) override { binary_instruction(i, "shiftl", i.lhs, i.rhs); }
        void visit(IR::Instrs::IAdd const &i)   override { binary_instruction(i, "iadd", i.lhs, i.rhs); }
        void visit(IR::Instrs::ISub const &i)   override { binary_instruction(i, "isub", i.lhs, i.rhs); }
        void visit(IR::Instrs::IMult const &i)  override { binary_instruction(i, "imult", i.lhs, i.rhs); }
        void visit(IR::Instrs::IDiv const &i)   override { binary_instruction(i, "idiv", i.lhs, i.rhs); }
        void visit(IR::Instrs::IMod const &i)   override { binary_instruction(i, "imod", i.lhs, i.rhs); }
        void visit(IR::Instrs::FAdd const &i)   override { binary_instruction(i, "fadd", i.lhs, i.rhs); }
        void visit(IR::Instrs::FSub const &i)   override { binary_instruction(i, "fsub", i.lhs, i.rhs); }
        void visit(IR::Instrs::FMult const &i)  override { binary_instruction(i, "fmult", i.lhs, i.rhs); }
        void visit(IR::Instrs::FDiv const &i)   override { binary_instruction(i, "fdiv", i.lhs, i.rhs); }
        void visit(IR::Instrs::FMod const &i)   override { binary_instruction(i, "fmod", i.lhs, i.rhs); }
        // }}}
        // unary instructions {{{
        void visit(IR::Instrs::Not const &i)       override { unary_instruction(i, "not", i.op); }
        void visit(IR::Instrs::BitNot const &i)    override { unary_instruction(i, "bitnot", i.op); }
        void visit(IR::Instrs::INeg const &i)      override { unary_instruction(i, "ineg", i.op); }
        void visit(IR::Instrs::FNeg const &i)      override { unary_instruction(i, "fneg", i.op); }
        // }}}
        // cast instructions {{{
        void visit(IR::Instrs::NoOpCast const &i)     override { cast_instruction(i, "noopcast", i.op, i.newt); }
        void visit(IR::Instrs::FloatToFloat const &i) override { cast_instruction(i, "ftof", i.op, i.newt); }
        void visit(IR::Instrs::IntToInt const &i)     override { cast_instruction(i, "itoi", i.op, i.newt); }
        void visit(IR::Instrs::IntToFloat const &i)   override { cast_instruction(i, "itof", i.op, i.newt); }
        void visit(IR::Instrs::FloatToInt const &i)   override { cast_instruction(i, "ftoi", i.op, i.newt); }
        // }}}
        // pointer instructions {{{
        void visit(IR::Instrs::DerefPtr const &i) override {
            unary_instruction(i, "derefptr", i.ptr);
        }
        void visit(IR::Instrs::Addrof const &i) override {
            instr_name("addrof");
            i.deref->value_accept(*pr.vrp);
            pr(", ");
            if (i.mut) pr("mut");
            else        pr("const");
            pr(")");
        }
        void visit(IR::Instrs::PtrArith const &i) override {
            binary_instruction(i, "ptrarith", i.ptr, i.offset);
        }
        // }}}

        void visit(IR::Instrs::Call const &i) override {
            instr_name("call");
            i.f->value_accept(*pr.vrp);
            for (IR::ASTValue const &v : i.args) {
                pr(", ");
                v.val->value_accept(*pr.vrp);
            }
            pr(")");
        }

        // branches {{{
        void visit(IR::Instrs::Return const &i) override {
            instr_name("return");
            i.value.val->value_accept(*pr.vrp);
            pr(")");
        }
        void visit(IR::Instrs::GotoBr const &i) override {
            instr_name("gotobr");
            stringify_block(*i.to);
            pr(")");
        }
        void visit(IR::Instrs::CondBr const &i) override {
            instr_name("condbr");
            i.v.val->value_accept(*pr.vrp);
            pr(", true="); stringify_block(*i.true_b);
            pr(", false="); stringify_block(*i.false_b);
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
        void declsym_visit(IR::Type const &ty) override {
            ty.type_accept(*this);
        }
        void declsym_visit(IR::Module const &mod) override {
            pr("mod {\n");
            walk(mod);
            pr("}\n");
        }
        // }}}
        // types {{{
        void type_visit(IR::FloatType const &ty) override {
            pr("type builtin float ")(ty.size)(" {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visit(IR::IntType const &ty) override {
            pr("type builtin int ")(ty.is_signed ? "signed " : "unsigned ")(ty.size)(" {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visit(IR::CharType const &ty) override {
            pr("type builtin char {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visit(IR::BoolType const &ty) override {
            pr("type builtin bool {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visit(IR::VoidType const &ty) override {
            pr("type builtin void {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visit(IR::GenericIntType const &ty) override {
            pr("type builtin generic int {\n");
            walk(ty);
            pr("}\n");
        }
        void type_visit(IR::GenericFloatType const &ty) override {
            pr("type builtin generic float {\n");
            walk(ty);
            pr("}\n");
        }

        void type_visit(IR::PointerType const &ty) override {
            pr("type pointer to ")(ty.ty->name())(" {\n");
            walk(ty);
            pr("}\n");
        }

        void type_visit(IR::FunctionType const &ty) override {
            pr("type functiontype ")((void*) &ty)(" ")(ty.name())(" {\n");
            walk(ty);
            pr("}\n");
        }
        // }}}
        // walk {{{
        void walk(IR::DeclSymbol const &ds) {
            for (auto _ds : ds.get_decl_symbols()) {
                std::string name = _ds.first;
                NNPtr<IR::DeclSymbol> pds = _ds.second;

                pr(name)(" = ");
                pds->declsym_accept(*pr.dsdp);
            }
            for (auto v : ds.get_values()) {
                std::string name = v.first;
                NNPtr<IR::Value> val = v.second;

                pr(name)(" = ");
                val->value_accept(*pr.vdp);
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
        unit.mod.declsym_accept(*dsdp);
    }
    // }}}
}

void IR::Printer::print() {
    _Printer p (unit, ostream);
    p.print();
}
