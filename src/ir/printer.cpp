#include "ir/printer.h"
#include "ir/value.h"
#include "ir/type.h"
#include "utils/format.h"

IR::Printer::Printer(llvm::raw_ostream &ostream): ostream(ostream) {}

void IR::Printer::visitStore(IR::Instrs::Store *i)
{
    ostream << format("store % --> %", i->value.stringify(), i->target->stringify());
}
void IR::Printer::visitPhi(IR::Instrs::Phi *i)
{
    ostream << format("phi [");
    bool first = true;
    for (auto &p : i->prevs)
    {
        if (!first)
            ostream << ", ";

        p.first->stringify(ostream);
        ostream << format(": %", p.second);

        first = false;
    }
    ostream << format("] -> %", i->target);
}
void IR::Printer::visitOr(IR::Instrs::Or *i)
{
    ostream << format("or % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitAnd(IR::Instrs::And *i)
{
    ostream << format("and % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitCmpNE(IR::Instrs::CmpNE *i)
{
    ostream << format("cmpne % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitCmpEQ(IR::Instrs::CmpEQ *i)
{
    ostream << format("cmpeq % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitCmpLT(IR::Instrs::CmpLT *i)
{
    ostream << format("cmplt % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitCmpGT(IR::Instrs::CmpGT *i)
{
    ostream << format("cmpgt % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitCmpLE(IR::Instrs::CmpLE *i)
{
    ostream << format("cmple % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitCmpGE(IR::Instrs::CmpGE *i)
{
    ostream << format("cmpge % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitBitXor(IR::Instrs::BitXor *i)
{
    ostream << format("bitxor % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitBitOr(IR::Instrs::BitOr *i)
{
    ostream << format("bitor % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitBitAnd(IR::Instrs::BitAnd *i)
{
    ostream << format("bitand % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitBitNot(IR::Instrs::BitNot *i)
{
    ostream << format("bitnot % -> %", i->op, i->target);
}
void IR::Printer::visitShiftR(IR::Instrs::ShiftR *i)
{
    ostream << format("shiftr % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitShiftL(IR::Instrs::ShiftL *i)
{
    ostream << format("shiftl % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitAdd(IR::Instrs::Add *i)
{
    ostream << format("add % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitSub(IR::Instrs::Sub *i)
{
    ostream << format("sub % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitMult(IR::Instrs::Mult *i)
{
    ostream << format("mult % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitDiv(IR::Instrs::Div *i)
{
    ostream << format("div % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitMod(IR::Instrs::Mod *i)
{
    ostream << format("mod % % -> %", i->lhs, i->rhs, i->target);
}
void IR::Printer::visitNeg(IR::Instrs::Neg *i)
{
    ostream << format("neg % -> %", i->op, i->target);
}
void IR::Printer::visitTrunc(IR::Instrs::Trunc *i)
{
    ostream << format("trunc % > % -> %", i->op, i->newt, i->target);
}
void IR::Printer::visitExt(IR::Instrs::Ext *i)
{
    ostream << format("ext % > % -> %", i->op, i->newt, i->target);
}
void IR::Printer::visitIntToFloat(IR::Instrs::IntToFloat *i)
{
    ostream << format("inttofloat % > % -> %", i->op, i->newt, i->target);
}
void IR::Printer::visitFloatToInt(IR::Instrs::FloatToInt *i)
{
    ostream << format("floattoint % > % -> %", i->op, i->newt, i->target);
}
void IR::Printer::visitReturn(IR::Instrs::Return *i)
{
    ostream << format("return %", i->value ? i->value->stringify() : "void");
}
void IR::Printer::visitCall(IR::Instrs::Call *i)
{
    ostream << format("call % ( ", i->f);
    for (IR::ASTValue const &v : i->args)
        ostream << v.stringify() << " ";
    ostream << format(") -> %", i->reg);
}
void IR::Printer::visitGotoBr(IR::Instrs::GotoBr *i)
{
    ostream << format("gotobr ==> ");
    i->to->stringify(ostream);
}
void IR::Printer::visitCondBr(IR::Instrs::CondBr *i)
{
    ostream << format("condbr % ? ==> ", i->v);
    i->trueB->stringify(ostream);
    ostream << " : ==> ";
    i->falseB->stringify(ostream);
}
