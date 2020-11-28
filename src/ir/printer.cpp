#include "ir/printer.h"
#include "ir/value.h"
#include "ir/type.h"

IR::Printer::Printer(llvm::raw_ostream &ostream): ostream(ostream) {}

void IR::Printer::visitStore(IR::Instrs::Store *i)
{
    ostream << "store ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->value.stringify();
}
void IR::Printer::visitOr(IR::Instrs::Or *i)
{
    ostream << "or ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitAnd(IR::Instrs::And *i)
{
    ostream << "and ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitCmpNE(IR::Instrs::CmpNE *i)
{
    ostream << "cmpne ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitCmpEQ(IR::Instrs::CmpEQ *i)
{
    ostream << "cmpeq ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitCmpLT(IR::Instrs::CmpLT *i)
{
    ostream << "cmplt ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitCmpGT(IR::Instrs::CmpGT *i)
{
    ostream << "cmpgt ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitCmpLE(IR::Instrs::CmpLE *i)
{
    ostream << "cmple ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitCmpGE(IR::Instrs::CmpGE *i)
{
    ostream << "cmpge ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitBitXor(IR::Instrs::BitXor *i)
{
    ostream << "bitxor ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitBitOr(IR::Instrs::BitOr *i)
{
    ostream << "bitor ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitBitAnd(IR::Instrs::BitAnd *i)
{
    ostream << "bitand ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitBitNot(IR::Instrs::BitNot *i)
{
    ostream << "bitnot ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->op.stringify();
}
void IR::Printer::visitShiftR(IR::Instrs::ShiftR *i)
{
    ostream << "shiftr ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitShiftL(IR::Instrs::ShiftL *i)
{
    ostream << "shiftl ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitAdd(IR::Instrs::Add *i)
{
    ostream << "add ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitSub(IR::Instrs::Sub *i)
{
    ostream << "sub ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitMult(IR::Instrs::Mult *i)
{
    ostream << "mult ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitDiv(IR::Instrs::Div *i)
{
    ostream << "div ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitMod(IR::Instrs::Mod *i)
{
    ostream << "mod ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->lhs.stringify();
    ostream << " ";
    ostream << i->rhs.stringify();
}
void IR::Printer::visitNeg(IR::Instrs::Neg *i)
{
    ostream << "neg ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->op.stringify();
}
void IR::Printer::visitTrunc(IR::Instrs::Trunc *i)
{
    ostream << "trunc ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->op.stringify();
    ostream << " ";
    ostream << i->newt->stringify();
}
void IR::Printer::visitExt(IR::Instrs::Ext *i)
{
    ostream << "ext ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->op.stringify();
    ostream << " ";
    ostream << i->newt->stringify();
}
void IR::Printer::visitIntToFloat(IR::Instrs::IntToFloat *i)
{
    ostream << "inttofloat ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->op.stringify();
    ostream << " ";
    ostream << i->newt->stringify();
}
void IR::Printer::visitFloatToInt(IR::Instrs::FloatToInt *i)
{
    ostream << "floattoint ";
    ostream << i->target->stringify();
    ostream << " ";
    ostream << i->op.stringify();
    ostream << " ";
    ostream << i->newt->stringify();
}
void IR::Printer::visitReturn(IR::Instrs::Return *i)
{
    ostream << "return ";
    ostream << (i->value ? i->value->stringify() : "void");
}
void IR::Printer::visitCall(IR::Instrs::Call *i)
{
    ostream << "call ";
    ostream << (i->reg ? i->reg->stringify() : "void");
    ostream << " ";
    ostream << i->f->stringify();
    ostream << " ";
    for (IR::ASTValue const &v : i->args)
    {
        ostream << v.stringify() << " ";
    }
}
void IR::Printer::visitGotoBr(IR::Instrs::GotoBr *i)
{
    ostream << "gotobr ";
    ostream << i->to->name;
}
void IR::Printer::visitCondBr(IR::Instrs::CondBr *i)
{
    ostream << "condbr ";
    ostream << i->v.stringify();
    ostream << " ";
    ostream << i->trueB->name;
    ostream << " ";
    ostream << i->falseB->name;
}
