#include "lowererlocal.h"

using namespace Lower;

LowerValueDef::LowerValueDef(Lowerer &lowerer): lowerer(lowerer) {}
void LowerValueDef::value_visit(IR::ConstBool  const &v) { report_abort_noh("lower def of ConstBool"); }
void LowerValueDef::value_visit(IR::ConstChar  const &v) { report_abort_noh("lower def of ConstChar"); }
void LowerValueDef::value_visit(IR::ConstInt   const &v) { report_abort_noh("lower def of ConstInt"); }
void LowerValueDef::value_visit(IR::ConstFloat const &v) { report_abort_noh("lower def of ConstFloat"); }
void LowerValueDef::value_visit(IR::Void       const &v) { report_abort_noh("lower def of ConstVoid"); }
void LowerValueDef::value_visit(IR::Register const &reg) { report_abort_noh("lower def of Register"); }

void LowerValueDef::value_visit(IR::Function const &v) {
    LowerFunction fl (lowerer, v);
    fl.lower();
}
