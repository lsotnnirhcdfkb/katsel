#include "message/errmsgs.h"

// ERR DEFS START {{{
BadChar::BadChar(Span const &tok): tok(tok) {}
UntermCharlit::UntermCharlit(Span const &tok): tok(tok) {}
UntermStrlit::UntermStrlit(Span const &tok): tok(tok) {}
IndentBlockCbrace::IndentBlockCbrace(Span const &tok): tok(tok) {}
InvalidNumlitBase::InvalidNumlitBase(Span const &tok): tok(tok) {}
NondecimalFloatlit::NondecimalFloatlit(Span const &tok): tok(tok) {}
InvalidCharForBase::InvalidCharForBase(Span const &tok): tok(tok) {}
IntlitNoDigits::IntlitNoDigits(Span const &tok): tok(tok) {}
MulticharCharlit::MulticharCharlit(Span const &tok): tok(tok) {}
UntermMultilineComment::UntermMultilineComment(Span const &tok): tok(tok) {}
DedentNomatch::DedentNomatch(Span const &tok): tok(tok) {}
Expected::Expected(Span const &expected, std::string const &name): expected(expected), name(name) {}
LhsUnsupportedOp::LhsUnsupportedOp(Located<NNPtr<IR::Value>> const &lhs, Span const &op): lhs(lhs), op(op) {}
UnaryUnsupportedOp::UnaryUnsupportedOp(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op): operand(operand), op(op) {}
CallNoncallable::CallNoncallable(Located<NNPtr<IR::Value>> const &func, Span const &oparn): func(func), oparn(oparn) {}
IncorrectArg::IncorrectArg(Located<NNPtr<IR::Value>> const &arg, NNPtr<IR::Type> const &expected): arg(arg), expected(expected) {}
ConflTysIfexpr::ConflTysIfexpr(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok): truev(truev), falsev(falsev), iftok(iftok), elsetok(elsetok) {}
AssignConflictTys::AssignConflictTys(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq): lhs(lhs), rhs(rhs), eq(eq) {}
ConflictRetTy::ConflictRetTy(Located<NNPtr<IR::Value>> const &val, NNPtr<IR::Function> const &f): val(val), f(f) {}
NoDeref::NoDeref(Span const &op, Located<NNPtr<IR::Value>> const &val): op(op), val(val) {}
ConflictVarInitTy::ConflictVarInitTy(Span const &eq, Span const &name, NNPtr<ASTNS::Type> const &type_ast, Located<NNPtr<IR::Value>> const &init, NNPtr<IR::Type> const &expected_type): eq(eq), name(name), type_ast(type_ast), init(init), expected_type(expected_type) {}
InvalidCast::InvalidCast(NNPtr<ASTNS::AST> const &ast, Located<NNPtr<IR::Value>> const &v, NNPtr<IR::Type> const &newty): ast(ast), v(v), newty(newty) {}
ConflictTysBinaryOp::ConflictTysBinaryOp(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &op): lhs(lhs), rhs(rhs), op(op) {}
CondNotBool::CondNotBool(Located<NNPtr<IR::Value>> const &v): v(v) {}
PtrArithRhsNotNum::PtrArithRhsNotNum(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs): lhs(lhs), optok(optok), rhs(rhs) {}
NoElseNotVoid::NoElseNotVoid(Located<NNPtr<IR::Value>> const &truev, Span const &iftok): truev(truev), iftok(iftok) {}
TypelessThis::TypelessThis(NNPtr<ASTNS::ThisParam> const &p): p(p) {}
WrongNumArgs::WrongNumArgs(NNPtr<IR::Function> const &func, NNPtr<ASTNS::AST> const &func_ref_ast, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args): func(func), func_ref_ast(func_ref_ast), oparn(oparn), args(args) {}
RedeclSym::RedeclSym(Span const &name, NNPtr<IR::Value> const &val): name(name), val(val) {}
UndeclSymb::UndeclSymb(Span const &path): path(path) {}
RedeclParam::RedeclParam(NNPtr<ASTNS::ParamB> const &param, NNPtr<IR::Register> const &prev): param(param), prev(prev) {}
RedeclVar::RedeclVar(Span const &name, NNPtr<IR::Register> const &prev): name(name), prev(prev) {}
NotAType::NotAType(Span const &notty): notty(notty) {}
NoMemberIn::NoMemberIn(NNPtr<IR::DeclSymbol> const &prev, Span const &current): prev(prev), current(current) {}
NoThis::NoThis(Span const &th): th(th) {}
NoMethod::NoMethod(Located<NNPtr<IR::Value>> const &op, Span const &name): op(op), name(name) {}
NoField::NoField(Located<NNPtr<IR::Value>> const &op, Span const &name): op(op), name(name) {}
AddrofNotLvalue::AddrofNotLvalue(Span const &op, Located<NNPtr<IR::Value>> const &val): op(op), val(val) {}
AssignInvalidLhs::AssignInvalidLhs(Span const &eq, Located<NNPtr<IR::Value>> const &lhs): eq(eq), lhs(lhs) {}
AssignNotMut::AssignNotMut(Located<NNPtr<IR::Value>> const &v, Span const &eq, NNPtr<IR::Register> const &reg): v(v), eq(eq), reg(reg) {}
MutAddrofNonmutOp::MutAddrofNonmutOp(Span const &op, NNPtr<IR::Register> const &reg): op(op), reg(reg) {}
NoSuppress::NoSuppress(Span const &dollar): dollar(dollar) {}
ThisNotFirst::ThisNotFirst(NNPtr<ASTNS::ThisParam> const &ast): ast(ast) {}
// ERR DEFS END }}}
