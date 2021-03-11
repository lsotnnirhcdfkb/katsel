#include "message/errmsgs.h"

// ERR DEFS START {{{
using Errors::BadChar;
BadChar::BadChar(Span const &tok): tok(tok) {}
using Errors::UntermCharlit;
UntermCharlit::UntermCharlit(Span const &tok): tok(tok) {}
using Errors::UntermStrlit;
UntermStrlit::UntermStrlit(Span const &tok): tok(tok) {}
using Errors::IndentBlockCbrace;
IndentBlockCbrace::IndentBlockCbrace(Span const &tok): tok(tok) {}
using Errors::InvalidNumlitBase;
InvalidNumlitBase::InvalidNumlitBase(Span const &tok): tok(tok) {}
using Errors::NondecimalFloatlit;
NondecimalFloatlit::NondecimalFloatlit(Span const &tok): tok(tok) {}
using Errors::InvalidCharForBase;
InvalidCharForBase::InvalidCharForBase(Span const &tok): tok(tok) {}
using Errors::IntlitNoDigits;
IntlitNoDigits::IntlitNoDigits(Span const &tok): tok(tok) {}
using Errors::MulticharCharlit;
MulticharCharlit::MulticharCharlit(Span const &tok): tok(tok) {}
using Errors::UntermMultilineComment;
UntermMultilineComment::UntermMultilineComment(Span const &tok): tok(tok) {}
using Errors::DedentNomatch;
DedentNomatch::DedentNomatch(Span const &tok): tok(tok) {}
using Errors::Expected;
Expected::Expected(Span const &expected, std::string const &name): expected(expected), name(name) {}
using Errors::LhsUnsupportedOp;
LhsUnsupportedOp::LhsUnsupportedOp(Located<NNPtr<IR::Value>> const &lhs, Span const &op): lhs(lhs), op(op) {}
using Errors::UnaryUnsupportedOp;
UnaryUnsupportedOp::UnaryUnsupportedOp(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op): operand(operand), op(op) {}
using Errors::CallNoncallable;
CallNoncallable::CallNoncallable(Located<NNPtr<IR::Value>> const &func, Span const &oparn): func(func), oparn(oparn) {}
using Errors::IncorrectArg;
IncorrectArg::IncorrectArg(Located<NNPtr<IR::Value>> const &arg, NNPtr<IR::Type> const &expected): arg(arg), expected(expected) {}
using Errors::ConflTysIfexpr;
ConflTysIfexpr::ConflTysIfexpr(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok): truev(truev), falsev(falsev), iftok(iftok), elsetok(elsetok) {}
using Errors::AssignConflictTys;
AssignConflictTys::AssignConflictTys(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq): lhs(lhs), rhs(rhs), eq(eq) {}
using Errors::ConflictRetTy;
ConflictRetTy::ConflictRetTy(Located<NNPtr<IR::Value>> const &val, NNPtr<IR::Function> const &f): val(val), f(f) {}
using Errors::NoDeref;
NoDeref::NoDeref(Span const &op, Located<NNPtr<IR::Value>> const &val): op(op), val(val) {}
using Errors::ConflictVarInitTy;
ConflictVarInitTy::ConflictVarInitTy(Span const &eq, Span const &name, NNPtr<ASTNS::Type> const &type_ast, Located<NNPtr<IR::Value>> const &init, NNPtr<IR::Type> const &expected_type): eq(eq), name(name), type_ast(type_ast), init(init), expected_type(expected_type) {}
using Errors::InvalidCast;
InvalidCast::InvalidCast(NNPtr<ASTNS::AST> const &ast, Located<NNPtr<IR::Value>> const &v, NNPtr<IR::Type> const &newty): ast(ast), v(v), newty(newty) {}
using Errors::ConflictTysBinaryOp;
ConflictTysBinaryOp::ConflictTysBinaryOp(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &op): lhs(lhs), rhs(rhs), op(op) {}
using Errors::CondNotBool;
CondNotBool::CondNotBool(Located<NNPtr<IR::Value>> const &v): v(v) {}
using Errors::PtrArithRhsNotNum;
PtrArithRhsNotNum::PtrArithRhsNotNum(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs): lhs(lhs), optok(optok), rhs(rhs) {}
using Errors::NoElseNotVoid;
NoElseNotVoid::NoElseNotVoid(Located<NNPtr<IR::Value>> const &truev, Span const &iftok): truev(truev), iftok(iftok) {}
using Errors::TypelessThis;
TypelessThis::TypelessThis(NNPtr<ASTNS::ThisParam> const &p): p(p) {}
using Errors::WrongNumArgs;
WrongNumArgs::WrongNumArgs(NNPtr<IR::Function> const &func, NNPtr<ASTNS::AST> const &func_ref_ast, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args): func(func), func_ref_ast(func_ref_ast), oparn(oparn), args(args) {}
using Errors::RedeclSym;
RedeclSym::RedeclSym(Span const &name, NNPtr<IR::Value> const &val): name(name), val(val) {}
using Errors::UndeclSymb;
UndeclSymb::UndeclSymb(Span const &path): path(path) {}
using Errors::RedeclParam;
RedeclParam::RedeclParam(NNPtr<ASTNS::ParamB> const &param, NNPtr<IR::Register> const &prev): param(param), prev(prev) {}
using Errors::RedeclVar;
RedeclVar::RedeclVar(Span const &name, NNPtr<IR::Register> const &prev): name(name), prev(prev) {}
using Errors::NotAType;
NotAType::NotAType(Span const &notty): notty(notty) {}
using Errors::NoMemberIn;
NoMemberIn::NoMemberIn(NNPtr<IR::DeclSymbol> const &prev, Span const &current): prev(prev), current(current) {}
using Errors::NoThis;
NoThis::NoThis(Span const &th): th(th) {}
using Errors::NoMethod;
NoMethod::NoMethod(Located<NNPtr<IR::Value>> const &op, Span const &name): op(op), name(name) {}
using Errors::NoField;
NoField::NoField(Located<NNPtr<IR::Value>> const &op, Span const &name): op(op), name(name) {}
using Errors::AddrofNotLvalue;
AddrofNotLvalue::AddrofNotLvalue(Span const &op, Located<NNPtr<IR::Value>> const &val): op(op), val(val) {}
using Errors::AssignInvalidLhs;
AssignInvalidLhs::AssignInvalidLhs(Span const &eq, Located<NNPtr<IR::Value>> const &lhs): eq(eq), lhs(lhs) {}
using Errors::AssignNotMut;
AssignNotMut::AssignNotMut(Located<NNPtr<IR::Value>> const &v, Span const &eq, NNPtr<IR::Register> const &reg): v(v), eq(eq), reg(reg) {}
using Errors::MutAddrofNonmutOp;
MutAddrofNonmutOp::MutAddrofNonmutOp(Span const &op, NNPtr<IR::Register> const &reg): op(op), reg(reg) {}
using Errors::NoSuppress;
NoSuppress::NoSuppress(Span const &dollar): dollar(dollar) {}
using Errors::ThisNotFirst;
ThisNotFirst::ThisNotFirst(NNPtr<ASTNS::ThisParam> const &ast): ast(ast) {}
// ERR DEFS END }}}
