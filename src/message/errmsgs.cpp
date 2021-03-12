#include "message/errmsgs.h"
#include "message/sections.h"
#include "message/ansistuff.h"

#include "ir/type.h"
#include "ir/value.h"
#include "ir/function.h"

#include "ast/ast.h"

#include "utils/format.h"

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
Expected::Expected(Span const &where, std::string const &what): where(where), what(what) {}
using Errors::LhsUnsupportedOp;
LhsUnsupportedOp::LhsUnsupportedOp(Located<NNPtr<IR::Value>> const &lhs, Span const &op): lhs(lhs), op(op) {}
using Errors::UnaryUnsupportedOp;
UnaryUnsupportedOp::UnaryUnsupportedOp(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op): operand(operand), op(op) {}
using Errors::NoCall;
NoCall::NoCall(Located<NNPtr<IR::Value>> const &func, Span const &oparn): func(func), oparn(oparn) {}
using Errors::IncorrectArg;
IncorrectArg::IncorrectArg(Located<NNPtr<IR::Value>> const &arg, NNPtr<IR::Type const> const &expected): arg(arg), expected(expected) {}
using Errors::ConflTysIfexpr;
ConflTysIfexpr::ConflTysIfexpr(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok): truev(truev), falsev(falsev), iftok(iftok), elsetok(elsetok) {}
using Errors::AssignConflictTys;
AssignConflictTys::AssignConflictTys(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq): lhs(lhs), rhs(rhs), eq(eq) {}
using Errors::ConflictRetTy;
ConflictRetTy::ConflictRetTy(Located<NNPtr<IR::Value>> const &val, NNPtr<IR::Function const> const &f): val(val), f(f) {}
using Errors::NoDeref;
NoDeref::NoDeref(Span const &op, Located<NNPtr<IR::Value>> const &val): op(op), val(val) {}
using Errors::ConflictVarInitTy;
ConflictVarInitTy::ConflictVarInitTy(Span const &eq, Span const &name, NNPtr<ASTNS::Type> const &type_ast, Located<NNPtr<IR::Value>> const &init, NNPtr<IR::Type const> const &expected_type): eq(eq), name(name), type_ast(type_ast), init(init), expected_type(expected_type) {}
using Errors::InvalidCast;
InvalidCast::InvalidCast(NNPtr<ASTNS::AST const> const &ast, Located<NNPtr<IR::Value>> const &v, NNPtr<IR::Type const> const &newty): ast(ast), v(v), newty(newty) {}
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
WrongNumArgs::WrongNumArgs(NNPtr<IR::Function const> const &func, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args): func(func), oparn(oparn), args(args) {}
using Errors::RedeclSym;
RedeclSym::RedeclSym(Span const &name, NNPtr<IR::Value> const &val): name(name), val(val) {}
using Errors::UndeclSymb;
UndeclSymb::UndeclSymb(Span const &path): path(path) {}
using Errors::RedeclParam;
RedeclParam::RedeclParam(NNPtr<ASTNS::ParamB> const &param, NNPtr<IR::Register> const &prev): param(param), prev(prev) {}
using Errors::RedeclVar;
RedeclVar::RedeclVar(Span const &name, NNPtr<IR::Register> const &prev): name(name), prev(prev) {}
using Errors::NotA_Type;
NotA_Type::NotA_Type(Span const &notty): notty(notty) {}
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
using Errors::ThisNotFirst;
ThisNotFirst::ThisNotFirst(NNPtr<ASTNS::ThisParam> const &ast): ast(ast) {}
// ERR DEFS END }}}

#define ERROR_COLOR A_FG_RED A_BOLD
#define NOTE_COLOR A_FG_GREEN
#define HINT_COLOR A_FG_BLUE A_BOLD

#define UNDER0 '^'
#define UNDER1 '~'
#define UNDER2 '.'

using namespace Errors::Sections;

#define SIMPLE_ERROR_CONSTRUCT(where) SimpleError(SimpleError::Type::ERROR, where, CODE, NAME)

Errors::SimpleError BadChar::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "bad character", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError UntermCharlit::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "unclosed character literal", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError UntermStrlit::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "unclosed string literal", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError IndentBlockCbrace::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "indentation block cannot be closed by explicit '}'", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError InvalidNumlitBase::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "invalid integer base", ERROR_COLOR },
            Underlines::Message { tok, UNDER1, "base must be one of 'x', 'b', 'o', or omitted for decimal numbers", HINT_COLOR },
        }))
        .value();
}
Errors::SimpleError NondecimalFloatlit::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "non-decimal floating point literal", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError InvalidCharForBase::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "invalid digit for base", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError IntlitNoDigits::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "integer literal with no digits", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError MulticharCharlit::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "character literal with more than one character", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError UntermMultilineComment::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "multiline comment is missing closing '*/'", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError DedentNomatch::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(tok)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { tok, UNDER0, "dedent to level that does not match any other indentation level", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError Expected::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(where)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { where, UNDER0, format("expected {}", what), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError LhsUnsupportedOp::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(op)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { op, UNDER0, "unsupported binary operator", ERROR_COLOR },
            Underlines::Message { lhs.span, UNDER1, format("with left operand of type {}", lhs.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError UnaryUnsupportedOp::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(op.span)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { op.span, UNDER0, "unsupported unary operator", ERROR_COLOR },
            Underlines::Message { operand.span, UNDER1, format("with operand of type {}", operand.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError NoCall::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(func.span)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { func.span, UNDER0, format("cannot call value of type {}", func.value->type()), ERROR_COLOR },
        }))
        .value();
}

Errors::SimpleError IncorrectArg::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(arg.span)
        .section(std::make_unique<Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { arg.span, UNDER0, format("wrong type passed to function call: {}", arg.value->type()), ERROR_COLOR },
            Underlines::Message { arg.span, UNDER1, format("function expects {} for this argument", *expected), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError ConflTysIfexpr::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(iftok)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { iftok, UNDER0, "conflicting types for branches of if expression", ERROR_COLOR },
            Underlines::Message { truev.span, UNDER1, format("{}", truev.value->type()), NOTE_COLOR },
            Underlines::Message { falsev.span, UNDER1, format("{}", falsev.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError AssignConflictTys::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(eq)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { eq, UNDER0, "conflicting types for assignment", ERROR_COLOR },
            Underlines::Message { lhs.span, UNDER1, format("{}", lhs.value->type()), NOTE_COLOR },
            Underlines::Message { rhs.span, UNDER1, format("{}", rhs.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError ConflictRetTy::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(val.span)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { val.span, UNDER0, "conflicting return type", ERROR_COLOR },
            Underlines::Message { val.span, UNDER0, format("returning {}", val.value->type()), NOTE_COLOR },
            Underlines::Message { f->def_span(), UNDER1, format("function returns {}", *f->ty->ret), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError NoDeref::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(val.span)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { op, UNDER0, format("dereferencing of non-pointer type {}", val.value->type()), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError ConflictVarInitTy::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(eq)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { init.span, UNDER0, "conflicting types for variable initialization", ERROR_COLOR },
            Underlines::Message { init.span, UNDER0, format("{}", init.value->type()), NOTE_COLOR },
            Underlines::Message { type_ast->span(), UNDER1, format("{}", *expected_type), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError InvalidCast::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(ast->span())
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { ast->span(), UNDER0, format("invalid cast from {} to {}", v.value->type(), *newty), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError ConflictTysBinaryOp::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(op)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { op, UNDER0, "conflicting types to binary operator", ERROR_COLOR },
            Underlines::Message { lhs.span, UNDER1, format("{}", lhs.value->type()), NOTE_COLOR },
            Underlines::Message { rhs.span, UNDER1, format("{}", rhs.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError CondNotBool::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(v.span)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { v.span, UNDER0, format("usage of {} as condition", v.value->type()), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError PtrArithRhsNotNum::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(optok.span)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { optok.span, UNDER0, "pointer arithmetic requires an integral right-hand operand", ERROR_COLOR },
            Underlines::Message { rhs.span, UNDER1, format("{}", rhs.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError NoElseNotVoid::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(iftok)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { iftok, UNDER0, "if expression with non-void true expression and no else case", ERROR_COLOR },
            Underlines::Message { truev.span, UNDER1, format("{}", truev.value->type()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError TypelessThis::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(p->span())
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { p->span(), UNDER0, "'this' parameter not allowed outside of impl or class block", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError WrongNumArgs::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(oparn)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { oparn, UNDER0, "wrong number of arguments to function call", ERROR_COLOR },
            Underlines::Message { func->def_span(), UNDER1, format("function expects {} arguments, but got {} arguments", func->ty->paramtys.size(), args.size()), NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError RedeclSym::to_simple_error() const {
    auto se = SIMPLE_ERROR_CONSTRUCT(name);
    auto sec = std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
        Underlines::Message { name, UNDER0, "redeclaration of symbol", ERROR_COLOR },
    });

    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(val.as_raw()))
        sec->messages.push_back(Errors::Sections::Underlines::Message { as_declared->def_span(), UNDER1, "previous declaration", NOTE_COLOR });

    se.section(std::move(sec));

    return se;
}
Errors::SimpleError UndeclSymb::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(path)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { path, UNDER0, "undeclared symbol", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError RedeclParam::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(param->span())
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { param->span(), UNDER0, "redeclaration of parameter", ERROR_COLOR },
            Underlines::Message { prev->def_span(), UNDER1, "previous declaration", NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError RedeclVar::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(name)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { name, UNDER0, "redeclaration of variable", ERROR_COLOR },
            Underlines::Message { prev->def_span(), UNDER1, "previous declaration", NOTE_COLOR },
        }))
        .value();
}
Errors::SimpleError NotA_Type::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(notty)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { notty, UNDER0, "not a type", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError NoMemberIn::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(current)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { current, UNDER0, format("no member called {} in {}", current.stringify(), *prev), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError NoThis::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(th)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { th, UNDER0, "usage of 'this' outside method", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError NoMethod::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(name)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { name, UNDER0, format("no method called '{}' on value of type {}", name.stringify(), op.value->type()), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError NoField::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(name)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { name, UNDER0, format("no field called '{}' on value of type {}", name.stringify(), op.value->type()), ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError AddrofNotLvalue::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(val.span)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { op, UNDER0, "taking address of non-lvalue", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError AssignInvalidLhs::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(eq)
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { eq, UNDER0, "non-lvalue assignment", ERROR_COLOR },
        }))
        .value();
}
Errors::SimpleError AssignNotMut::to_simple_error() const {
    auto se = SIMPLE_ERROR_CONSTRUCT(v.span);
    auto sec = std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
        Underlines::Message { eq, UNDER0, "cannot assign to immutable lvalue", ERROR_COLOR },
    });

    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(reg.as_raw()))
        sec->messages.push_back(Errors::Sections::Underlines::Message { as_declared->def_span(), UNDER1, "variable declared immutable here", NOTE_COLOR });

    se.section(std::move(sec));
    return se;
}
Errors::SimpleError MutAddrofNonmutOp::to_simple_error() const {
    auto se = SIMPLE_ERROR_CONSTRUCT(op);
    auto sec = std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
        Underlines::Message { op, UNDER0, "cannot take mutable pointer to non-mutable lvalue", ERROR_COLOR },
    });

    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(reg.as_raw()))
        sec->messages.push_back(Errors::Sections::Underlines::Message { as_declared->def_span(), UNDER1, "value declared immutable here", NOTE_COLOR });

    se.section(std::move(sec));

    return se;
}
Errors::SimpleError ThisNotFirst::to_simple_error() const {
    return SIMPLE_ERROR_CONSTRUCT(ast->span())
        .section(std::make_unique<Sections::Underlines>(std::vector<Underlines::Message> {
            Underlines::Message { ast->span(), UNDER0, "'this' parameter must be the first parameter of a method", ERROR_COLOR },
        }))
        .value();
}
