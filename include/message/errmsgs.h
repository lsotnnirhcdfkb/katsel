#pragma once

namespace IR { namespace Instrs { class DerefPtr; } class Value; class Function; class Register; class Type; class DeclSymbol; }
class Location;
class Span;

#include <vector>
#include <string>
#include "ast/astfwd.h"
#include "utils/location.h"
#include "message/errors.h"

namespace Errors {
    // ERR DECLS START {{{
    class BadChar : public Error {
    public:
        BadChar(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class UntermCharlit : public Error {
    public:
        UntermCharlit(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class UntermStrlit : public Error {
    public:
        UntermStrlit(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class IndentBlockCbrace : public Error {
    public:
        IndentBlockCbrace(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class InvalidNumlitBase : public Error {
    public:
        InvalidNumlitBase(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NondecimalFloatlit : public Error {
    public:
        NondecimalFloatlit(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class InvalidCharForBase : public Error {
    public:
        InvalidCharForBase(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class IntlitNoDigits : public Error {
    public:
        IntlitNoDigits(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class MulticharCharlit : public Error {
    public:
        MulticharCharlit(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class UntermMultilineComment : public Error {
    public:
        UntermMultilineComment(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class DedentNomatch : public Error {
    public:
        DedentNomatch(Span const &tok);
    private:
        Span tok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class Expected : public Error {
    public:
        Expected(Span const &expected, std::string const &name);
    private:
        Span expected;
        std::string name;
    protected:
        SimpleError toSimpleError() const override;
    };
    class LhsUnsupportedOp : public Error {
    public:
        LhsUnsupportedOp(Located<NNPtr<IR::Value>> const &lhs, Span const &op);
    private:
        Located<NNPtr<IR::Value>> lhs;
        Span op;
    protected:
        SimpleError toSimpleError() const override;
    };
    class UnaryUnsupportedOp : public Error {
    public:
        UnaryUnsupportedOp(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op);
    private:
        Located<NNPtr<IR::Value>> operand;
        Located<ASTNS::UnaryOperator> op;
    protected:
        SimpleError toSimpleError() const override;
    };
    class CallNoncallable : public Error {
    public:
        CallNoncallable(Located<NNPtr<IR::Value>> const &func, Span const &oparn);
    private:
        Located<NNPtr<IR::Value>> func;
        Span oparn;
    protected:
        SimpleError toSimpleError() const override;
    };
    class IncorrectArg : public Error {
    public:
        IncorrectArg(Located<NNPtr<IR::Value>> const &arg, NNPtr<IR::Type> const &expected);
    private:
        Located<NNPtr<IR::Value>> arg;
        NNPtr<IR::Type> expected;
    protected:
        SimpleError toSimpleError() const override;
    };
    class ConflTysIfexpr : public Error {
    public:
        ConflTysIfexpr(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok);
    private:
        Located<NNPtr<IR::Value>> truev;
        Located<NNPtr<IR::Value>> falsev;
        Span iftok;
        Span elsetok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class AssignConflictTys : public Error {
    public:
        AssignConflictTys(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq);
    private:
        Located<NNPtr<IR::Value>> lhs;
        Located<NNPtr<IR::Value>> rhs;
        Span eq;
    protected:
        SimpleError toSimpleError() const override;
    };
    class ConflictRetTy : public Error {
    public:
        ConflictRetTy(Located<NNPtr<IR::Value>> const &val, NNPtr<IR::Function> const &f);
    private:
        Located<NNPtr<IR::Value>> val;
        NNPtr<IR::Function> f;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoDeref : public Error {
    public:
        NoDeref(Span const &op, Located<NNPtr<IR::Value>> const &val);
    private:
        Span op;
        Located<NNPtr<IR::Value>> val;
    protected:
        SimpleError toSimpleError() const override;
    };
    class ConflictVarInitTy : public Error {
    public:
        ConflictVarInitTy(Span const &eq, Span const &name, NNPtr<ASTNS::Type> const &type_ast, Located<NNPtr<IR::Value>> const &init, NNPtr<IR::Type> const &expected_type);
    private:
        Span eq;
        Span name;
        NNPtr<ASTNS::Type> type_ast;
        Located<NNPtr<IR::Value>> init;
        NNPtr<IR::Type> expected_type;
    protected:
        SimpleError toSimpleError() const override;
    };
    class InvalidCast : public Error {
    public:
        InvalidCast(NNPtr<ASTNS::AST> const &ast, Located<NNPtr<IR::Value>> const &v, NNPtr<IR::Type> const &newty);
    private:
        NNPtr<ASTNS::AST> ast;
        Located<NNPtr<IR::Value>> v;
        NNPtr<IR::Type> newty;
    protected:
        SimpleError toSimpleError() const override;
    };
    class ConflictTysBinaryOp : public Error {
    public:
        ConflictTysBinaryOp(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &op);
    private:
        Located<NNPtr<IR::Value>> lhs;
        Located<NNPtr<IR::Value>> rhs;
        Span op;
    protected:
        SimpleError toSimpleError() const override;
    };
    class CondNotBool : public Error {
    public:
        CondNotBool(Located<NNPtr<IR::Value>> const &v);
    private:
        Located<NNPtr<IR::Value>> v;
    protected:
        SimpleError toSimpleError() const override;
    };
    class PtrArithRhsNotNum : public Error {
    public:
        PtrArithRhsNotNum(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs);
    private:
        Located<NNPtr<IR::Value>> lhs;
        Located<ASTNS::BinaryOperator> optok;
        Located<NNPtr<IR::Value>> rhs;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoElseNotVoid : public Error {
    public:
        NoElseNotVoid(Located<NNPtr<IR::Value>> const &truev, Span const &iftok);
    private:
        Located<NNPtr<IR::Value>> truev;
        Span iftok;
    protected:
        SimpleError toSimpleError() const override;
    };
    class TypelessThis : public Error {
    public:
        TypelessThis(NNPtr<ASTNS::ThisParam> const &p);
    private:
        NNPtr<ASTNS::ThisParam> p;
    protected:
        SimpleError toSimpleError() const override;
    };
    class WrongNumArgs : public Error {
    public:
        WrongNumArgs(NNPtr<IR::Function> const &func, NNPtr<ASTNS::AST> const &func_ref_ast, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args);
    private:
        NNPtr<IR::Function> func;
        NNPtr<ASTNS::AST> func_ref_ast;
        Span oparn;
        std::vector<Located<NNPtr<IR::Value>>> args;
    protected:
        SimpleError toSimpleError() const override;
    };
    class RedeclSym : public Error {
    public:
        RedeclSym(Span const &name, NNPtr<IR::Value> const &val);
    private:
        Span name;
        NNPtr<IR::Value> val;
    protected:
        SimpleError toSimpleError() const override;
    };
    class UndeclSymb : public Error {
    public:
        UndeclSymb(Span const &path);
    private:
        Span path;
    protected:
        SimpleError toSimpleError() const override;
    };
    class RedeclParam : public Error {
    public:
        RedeclParam(NNPtr<ASTNS::ParamB> const &param, NNPtr<IR::Register> const &prev);
    private:
        NNPtr<ASTNS::ParamB> param;
        NNPtr<IR::Register> prev;
    protected:
        SimpleError toSimpleError() const override;
    };
    class RedeclVar : public Error {
    public:
        RedeclVar(Span const &name, NNPtr<IR::Register> const &prev);
    private:
        Span name;
        NNPtr<IR::Register> prev;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NotAType : public Error {
    public:
        NotAType(Span const &notty);
    private:
        Span notty;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoMemberIn : public Error {
    public:
        NoMemberIn(NNPtr<IR::DeclSymbol> const &prev, Span const &current);
    private:
        NNPtr<IR::DeclSymbol> prev;
        Span current;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoThis : public Error {
    public:
        NoThis(Span const &th);
    private:
        Span th;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoMethod : public Error {
    public:
        NoMethod(Located<NNPtr<IR::Value>> const &op, Span const &name);
    private:
        Located<NNPtr<IR::Value>> op;
        Span name;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoField : public Error {
    public:
        NoField(Located<NNPtr<IR::Value>> const &op, Span const &name);
    private:
        Located<NNPtr<IR::Value>> op;
        Span name;
    protected:
        SimpleError toSimpleError() const override;
    };
    class AddrofNotLvalue : public Error {
    public:
        AddrofNotLvalue(Span const &op, Located<NNPtr<IR::Value>> const &val);
    private:
        Span op;
        Located<NNPtr<IR::Value>> val;
    protected:
        SimpleError toSimpleError() const override;
    };
    class AssignInvalidLhs : public Error {
    public:
        AssignInvalidLhs(Span const &eq, Located<NNPtr<IR::Value>> const &lhs);
    private:
        Span eq;
        Located<NNPtr<IR::Value>> lhs;
    protected:
        SimpleError toSimpleError() const override;
    };
    class AssignNotMut : public Error {
    public:
        AssignNotMut(Located<NNPtr<IR::Value>> const &v, Span const &eq, NNPtr<IR::Register> const &reg);
    private:
        Located<NNPtr<IR::Value>> v;
        Span eq;
        NNPtr<IR::Register> reg;
    protected:
        SimpleError toSimpleError() const override;
    };
    class MutAddrofNonmutOp : public Error {
    public:
        MutAddrofNonmutOp(Span const &op, NNPtr<IR::Register> const &reg);
    private:
        Span op;
        NNPtr<IR::Register> reg;
    protected:
        SimpleError toSimpleError() const override;
    };
    class NoSuppress : public Error {
    public:
        NoSuppress(Span const &dollar);
    private:
        Span dollar;
    protected:
        SimpleError toSimpleError() const override;
    };
    class ThisNotFirst : public Error {
    public:
        ThisNotFirst(NNPtr<ASTNS::ThisParam> const &ast);
    private:
        NNPtr<ASTNS::ThisParam> ast;
    protected:
        SimpleError toSimpleError() const override;
    };
    // ERR DECLS END }}}
}
