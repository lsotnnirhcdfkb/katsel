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
        static constexpr char const *CODE = "E0000";
        static constexpr char const *NAME = "bad-char";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class UntermCharlit : public Error {
    public:
        UntermCharlit(Span const &tok);
    private:
        static constexpr char const *CODE = "E0001";
        static constexpr char const *NAME = "unterm-charlit";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class UntermStrlit : public Error {
    public:
        UntermStrlit(Span const &tok);
    private:
        static constexpr char const *CODE = "E0002";
        static constexpr char const *NAME = "unterm-strlit";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class IndentBlockCbrace : public Error {
    public:
        IndentBlockCbrace(Span const &tok);
    private:
        static constexpr char const *CODE = "E0003";
        static constexpr char const *NAME = "indent-block-cbrace";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class InvalidNumlitBase : public Error {
    public:
        InvalidNumlitBase(Span const &tok);
    private:
        static constexpr char const *CODE = "E0004";
        static constexpr char const *NAME = "invalid-numlit-base";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NondecimalFloatlit : public Error {
    public:
        NondecimalFloatlit(Span const &tok);
    private:
        static constexpr char const *CODE = "E0005";
        static constexpr char const *NAME = "nondecimal-floatlit";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class InvalidCharForBase : public Error {
    public:
        InvalidCharForBase(Span const &tok);
    private:
        static constexpr char const *CODE = "E0006";
        static constexpr char const *NAME = "invalid-char-for-base";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class IntlitNoDigits : public Error {
    public:
        IntlitNoDigits(Span const &tok);
    private:
        static constexpr char const *CODE = "E0007";
        static constexpr char const *NAME = "intlit-no-digits";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class MulticharCharlit : public Error {
    public:
        MulticharCharlit(Span const &tok);
    private:
        static constexpr char const *CODE = "E0008";
        static constexpr char const *NAME = "multichar-charlit";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class UntermMultilineComment : public Error {
    public:
        UntermMultilineComment(Span const &tok);
    private:
        static constexpr char const *CODE = "E0009";
        static constexpr char const *NAME = "unterm-multiline-comment";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class DedentNomatch : public Error {
    public:
        DedentNomatch(Span const &tok);
    private:
        static constexpr char const *CODE = "E0010";
        static constexpr char const *NAME = "dedent-nomatch";
        Span tok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class Expected : public Error {
    public:
        Expected(Span const &where, std::string const &what);
    private:
        static constexpr char const *CODE = "E0011";
        static constexpr char const *NAME = "expected";
        Span where;
        std::string what;
    protected:
        SimpleError to_simple_error() const override;
    };
    class LhsUnsupportedOp : public Error {
    public:
        LhsUnsupportedOp(Located<NNPtr<IR::Value>> const &lhs, Span const &op);
    private:
        static constexpr char const *CODE = "E0012";
        static constexpr char const *NAME = "lhs-unsupported-op";
        Located<NNPtr<IR::Value>> lhs;
        Span op;
    protected:
        SimpleError to_simple_error() const override;
    };
    class UnaryUnsupportedOp : public Error {
    public:
        UnaryUnsupportedOp(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op);
    private:
        static constexpr char const *CODE = "E0013";
        static constexpr char const *NAME = "unary-unsupported-op";
        Located<NNPtr<IR::Value>> operand;
        Located<ASTNS::UnaryOperator> op;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoCall : public Error {
    public:
        NoCall(Located<NNPtr<IR::Value>> const &func, Span const &oparn);
    private:
        static constexpr char const *CODE = "E0014";
        static constexpr char const *NAME = "no-call";
        Located<NNPtr<IR::Value>> func;
        Span oparn;
    protected:
        SimpleError to_simple_error() const override;
    };
    class IncorrectArg : public Error {
    public:
        IncorrectArg(Located<NNPtr<IR::Value>> const &arg, NNPtr<IR::Type const> const &expected);
    private:
        static constexpr char const *CODE = "E0015";
        static constexpr char const *NAME = "incorrect-arg";
        Located<NNPtr<IR::Value>> arg;
        NNPtr<IR::Type const> expected;
    protected:
        SimpleError to_simple_error() const override;
    };
    class ConflTysIfexpr : public Error {
    public:
        ConflTysIfexpr(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok);
    private:
        static constexpr char const *CODE = "E0016";
        static constexpr char const *NAME = "confl-tys-ifexpr";
        Located<NNPtr<IR::Value>> truev;
        Located<NNPtr<IR::Value>> falsev;
        Span iftok;
        Span elsetok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class AssignConflictTys : public Error {
    public:
        AssignConflictTys(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq);
    private:
        static constexpr char const *CODE = "E0017";
        static constexpr char const *NAME = "assign-conflict-tys";
        Located<NNPtr<IR::Value>> lhs;
        Located<NNPtr<IR::Value>> rhs;
        Span eq;
    protected:
        SimpleError to_simple_error() const override;
    };
    class ConflictRetTy : public Error {
    public:
        ConflictRetTy(Located<NNPtr<IR::Value>> const &val, NNPtr<IR::Function const> const &f);
    private:
        static constexpr char const *CODE = "E0018";
        static constexpr char const *NAME = "conflict-ret-ty";
        Located<NNPtr<IR::Value>> val;
        NNPtr<IR::Function const> f;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoDeref : public Error {
    public:
        NoDeref(Span const &op, Located<NNPtr<IR::Value>> const &val);
    private:
        static constexpr char const *CODE = "E0019";
        static constexpr char const *NAME = "no-deref";
        Span op;
        Located<NNPtr<IR::Value>> val;
    protected:
        SimpleError to_simple_error() const override;
    };
    class ConflictVarInitTy : public Error {
    public:
        ConflictVarInitTy(Span const &eq, Span const &name, NNPtr<ASTNS::Type> const &type_ast, Located<NNPtr<IR::Value>> const &init, NNPtr<IR::Type const> const &expected_type);
    private:
        static constexpr char const *CODE = "E0020";
        static constexpr char const *NAME = "conflict-var-init-ty";
        Span eq;
        Span name;
        NNPtr<ASTNS::Type> type_ast;
        Located<NNPtr<IR::Value>> init;
        NNPtr<IR::Type const> expected_type;
    protected:
        SimpleError to_simple_error() const override;
    };
    class InvalidCast : public Error {
    public:
        InvalidCast(NNPtr<ASTNS::AST const> const &ast, Located<NNPtr<IR::Value>> const &v, NNPtr<IR::Type const> const &newty);
    private:
        static constexpr char const *CODE = "E0021";
        static constexpr char const *NAME = "invalid-cast";
        NNPtr<ASTNS::AST const> ast;
        Located<NNPtr<IR::Value>> v;
        NNPtr<IR::Type const> newty;
    protected:
        SimpleError to_simple_error() const override;
    };
    class ConflictTysBinaryOp : public Error {
    public:
        ConflictTysBinaryOp(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &op);
    private:
        static constexpr char const *CODE = "E0022";
        static constexpr char const *NAME = "conflict-tys-binary-op";
        Located<NNPtr<IR::Value>> lhs;
        Located<NNPtr<IR::Value>> rhs;
        Span op;
    protected:
        SimpleError to_simple_error() const override;
    };
    class CondNotBool : public Error {
    public:
        CondNotBool(Located<NNPtr<IR::Value>> const &v);
    private:
        static constexpr char const *CODE = "E0023";
        static constexpr char const *NAME = "cond-not-bool";
        Located<NNPtr<IR::Value>> v;
    protected:
        SimpleError to_simple_error() const override;
    };
    class PtrArithRhsNotNum : public Error {
    public:
        PtrArithRhsNotNum(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs);
    private:
        static constexpr char const *CODE = "E0024";
        static constexpr char const *NAME = "ptr-arith-rhs-not-num";
        Located<NNPtr<IR::Value>> lhs;
        Located<ASTNS::BinaryOperator> optok;
        Located<NNPtr<IR::Value>> rhs;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoElseNotVoid : public Error {
    public:
        NoElseNotVoid(Located<NNPtr<IR::Value>> const &truev, Span const &iftok);
    private:
        static constexpr char const *CODE = "E0025";
        static constexpr char const *NAME = "no-else-not-void";
        Located<NNPtr<IR::Value>> truev;
        Span iftok;
    protected:
        SimpleError to_simple_error() const override;
    };
    class TypelessThis : public Error {
    public:
        TypelessThis(NNPtr<ASTNS::ThisParam> const &p);
    private:
        static constexpr char const *CODE = "E0026";
        static constexpr char const *NAME = "typeless-this";
        NNPtr<ASTNS::ThisParam> p;
    protected:
        SimpleError to_simple_error() const override;
    };
    class WrongNumArgs : public Error {
    public:
        WrongNumArgs(NNPtr<IR::Function const> const &func, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args);
    private:
        static constexpr char const *CODE = "E0027";
        static constexpr char const *NAME = "wrong-num-args";
        NNPtr<IR::Function const> func;
        Span oparn;
        std::vector<Located<NNPtr<IR::Value>>> args;
    protected:
        SimpleError to_simple_error() const override;
    };
    class RedeclSym : public Error {
    public:
        RedeclSym(Span const &name, NNPtr<IR::Value> const &val);
    private:
        static constexpr char const *CODE = "E0028";
        static constexpr char const *NAME = "redecl-sym";
        Span name;
        NNPtr<IR::Value> val;
    protected:
        SimpleError to_simple_error() const override;
    };
    class UndeclSymb : public Error {
    public:
        UndeclSymb(Span const &path);
    private:
        static constexpr char const *CODE = "E0029";
        static constexpr char const *NAME = "undecl-symb";
        Span path;
    protected:
        SimpleError to_simple_error() const override;
    };
    class RedeclParam : public Error {
    public:
        RedeclParam(NNPtr<ASTNS::ParamB> const &param, NNPtr<IR::Register> const &prev);
    private:
        static constexpr char const *CODE = "E0030";
        static constexpr char const *NAME = "redecl-param";
        NNPtr<ASTNS::ParamB> param;
        NNPtr<IR::Register> prev;
    protected:
        SimpleError to_simple_error() const override;
    };
    class RedeclVar : public Error {
    public:
        RedeclVar(Span const &name, NNPtr<IR::Register> const &prev);
    private:
        static constexpr char const *CODE = "E0031";
        static constexpr char const *NAME = "redecl-var";
        Span name;
        NNPtr<IR::Register> prev;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NotA_Type : public Error {
    public:
        NotA_Type(Span const &notty);
    private:
        static constexpr char const *CODE = "E0032";
        static constexpr char const *NAME = "not-a-type";
        Span notty;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoMemberIn : public Error {
    public:
        NoMemberIn(NNPtr<IR::DeclSymbol> const &prev, Span const &current);
    private:
        static constexpr char const *CODE = "E0033";
        static constexpr char const *NAME = "no-member-in";
        NNPtr<IR::DeclSymbol> prev;
        Span current;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoThis : public Error {
    public:
        NoThis(Span const &th);
    private:
        static constexpr char const *CODE = "E0034";
        static constexpr char const *NAME = "no-this";
        Span th;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoMethod : public Error {
    public:
        NoMethod(Located<NNPtr<IR::Value>> const &op, Span const &name);
    private:
        static constexpr char const *CODE = "E0035";
        static constexpr char const *NAME = "no-method";
        Located<NNPtr<IR::Value>> op;
        Span name;
    protected:
        SimpleError to_simple_error() const override;
    };
    class NoField : public Error {
    public:
        NoField(Located<NNPtr<IR::Value>> const &op, Span const &name);
    private:
        static constexpr char const *CODE = "E0036";
        static constexpr char const *NAME = "no-field";
        Located<NNPtr<IR::Value>> op;
        Span name;
    protected:
        SimpleError to_simple_error() const override;
    };
    class AddrofNotLvalue : public Error {
    public:
        AddrofNotLvalue(Span const &op, Located<NNPtr<IR::Value>> const &val);
    private:
        static constexpr char const *CODE = "E0037";
        static constexpr char const *NAME = "addrof-not-lvalue";
        Span op;
        Located<NNPtr<IR::Value>> val;
    protected:
        SimpleError to_simple_error() const override;
    };
    class AssignInvalidLhs : public Error {
    public:
        AssignInvalidLhs(Span const &eq, Located<NNPtr<IR::Value>> const &lhs);
    private:
        static constexpr char const *CODE = "E0038";
        static constexpr char const *NAME = "assign-invalid-lhs";
        Span eq;
        Located<NNPtr<IR::Value>> lhs;
    protected:
        SimpleError to_simple_error() const override;
    };
    class AssignNotMut : public Error {
    public:
        AssignNotMut(Located<NNPtr<IR::Value>> const &v, Span const &eq, NNPtr<IR::Register> const &reg);
    private:
        static constexpr char const *CODE = "E0039";
        static constexpr char const *NAME = "assign-not-mut";
        Located<NNPtr<IR::Value>> v;
        Span eq;
        NNPtr<IR::Register> reg;
    protected:
        SimpleError to_simple_error() const override;
    };
    class MutAddrofNonmutOp : public Error {
    public:
        MutAddrofNonmutOp(Span const &op, NNPtr<IR::Register> const &reg);
    private:
        static constexpr char const *CODE = "E0040";
        static constexpr char const *NAME = "mut-addrof-nonmut-op";
        Span op;
        NNPtr<IR::Register> reg;
    protected:
        SimpleError to_simple_error() const override;
    };
    class ThisNotFirst : public Error {
    public:
        ThisNotFirst(NNPtr<ASTNS::ThisParam> const &ast);
    private:
        static constexpr char const *CODE = "E0041";
        static constexpr char const *NAME = "this-not-first";
        NNPtr<ASTNS::ThisParam> ast;
    protected:
        SimpleError to_simple_error() const override;
    };
    // ERR DECLS END }}}
}
