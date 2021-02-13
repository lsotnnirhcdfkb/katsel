#pragma once

namespace IR { namespace Instrs { class DerefPtr; } class Value; class Function; class Register; class Type; class DeclSymbol; }
class Location;
class Span;

#include <vector>
#include <string>
#include "ast/astfwd.h"
#include "utils/location.h"

// ERRH START

// E0000 - unexpected-char
#define ERR_UNEXPECTED_CHAR E0000
void E0000(Span const &tok);

// E0001 - unterm-charlit
#define ERR_UNTERM_CHARLIT E0001
void E0001(Span const &tok);

// E0002 - unterm-strlit
#define ERR_UNTERM_STRLIT E0002
void E0002(Span const &tok);

// E0003 - invalid-numlit-base
#define ERR_INVALID_NUMLIT_BASE E0003
void E0003(Span const &tok);

// E0004 - nondecimal-floatlit
#define ERR_NONDECIMAL_FLOATLIT E0004
void E0004(Span const &tok);

// E0005 - invalid-char-for-base
#define ERR_INVALID_CHAR_FOR_BASE E0005
void E0005(Span const &tok);

// E0006 - intlit-no-digits
#define ERR_INTLIT_NO_DIGITS E0006
void E0006(Span const &tok);

// E0007 - multichar-charlit
#define ERR_MULTICHAR_CHARLIT E0007
void E0007(Span const &tok);

// E0008 - unterm-multiline-comment
#define ERR_UNTERM_MULTILINE_COMMENT E0008
void E0008(Span const &tok);

// E0009 - dedent-nomatch
#define ERR_DEDENT_NOMATCH E0009
void E0009(Span const &tok);

// E0010 - char-after-backslash
#define ERR_CHAR_AFTER_BACKSLASH E0010
void E0010(Span const &tok);

// E0011 - unrecoverable-invalid-syntax
#define ERR_UNRECOVERABLE_INVALID_SYNTAX E0011
void E0011(Span const &next, std::string lookahead_type_name, Span const &lasttok, std::initializer_list<std::string> const &expectations);

// E0012 - simple-invalid-syntax
#define ERR_SIMPLE_INVALID_SYNTAX E0012
void E0012(Span const &next, std::string lookahead_type_name, Span const &lasttok, std::initializer_list<std::string> const &expectations, std::string const &inserted_type);

// E0013 - skipping-invalid-syntax
#define ERR_SKIPPING_INVALID_SYNTAX E0013
void E0013(Span const &next, std::string lookahead_type_name, Span const &lasttok, std::initializer_list<std::string> const &expectations, Span const &replaced, std::string const &replacement_type);

// E0014 - lhs-unsupported-op
#define ERR_LHS_UNSUPPORTED_OP E0014
void E0014(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &op);

// E0015 - unary-unsupported-op
#define ERR_UNARY_UNSUPPORTED_OP E0015
void E0015(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op);

// E0016 - call-noncallable
#define ERR_CALL_NONCALLABLE E0016
void E0016(Located<NNPtr<IR::Value>> const &func, Span const &oparn);

// E0017 - incorrect-arg
#define ERR_INCORRECT_ARG E0017
void E0017(Located<NNPtr<IR::Value>> const &arg, IR::Type const &expected);

// E0018 - confl-tys-ifexpr
#define ERR_CONFL_TYS_IFEXPR E0018
void E0018(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok);

// E0019 - assign-conflict-tys
#define ERR_ASSIGN_CONFLICT_TYS E0019
void E0019(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq);

// E0020 - conflict-ret-ty
#define ERR_CONFLICT_RET_TY E0020
void E0020(Located<NNPtr<IR::Value>> const &val, IR::Function const &f);

// E0021 - no-deref
#define ERR_NO_DEREF E0021
void E0021(Span const &op, Located<NNPtr<IR::Value>> const &val);

// E0022 - conflict-var-init-ty
#define ERR_CONFLICT_VAR_INIT_TY E0022
void E0022(Span const &eq, Span const &name, ASTNS::Type const &type_ast, Located<NNPtr<IR::Value>> const &init, IR::Type const &expected_type);

// E0023 - invalid-cast
#define ERR_INVALID_CAST E0023
void E0023(ASTNS::AST const &ast, Located<NNPtr<IR::Value>> v, IR::Type const &newty);

// E0024 - conflict-tys-binary-op
#define ERR_CONFLICT_TYS_BINARY_OP E0024
void E0024(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Located<ASTNS::BinaryOperator> const &op);

// E0025 - cond-not-bool
#define ERR_COND_NOT_BOOL E0025
void E0025(Located<NNPtr<IR::Value>> const &v);

// E0026 - ptr-arith-rhs-not-num
#define ERR_PTR_ARITH_RHS_NOT_NUM E0026
void E0026(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs);

// E0027 - no-else-not-void
#define ERR_NO_ELSE_NOT_VOID E0027
void E0027(Located<NNPtr<IR::Value>> const &truev, Span const &iftok);

// E0028 - typeless-this
#define ERR_TYPELESS_THIS E0028
void E0028(ASTNS::ThisParam const &p);

// E0029 - wrong-num-args
#define ERR_WRONG_NUM_ARGS E0029
void E0029(IR::Function const &func, ASTNS::AST const &func_ref_ast, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args);

// E0030 - redecl-sym
#define ERR_REDECL_SYM E0030
void E0030(Span const &name, IR::Value const &val);

// E0031 - undecl-symb
#define ERR_UNDECL_SYMB E0031
void E0031(Span const &path);

// E0032 - redecl-param
#define ERR_REDECL_PARAM E0032
void E0032(ASTNS::ParamB const &param, IR::Register const &prev);

// E0033 - redecl-var
#define ERR_REDECL_VAR E0033
void E0033(Span const &name, IR::Register const &prev);

// E0034 - not-a-type
#define ERR_NOT_A_TYPE E0034
void E0034(Span const &notty, ASTNS::AST const &decl_ast);

// E0035 - no-member-in
#define ERR_NO_MEMBER_IN E0035
void E0035(IR::DeclSymbol const &prev, Span const &current);

// E0036 - no-this
#define ERR_NO_THIS E0036
void E0036(Span const &th);

// E0037 - no-method
#define ERR_NO_METHOD E0037
void E0037(Located<NNPtr<IR::Value>> const &op, Span const &name);

// E0038 - no-field
#define ERR_NO_FIELD E0038
void E0038(Located<NNPtr<IR::Value>> const &op, Span const &name);

// E0039 - addrof-not-lvalue
#define ERR_ADDROF_NOT_LVALUE E0039
void E0039(Span const &op, Located<NNPtr<IR::Value>> const &val);

// E0040 - assign-invalid-lhs
#define ERR_ASSIGN_INVALID_LHS E0040
void E0040(Span const &eq, Located<NNPtr<IR::Value>> const &lhs);

// E0041 - assign-not-mut
#define ERR_ASSIGN_NOT_MUT E0041
void E0041(Located<NNPtr<IR::Value>> const &v, Span const &eq, IR::Instrs::DerefPtr const &target_deref);

// E0042 - mut-addrof-nonmut-op
#define ERR_MUT_ADDROF_NONMUT_OP E0042
void E0042(Span const &op, IR::Instrs::DerefPtr const &as_deref);

// E0043 - no-suppress
#define ERR_NO_SUPPRESS E0043
void E0043(Span const &dollar);

// E0044 - this-not-first
#define ERR_THIS_NOT_FIRST E0044
void E0044(ASTNS::ThisParam const &ast);

// ===> warnings <===

// W0000 - Wextra-semi
#define WARN_EXTRA_SEMI W0000
void W0000(Span const &semi);

// W0001 - Wimmut-noinit
#define WARN_IMMUT_NOINIT W0001
void W0001(ASTNS::VarStmtItem const &ast);

// W0002 - Wblock-no-indent
#define WARN_BLOCK_NO_INDENT W0002
void W0002(Span const &obrace, Span const &cbrace);
// ERRH END
