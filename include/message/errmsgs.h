#pragma once

class Token;
namespace IR { namespace Instrs { class Register; class DerefPtr; } class Value; class Function; class Type; struct ASTValue; class DeclSymbol; }
class Location;

#include <vector>
#include <string>
#include "ast/astfwd.h"

// ERRH START

// E0000 - unexpected-char
#define ERR_UNEXPECTED_CHAR E0000
void E0000(Token const &tok);

// E0001 - unterm-charlit
#define ERR_UNTERM_CHARLIT E0001
void E0001(Token const &tok);

// E0002 - unterm-strlit
#define ERR_UNTERM_STRLIT E0002
void E0002(Token const &tok);

// E0003 - invalid-intlit-base
#define ERR_INVALID_INTLIT_BASE E0003
void E0003(Token const &tok);

// E0004 - nondecimal-floatlit
#define ERR_NONDECIMAL_FLOATLIT E0004
void E0004(Token const &tok);

// E0005 - invalid-char-floatlit
#define ERR_INVALID_CHAR_FLOATLIT E0005
void E0005(Token const &tok);

// E0006 - invalid-char-for-base
#define ERR_INVALID_CHAR_FOR_BASE E0006
void E0006(Token const &tok);

// E0007 - intlit-no-digits
#define ERR_INTLIT_NO_DIGITS E0007
void E0007(Token const &tok);

// E0008 - multichar-charlit
#define ERR_MULTICHAR_CHARLIT E0008
void E0008(Token const &tok);

// E0009 - unterm-multiline-comment
#define ERR_UNTERM_MULTILINE_COMMENT E0009
void E0009(Token const &tok);

// E0010 - dedent-nomatch
#define ERR_DEDENT_NOMATCH E0010
void E0010(Token const &tok);

// E0011 - char-after-backslash
#define ERR_CHAR_AFTER_BACKSLASH E0011
void E0011(Token const &tok);

// E0012 - unrecoverable-invalid-syntax
#define ERR_UNRECOVERABLE_INVALID_SYNTAX E0012
void E0012(Token const &lookahead, Token const &lasttok, std::vector<std::string> const &expectations);

// E0013 - simple-invalid-syntax
#define ERR_SIMPLE_INVALID_SYNTAX E0013
void E0013(Token const &lookahead, Token const &lasttok, std::string const &bestfix, std::vector<std::string> const &expectations);

// E0014 - panicking-invalid-syntax
#define ERR_PANICKING_INVALID_SYNTAX E0014
void E0014(Token const &lookahead, Token const &lasttok, Token const &panicuntil, std::vector<std::string> const &expectations);

// E0015 - lhs-unsupported-op
#define ERR_LHS_UNSUPPORTED_OP E0015
void E0015(IR::ASTValue const &lhs, Token const &op);

// E0016 - unary-unsupported-op
#define ERR_UNARY_UNSUPPORTED_OP E0016
void E0016(IR::ASTValue const &operand, Token const &_operator);

// E0017 - call-noncallable
#define ERR_CALL_NONCALLABLE E0017
void E0017(IR::ASTValue const &func, Token const &oparn);

// E0018 - incorrect-arg
#define ERR_INCORRECT_ARG E0018
void E0018(IR::ASTValue const &arg, IR::Type const &expected);

// E0019 - confl-tys-ifexpr
#define ERR_CONFL_TYS_IFEXPR E0019
void E0019(IR::ASTValue const &truev, IR::ASTValue const &falsev, Token const &iftok, Token const &elsetok);

// E0020 - assign-conflict-tys
#define ERR_ASSIGN_CONFLICT_TYS E0020
void E0020(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &eq);

// E0021 - conflict-ret-ty
#define ERR_CONFLICT_RET_TY E0021
void E0021(IR::ASTValue const &val, IR::Function const &f);

// E0022 - no-deref
#define ERR_NO_DEREF E0022
void E0022(Token const &op, IR::ASTValue const &val);

// E0023 - conflict-var-init-ty
#define ERR_CONFLICT_VAR_INIT_TY E0023
void E0023(Token const &eq, Token const &name, ASTNS::Type const &type_ast, IR::ASTValue const &init, IR::Type const &expected_type);

// E0024 - invalid-cast
#define ERR_INVALID_CAST E0024
void E0024(ASTNS::AST const &ast, IR::ASTValue v, IR::Type const &newty);

// E0025 - conflict-tys-binary-op
#define ERR_CONFLICT_TYS_BINARY_OP E0025
void E0025(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &op);

// E0026 - cond-not-bool
#define ERR_COND_NOT_BOOL E0026
void E0026(IR::ASTValue const &v);

// E0027 - ptr-arith-rhs-not-num
#define ERR_PTR_ARITH_RHS_NOT_NUM E0027
void E0027(IR::ASTValue const &lhs, Token const &optok, IR::ASTValue const &rhs);

// E0028 - no-else-not-void
#define ERR_NO_ELSE_NOT_VOID E0028
void E0028(IR::ASTValue const &truev, Token const &iftok);

// E0029 - typeless-this
#define ERR_TYPELESS_THIS E0029
void E0029(ASTNS::ThisParam const &p);

// E0030 - wrong-num-args
#define ERR_WRONG_NUM_ARGS E0030
void E0030(IR::Function const &func, ASTNS::AST const &func_ref_ast, Token const &oparn, std::vector<IR::ASTValue> const &args);

// E0031 - redecl-sym
#define ERR_REDECL_SYM E0031
void E0031(Token const &name, IR::Value const &val);

// E0032 - undecl-symb
#define ERR_UNDECL_SYMB E0032
void E0032(Location const &path);

// E0033 - redecl-param
#define ERR_REDECL_PARAM E0033
void E0033(ASTNS::ParamB const &param, IR::Instrs::Register const &prev);

// E0034 - redecl-var
#define ERR_REDECL_VAR E0034
void E0034(Token const &name, IR::Instrs::Register const &prev);

// E0035 - not-a-type
#define ERR_NOT_A_TYPE E0035
void E0035(Location const &notty, ASTNS::AST const &decl_ast);

// E0036 - no-member-in
#define ERR_NO_MEMBER_IN E0036
void E0036(IR::DeclSymbol const &prev, Token const &current);

// E0037 - no-this
#define ERR_NO_THIS E0037
void E0037(Token const &th);

// E0038 - no-method
#define ERR_NO_METHOD E0038
void E0038(IR::ASTValue const &op, Token const &name);

// E0039 - no-field
#define ERR_NO_FIELD E0039
void E0039(IR::ASTValue const &op, Token const &name);

// E0040 - addrof-not-lvalue
#define ERR_ADDROF_NOT_LVALUE E0040
void E0040(Token const &op, IR::ASTValue const &val);

// E0041 - assign-invalid-lhs
#define ERR_ASSIGN_INVALID_LHS E0041
void E0041(Token const &eq, IR::ASTValue const &lhs);

// E0042 - assign-not-mut
#define ERR_ASSIGN_NOT_MUT E0042
void E0042(IR::ASTValue const &v, Token const &eq, IR::Instrs::DerefPtr const &target_deref);

// E0043 - mut-addrof-nonmut-op
#define ERR_MUT_ADDROF_NONMUT_OP E0043
void E0043(Token const &op, IR::Instrs::DerefPtr const &as_deref);

// E0044 - no-suppress
#define ERR_NO_SUPPRESS E0044
void E0044(Location const &dot);

// E0045 - this-not-first
#define ERR_THIS_NOT_FIRST E0045
void E0045(ASTNS::ThisParam const &ast);

// ===> warnings <===

// W0000 - Wextra-semi
#define WARN_EXTRA_SEMI W0000
void W0000(Token const &semi);

// W0001 - Wimmut-noinit
#define WARN_IMMUT_NOINIT W0001
void W0001(ASTNS::VarStmtItem const &ast);

// W0002 - Wblock-no-indent
#define WARN_BLOCK_NO_INDENT W0002
void W0002(Token const &obrace, Token const &cbrace);
// ERRH END
