#pragma once

struct Token;
namespace IR { namespace Instrs { class Register; class DerefPtr; } class Value; class Function; class Type; struct ASTValue; class DeclSymbol; }

#include <vector>
#include <string>
#include "ast/ast.h"

// ERRH START
// The following code was autogenerated - see the utils/ directory

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

// E0100 - unrecoverable-invalid-syntax
#define ERR_UNRECOVERABLE_INVALID_SYNTAX E0100
void E0100(Token const &lookahead, Token const &lasttok, std::vector<std::string> const &expectations);

// E0101 - simple-invalid-syntax
#define ERR_SIMPLE_INVALID_SYNTAX E0101
void E0101(Token const &lookahead, Token const &lasttok, std::string const &bestfix, std::vector<std::string> const &expectations);

// E0102 - panicking-invalid-syntax
#define ERR_PANICKING_INVALID_SYNTAX E0102
void E0102(Token const &lookahead, Token const &lasttok, Token const &panicuntil, std::vector<std::string> const &expectations);

// E0200 - redecl-sym
#define ERR_REDECL_SYM E0200
void E0200(Token const &name, IR::Value *val);

// E0201 - lhs-unsupported-op
#define ERR_LHS_UNSUPPORTED_OP E0201
void E0201(IR::ASTValue const &lhs, Token const &op);

// E0202 - addrof-not-lvalue
#define ERR_ADDROF_NOT_LVALUE E0202
void E0202(Token const &op, IR::ASTValue const &val);

// E0203 - unary-unsupported-op
#define ERR_UNARY_UNSUPPORTED_OP E0203
void E0203(IR::ASTValue const &operand, Token const &_operator);

// E0204 - call-noncallable
#define ERR_CALL_NONCALLABLE E0204
void E0204(IR::ASTValue const &func, Token const &oparn);

// E0205 - wrong-num-args
#define ERR_WRONG_NUM_ARGS E0205
void E0205(IR::ASTValue const &func, Token const &oparn, std::vector<IR::ASTValue> const &args);

// E0206 - incorrect-arg
#define ERR_INCORRECT_ARG E0206
void E0206(IR::ASTValue const &arg, IR::Type const *expected);

// E0207 - undecl-symb
#define ERR_UNDECL_SYMB E0207
void E0207(Location const &path);

// E0208 - confl-tys-ifexpr
#define ERR_CONFL_TYS_IFEXPR E0208
void E0208(IR::ASTValue const &truev, IR::ASTValue const &falsev, Token const &iftok, Token const &elsetok);

// E0209 - assign-invalid-lhs
#define ERR_ASSIGN_INVALID_LHS E0209
void E0209(Token const &eq, IR::ASTValue const &lhs);

// E0210 - assign-conflict-tys
#define ERR_ASSIGN_CONFLICT_TYS E0210
void E0210(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &eq);

// E0212 - conflict-ret-ty
#define ERR_CONFLICT_RET_TY E0212
void E0212(IR::ASTValue const &val, IR::Function *f);

// E0213 - no-deref
#define ERR_NO_DEREF E0213
void E0213(Token const &op, IR::ASTValue const &val);

// E0214 - redecl-var
#define ERR_REDECL_VAR E0214
void E0214(Token const &name, IR::Instrs::Register const *prev);

// E0215 - conflict-var-init-ty
#define ERR_CONFLICT_VAR_INIT_TY E0215
void E0215(Token const &eq, Token const &name, ASTNS::Type *typeAST, IR::ASTValue const &init, IR::Type const *expectedType);

// E0216 - invalid-cast
#define ERR_INVALID_CAST E0216
void E0216(ASTNS::AST *ast, IR::ASTValue v, IR::Type const *newty);

// E0217 - conflict-tys-binary-op
#define ERR_CONFLICT_TYS_BINARY_OP E0217
void E0217(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &op);

// E0218 - redecl-param
#define ERR_REDECL_PARAM E0218
void E0218(Token const &name, IR::Instrs::Register const *prev);

// E0219 - cond-not-bool
#define ERR_COND_NOT_BOOL E0219
void E0219(IR::ASTValue &v);

// E0220 - no-else-not-void
#define ERR_NO_ELSE_NOT_VOID E0220
void E0220(IR::ASTValue const &truev, Token const &iftok);

// E0221 - ptr-arith-rhs-not-num
#define ERR_PTR_ARITH_RHS_NOT_NUM E0221
void E0221(IR::ASTValue const &lhs, Token const &optok, IR::ASTValue const &rhs);

// E0222 - not-a-type
#define ERR_NOT_A_TYPE E0222
void E0222(Location const &notty, ASTNS::AST *declAST);

// E0223 - no-item-in
#define ERR_NO_ITEM_IN E0223
void E0223(IR::DeclSymbol const *prev, Token const &current);

// E0224 - assign-not-mut
#define ERR_ASSIGN_NOT_MUT E0224
void E0224(IR::ASTValue const &v, Token const &eq, IR::Instrs::DerefPtr *targetDeref);

// E0225 - mut-addrof-nonmut-op
#define ERR_MUT_ADDROF_NONMUT_OP E0225
void E0225(Token const &op, IR::Instrs::DerefPtr *asDeref);

// E0226 - no-suppress
#define ERR_NO_SUPPRESS E0226
void E0226(Location const &dot);

// W0000 - Wextra-semi
#define WARN_EXTRA_SEMI W0000
void W0000(Token const &semi);

// This code was autogenerated - see the utils/ directory
// ERRH END
