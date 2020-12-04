#include "message/errmsgs.h"
#include "message/errors.h"
#include "utils/format.h"

// ERRCPP START

// The following code was autogenerated - see the utils/ directory
// E0000 - unexpected-char
// | The lexer found an unexpected character that could not begin
// | a token.
void E0000(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0000 (unexpected-char)");
    e.underline(Error::Underline(tok, '^')
        .error("unexpected character")
    );
    e.report();
}
// E0001 - unterm-charlit
// | The lexer found an unterminated character literal. A common
// | cause of this is character literals that are more than one
// | character long.
void E0001(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0001 (unterm-charlit)");
    e.underline(Error::Underline(tok, '^')
        .error("unterminated character literal")
    );
    e.report();
}
// E0002 - unterm-strlit
// | The lexer found a newline in a string literal, thereby
// | making it unterminated. Newlines that need to appear inside
// | the string literal must be escaped by putting `\n`.
void E0002(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0002 (unterm-strlit)");
    e.underline(Error::Underline(tok, '^')
        .error("unterminated string literal")
    );
    e.report();
}
// E0003 - invalid-intlit-base
// | The lexer found an integer literal that has an invalid base.
void E0003(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0003 (invalid-intlit-base)");
    e.underline(Error::Underline(tok, '^')
        .error("invalid integer literal base")
    );
    e.report();
}
// E0004 - nondecimal-floatlit
// | The lexer found a non-decimal floating point literal.
void E0004(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0004 (nondecimal-floatlit)");
    e.underline(Error::Underline(tok, '^')
        .error("invalid integer literal base")
    );
    e.report();
}
// E0100 - unrecoverable-invalid-syntax
// | The parser found an unrecoverable syntax error, and has
// | conflicting information about what construct was being
// | parsed when the error happened.
void E0100(Token const &last, Token const &lookahead, std::vector<std::string> const &expectations)
{
    Error e = Error(Error::MsgType::ERROR, last, "E0100 (unrecoverable-invalid-syntax)");
    e.underline(Error::Underline(last, '^')
        .error("invalid syntax")
    );
    e.underline(Error::Underline(lookahead, '~')
        .note("unexpected token here")
    );
auto un (Error::Underline(last, '^'));
for (std::string const &expectation : expectations)
  un.hint(expectation);
e.underline(un);
    e.report();
}
// E0101 - simple-invalid-syntax
// | The parser found a syntax error, has conflicting information
// | about what construct was being parsed when the error
// | happened, and recovered by inserting, substituting, or
// | removing a single token.
void E0101(Token const &last, Token const &lookahead, std::string const &bestfix, std::vector<std::string> const &expectations)
{
    Error e = Error(Error::MsgType::ERROR, last, "E0101 (simple-invalid-syntax)");
    e.underline(Error::Underline(last, '^')
        .error("invalid syntax")
    );
    e.underline(Error::Underline(lookahead, '~')
        .note("unexpected token here")
        .note(bestfix)
    );
auto un (Error::Underline(last, '^'));
for (std::string const &expectation : expectations)
  un.hint(expectation);
e.underline(un);
    e.report();
}
// E0102 - panicking-invalid-syntax
// | The parser found a syntax error, has conflicting information
// | about what construct was being parsed when the error
// | happened, and recovered via panic mode error recovery.
void E0102(Token const &last, Token const &lookahead, std::vector<std::string> const &expectations)
{
    Error e = Error(Error::MsgType::ERROR, last, "E0102 (panicking-invalid-syntax)");
    e.underline(Error::Underline(last, '^')
        .error("invalid syntax")
    );
    e.underline(Error::Underline(lookahead, '~')
        .note("unexpected token here")
    );
auto un (Error::Underline(last, '^'));
for (std::string const &expectation : expectations)
  un.hint(expectation);
e.underline(un);
    e.report();
}
// E0200 - redecl-sym
// | Symbol was redeclared
void E0200(Token const &name, IR::Value *val)
{
    Error e = Error(Error::MsgType::ERROR, name, "E0200 (redecl-sym)");
    e.underline(Error::Underline(name, '^')
        .error("redeclaration of symbol")
    );
IR::Function *asf (dynamic_cast<IR::Function*>(val));
if (asf)
  e.underline(Error::Underline(asf->defAST(), '^')
    .note("previous declaration"));
    e.report();
}
// E0201 - lhs-unsupported-op
// | Left hand side of binary expression does not support
// | operator
void E0201(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &op)
{
    Error e = Error(Error::MsgType::ERROR, op, "E0201 (lhs-unsupported-op)");
    e.underline(Error::Underline(lhs, '^')
        .note(format("lhs is of type \"%\"", lhs.type()->stringify()))
    );
    IR::DeclaredValue * asdeclval;
    if (( asdeclval = dynamic_cast<IR::DeclaredValue*>( lhs.val)))
    e.underline(Error::Underline(asdeclval->defAST(), '~')
        .note("lhs declared here")
    );
    e.underline(Error::Underline(op, '^')
        .error("unsupported binary operator for left operand")
    );
    e.underline(Error::Underline(rhs, '~')
    );
    e.report();
}
// E0203 - unary-unsupported-op
// | Operand of unary expression does not support operator
void E0203(IR::ASTValue const &operand, Token const &_operator)
{
    Error e = Error(Error::MsgType::ERROR, _operator, "E0203 (unary-unsupported-op)");
    e.underline(Error::Underline(operand, '^')
        .note(format("operand is of type \"%\"", operand.type()->stringify()))
    );
    IR::DeclaredValue * asdeclval;
    if (( asdeclval = dynamic_cast<IR::DeclaredValue*>( operand.val)))
    e.underline(Error::Underline(asdeclval->defAST(), '~')
        .note("operand declared here")
    );
    e.underline(Error::Underline(_operator, '^')
        .error("unsupported unary operator")
    );
    e.report();
}
// E0204 - call-noncallable
// | Non-callable value called
void E0204(IR::ASTValue const &func, Token const &oparn)
{
    Error e = Error(Error::MsgType::ERROR, oparn, "E0204 (call-noncallable)");
    e.underline(Error::Underline(func, '^')
        .error("calling of non-callable value")
        .note(format("value of type \"%\"", func.type()->stringify()))
    );
    IR::DeclaredValue * fdecl;
    if (( fdecl = dynamic_cast<IR::DeclaredValue*>( func.val)))
    e.underline(Error::Underline(fdecl->defAST(), '~')
        .note("callee declared here")
    );
    e.report();
}
// E0205 - wrong-num-args
// | Wrong number of arguments to function call
void E0205(IR::ASTValue const &func, Token const &oparn, ASTNS::ArgB *argsast, std::vector<IR::ASTValue> const &args)
{
    Error e = Error(Error::MsgType::ERROR, oparn, "E0205 (wrong-num-args)");
    e.underline(Error::Underline(argsast ? Location(argsast) : Location(oparn), '^')
        .error("wrong number of arguments to function call")
    );
    e.underline(Error::Underline(func, '~')
    );
    e.underline(Error::Underline(static_cast<IR::Function*>(func.val)->defAST(), '~')
        .note(format("function expects % arguments, but got % arguments", static_cast<IR::FunctionType*>(func.type())->paramtys.size(), args.size()))
    );
    e.report();
}
// E0206 - incorrect-arg
// | Incorrect argument to function call
void E0206(IR::ASTValue const &arg, IR::Type const *expected)
{
    Error e = Error(Error::MsgType::ERROR, arg, "E0206 (incorrect-arg)");
    e.underline(Error::Underline(arg, '^')
        .error("invalid argument to function call")
        .note(format("argument is of type \"%\"", arg.type()->stringify()))
        .note(format("function expects \"%\"", expected->stringify()))
    );
    e.report();
}
// E0207 - undecl-symb
// | undeclared symbol
void E0207(Token const &sym)
{
    Error e = Error(Error::MsgType::ERROR, sym, "E0207 (undecl-symb)");
    e.underline(Error::Underline(sym, '^')
        .error("undeclared symbol")
    );
    e.report();
}
// E0208 - confl-tys-ternexpr
// | Conflicting types for ternary expression
void E0208(IR::ASTValue const &truev, IR::ASTValue const &falsev, Token const &quest)
{
    Error e = Error(Error::MsgType::ERROR, quest, "E0208 (confl-tys-ternexpr)");
    e.underline(Error::Underline(quest, '^')
        .error("conflicting types for ternary expression")
    );
    e.underline(Error::Underline(truev, '~')
        .note(truev.type()->stringify())
    );
    e.underline(Error::Underline(falsev, '~')
        .note(falsev.type()->stringify())
    );
    e.report();
}
// E0209 - assign-invalid-lhs
// | Invalid assignment target
void E0209(Token const &eq, IR::ASTValue const &lhs)
{
    Error e = Error(Error::MsgType::ERROR, eq, "E0209 (assign-invalid-lhs)");
    e.underline(Error::Underline(eq, '^')
        .error("non-lvalue assignment")
    );
    e.underline(Error::Underline(lhs, '~')
    );
    e.report();
}
// E0210 - assign-conflict-tys
// | Assignment target and value do not have same type
void E0210(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &eq)
{
    Error e = Error(Error::MsgType::ERROR, eq, "E0210 (assign-conflict-tys)");
    e.underline(Error::Underline(eq, '^')
        .error("conflicting types for assignment")
    );
    e.underline(Error::Underline(lhs, '~')
        .note(lhs.type()->stringify())
    );
    e.underline(Error::Underline(rhs, '~')
        .note(rhs.type()->stringify())
    );
    e.report();
}
// E0211 - ret-val-void-fun
// | Return statement in void function
void E0211(IR::ASTValue const &val, IR::Function *f)
{
    Error e = Error(Error::MsgType::ERROR, val, "E0211 (ret-val-void-fun)");
    e.underline(Error::Underline(val, '^')
        .error("non-void return in void function")
    );
    e.underline(Error::Underline(f->defAST()->retty.get(), '~')
        .note("function returns void")
    );
    e.report();
}
// E0212 - conflict-ret-ty
// | Conflicting return types
void E0212(IR::ASTValue const &val, IR::Function *f)
{
    Error e = Error(Error::MsgType::ERROR, val, "E0212 (conflict-ret-ty)");
    e.underline(Error::Underline(val, '^')
        .error("conflicting return type")
    );
    e.underline(Error::Underline(f->defAST()->retty.get(), '~')
        .note(format("function returns \"%\"", f->ty->ret->stringify()))
    );
    e.report();
}
// E0213 - ret-void-nonvoid-fun
// | Void return in non-void function
void E0213(ASTNS::AST *retstmt, IR::Function *f)
{
    Error e = Error(Error::MsgType::ERROR, retstmt, "E0213 (ret-void-nonvoid-fun)");
    e.underline(Error::Underline(retstmt, '^')
        .error("void return in non-void function")
    );
    e.underline(Error::Underline(f->defAST()->retty.get(), '~')
        .note(format("function returns \"%\"", f->ty->ret->stringify()))
    );
    e.report();
}
// E0214 - redecl-var
// | Redeclaration of variable
void E0214(Token const &name, IR::Register const *prev)
{
    Error e = Error(Error::MsgType::ERROR, name, "E0214 (redecl-var)");
    e.underline(Error::Underline(name, '^')
        .error("redeclaration of variable")
    );
    e.underline(Error::Underline(prev->defAST(), '~')
        .note("previous declaration")
    );
    e.report();
}
// E0215 - conflict-var-init-ty
// | Conflicting type for variable initialization
void E0215(Token const &eq, Token const &name, IR::ASTValue const &init, IR::Register const *var)
{
    Error e = Error(Error::MsgType::ERROR, eq, "E0215 (conflict-var-init-ty)");
    e.underline(Error::Underline(eq, '~')
    );
    e.underline(Error::Underline(init, '^')
        .error("conflicting types for variable initialization")
        .note(init.type()->stringify())
    );
    e.underline(Error::Underline(name, '^')
        .note(var->type()->stringify())
    );
    e.report();
}
// E0216 - invalid-cast
// | Invalid cast
void E0216(ASTNS::AST *ast, IR::ASTValue v, IR::Type const *newty)
{
    Error e = Error(Error::MsgType::ERROR, ast, "E0216 (invalid-cast)");
    e.underline(Error::Underline(ast, '^')
        .error(format("invalid cast from \"%\" to \"%\"", v.type()->stringify(), newty->stringify()))
    );
    e.report();
}
// E0217 - conflict-tys-binary-op
// | Conflicting types to binary operator
void E0217(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &op)
{
    Error e = Error(Error::MsgType::ERROR, op, "E0217 (conflict-tys-binary-op)");
    e.underline(Error::Underline(lhs, '~')
        .note(lhs.type()->stringify())
    );
    e.underline(Error::Underline(rhs, '~')
        .note(rhs.type()->stringify())
    );
    e.underline(Error::Underline(op, '^')
        .error("conflicting types to binary operator")
    );
    e.report();
}
// This code was autogenerated - see the utils/ directory

// ERRCPP END
