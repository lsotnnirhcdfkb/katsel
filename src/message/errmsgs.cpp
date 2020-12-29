#include "message/errmsgs.h"
#include "errors.h"
#include "utils/format.h"

// ERRCPP START
// The following code was autogenerated - see the utils/ directory
// E0000 - unexpected-char
// | The lexer found an unexpected character that could not begin
// | a token.
void E0000(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0000 - unexpected-char");
    e.underline(Error::Underline(tok, '^')
        .error("unexpected character")
    );
    e.report();
}

// E0001 - unterm-charlit
// | The lexer found an unterminated character literal.
void E0001(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0001 - unterm-charlit");
    e.underline(Error::Underline(tok, '^')
        .error("unterminated character literal")
    );
    e.report();
}

// E0002 - unterm-strlit
// | The lexer found a newline in a string literal, thereby
// | making it unterminated.
void E0002(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0002 - unterm-strlit");
    e.underline(Error::Underline(tok, '^')
        .error("unterminated string literal")
    );
    e.report();
}

// E0003 - invalid-intlit-base
// | The lexer found an integer literal that has an invalid base.
void E0003(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0003 - invalid-intlit-base");
    e.underline(Error::Underline(tok, '^')
        .error("invalid integer literal base")
    );
    e.report();
}

// E0004 - nondecimal-floatlit
// | The lexer found a non-decimal floating point literal.
void E0004(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0004 - nondecimal-floatlit");
    e.underline(Error::Underline(tok, '^')
        .error("invalid integer literal base")
    );
    e.report();
}

// E0005 - invalid-char-floatlit
// | Invalid numeric character for floating point literal
void E0005(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0005 - invalid-char-floatlit");
    e.underline(Error::Underline(tok, '^')
        .error("invalid character in floating point literal")
    );
    e.report();
}

// E0006 - invalid-char-for-base
// | Invalid numberic character in integer literal for base
void E0006(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0006 - invalid-char-for-base");
    e.underline(Error::Underline(tok, '^')
        .error("invalid character in integer literal for base")
    );
    e.report();
}

// E0007 - intlit-no-digits
// | Integer literal with no digits
void E0007(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0007 - intlit-no-digits");
    e.underline(Error::Underline(tok, '^')
        .error("integer literal with no digits")
    );
    e.report();
}

// E0008 - multichar-charlit
// | Character literal with more than one character
void E0008(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0008 - multichar-charlit");
    e.underline(Error::Underline(tok, '^')
        .error("character literal with more than one character")
    );
    e.report();
}

// E0009 - unterm-multiline-comment
// | Unterminated multiline comment
void E0009(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0009 - unterm-multiline-comment");
    e.underline(Error::Underline(tok, '^')
        .error("unterminated multiline comment")
    );
    e.report();
}

// E0010 - dedent-nomatch
// | Dedent level does not match any other indentation level
void E0010(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0010 - dedent-nomatch");
    e.underline(Error::Underline(tok, '^')
        .error("dedent to unknown level")
    );
    e.report();
}

// E0011 - char-after-backslash
// | Non-newline after line continuation backslash
void E0011(Token const &tok)
{
    Error e = Error(Error::MsgType::ERROR, tok, "E0011 - char-after-backslash");
    e.underline(Error::Underline(tok, '^')
        .error("non-newline after line continuation backslash")
    );
    e.report();
}

// E0100 - unrecoverable-invalid-syntax
// | The parser found an unrecoverable syntax error.
void E0100(Token const &lookahead, Token const &lasttok, std::vector<std::string> const &expectations)
{
    Error e = Error(Error::MsgType::ERROR, lookahead, "E0100 - unrecoverable-invalid-syntax");
    e.underline(Error::Underline(lookahead, '^')
        .error(format("unexpected %", lookahead.type))
    );
auto un (Error::Underline(lasttok, '~'));
for (std::string const &expectation : expectations)
  un.hint(expectation);
e.underline(un);
    e.report();
}

// E0101 - simple-invalid-syntax
// | The parser found a syntax error and recovered by inserting,
// | substituting, or removing a single token.
void E0101(Token const &lookahead, Token const &lasttok, std::string const &bestfix, std::vector<std::string> const &expectations)
{
    Error e = Error(Error::MsgType::ERROR, lookahead, "E0101 - simple-invalid-syntax");
    e.underline(Error::Underline(lookahead, '^')
        .error(format("unexpected %", lookahead.type))
        .note(bestfix)
    );
auto un (Error::Underline(lasttok, '~'));
for (std::string const &expectation : expectations)
  un.hint(expectation);
e.underline(un);
    e.report();
}

// E0102 - panicking-invalid-syntax
// | The parser found a syntax error and recovered via panic mode
// | error recovery.
void E0102(Token const &lookahead, Token const &lasttok, Token const &panicuntil, std::vector<std::string> const &expectations)
{
    Error e = Error(Error::MsgType::ERROR, lookahead, "E0102 - panicking-invalid-syntax");
    e.underline(Error::Underline(lookahead, '^')
        .error(format("unexpected %", lookahead.type))
    );
    e.underline(Error::Underline(panicuntil, '-')
        .note(format("parser panicked until %", panicuntil.type))
    );
auto un (Error::Underline(lasttok, '~'));
for (std::string const &expectation : expectations)
  un.hint(expectation);
e.underline(un);
    e.report();
}

// E0200 - redecl-sym
// | Symbol was redeclared
void E0200(Token const &name, IR::Value *val)
{
    Error e = Error(Error::MsgType::ERROR, name, "E0200 - redecl-sym");
    e.underline(Error::Underline(name, '^')
        .error("redeclaration of symbol")
    );
    IR::DeclaredValue * asdeclared;
    if (( asdeclared = dynamic_cast<IR::DeclaredValue*>( val)))
    e.underline(Error::Underline(asdeclared->defAST(), '~')
        .note("previous declaration")
    );
    e.report();
}

// E0201 - lhs-unsupported-op
// | Left hand side of binary expression does not support
// | operator
void E0201(IR::ASTValue const &lhs, Token const &op)
{
    Error e = Error(Error::MsgType::ERROR, op, "E0201 - lhs-unsupported-op");
    e.underline(Error::Underline(lhs, '^')
        .note(format("lhs is of type %", lhs.type()))
    );
    IR::DeclaredValue * asdeclared;
    if (( asdeclared = dynamic_cast<IR::DeclaredValue*>( lhs.val)))
    e.underline(Error::Underline(asdeclared->defAST(), '~')
        .note("lhs declared here")
    );
    e.underline(Error::Underline(op, '^')
        .error("unsupported binary operator for left operand")
    );
    e.report();
}

// E0202 - addrof-not-lvalue
// | Taking an address of a non-lvalue is impossible
void E0202(Token const &op, IR::ASTValue const &val)
{
    Error e = Error(Error::MsgType::ERROR, val, "E0202 - addrof-not-lvalue");
    e.underline(Error::Underline(op, '^')
        .error("taking address of non-lvalue")
    );
    e.underline(Error::Underline(val, '~')
    );
    e.report();
}

// E0203 - unary-unsupported-op
// | Operand of unary expression does not support operator
void E0203(IR::ASTValue const &operand, Token const &_operator)
{
    Error e = Error(Error::MsgType::ERROR, _operator, "E0203 - unary-unsupported-op");
    e.underline(Error::Underline(operand, '^')
        .note(format("operand is of type %", operand.type()))
    );
    IR::DeclaredValue * asdeclared;
    if (( asdeclared = dynamic_cast<IR::DeclaredValue*>( operand.val)))
    e.underline(Error::Underline(asdeclared->defAST(), '~')
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
    Error e = Error(Error::MsgType::ERROR, oparn, "E0204 - call-noncallable");
    e.underline(Error::Underline(func, '^')
        .error("calling of non-callable value")
        .note(format("value of type %", func.type()))
    );
    IR::DeclaredValue * asdeclared;
    if (( asdeclared = dynamic_cast<IR::DeclaredValue*>( func.val)))
    e.underline(Error::Underline(asdeclared->defAST(), '~')
        .note("callee declared here")
    );
    e.report();
}

// E0205 - wrong-num-args
// | Wrong number of arguments to function call
void E0205(IR::ASTValue const &func, Token const &oparn, ASTNS::ArgB *argsast, std::vector<IR::ASTValue> const &args)
{
    Error e = Error(Error::MsgType::ERROR, oparn, "E0205 - wrong-num-args");
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
    Error e = Error(Error::MsgType::ERROR, arg, "E0206 - incorrect-arg");
    e.underline(Error::Underline(arg, '^')
        .error("invalid argument to function call")
        .note(format("argument is of type %", arg.type()))
        .note(format("function expects %", expected))
    );
    e.report();
}

// E0207 - undecl-symb
// | Usage of undeclared symbol
void E0207(Token const &sym)
{
    Error e = Error(Error::MsgType::ERROR, sym, "E0207 - undecl-symb");
    e.underline(Error::Underline(sym, '^')
        .error("undeclared symbol")
    );
    e.report();
}

// E0208 - confl-tys-ifexpr
// | Conflicting types for branches of if expression
void E0208(IR::ASTValue const &truev, IR::ASTValue const &falsev, Token const &iftok)
{
    Error e = Error(Error::MsgType::ERROR, iftok, "E0208 - confl-tys-ifexpr");
    e.underline(Error::Underline(iftok, '^')
        .error("conflicting types for branches of if expression")
    );
    e.underline(Error::Underline(truev, '~')
        .note(format("%", truev.type()))
    );
    e.underline(Error::Underline(falsev, '~')
        .note(format("%", falsev.type()))
    );
    e.report();
}

// E0209 - assign-invalid-lhs
// | Invalid assignment target
void E0209(Token const &eq, IR::ASTValue const &lhs)
{
    Error e = Error(Error::MsgType::ERROR, eq, "E0209 - assign-invalid-lhs");
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
    Error e = Error(Error::MsgType::ERROR, eq, "E0210 - assign-conflict-tys");
    e.underline(Error::Underline(eq, '^')
        .error("conflicting types for assignment")
    );
    e.underline(Error::Underline(lhs, '~')
        .note(format("%", lhs.type()))
    );
    e.underline(Error::Underline(rhs, '~')
        .note(format("%", rhs.type()))
    );
    e.report();
}

// E0212 - conflict-ret-ty
// | Conflicting return types
void E0212(IR::ASTValue const &val, IR::Function *f)
{
    Error e = Error(Error::MsgType::ERROR, val, "E0212 - conflict-ret-ty");
    e.underline(Error::Underline(val, '^')
        .error("conflicting return type")
        .note(format("returning %", val.type()))
    );
    e.underline(Error::Underline(f->defAST()->retty.get(), '~')
        .note(format("function returns %", f->ty->ret))
    );
    e.report();
}

// E0213 - no-deref
// | Cannot dereference non-pointer
void E0213(Token const &op, IR::ASTValue const &val)
{
    Error e = Error(Error::MsgType::ERROR, val, "E0213 - no-deref");
    e.underline(Error::Underline(op, '^')
        .error(format("dereferencing of non-pointer type %", val.type()))
    );
    e.underline(Error::Underline(val, '~')
    );
    e.report();
}

// E0214 - redecl-var
// | Redeclaration of variable
void E0214(Token const &name, IR::Register const *prev)
{
    Error e = Error(Error::MsgType::ERROR, name, "E0214 - redecl-var");
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
    Error e = Error(Error::MsgType::ERROR, eq, "E0215 - conflict-var-init-ty");
    e.underline(Error::Underline(eq, '~')
    );
    e.underline(Error::Underline(init, '^')
        .error("conflicting types for variable initialization")
        .note(format("%", init.type()))
    );
    e.underline(Error::Underline(name, '^')
        .note(format("%", var->type()))
    );
    e.report();
}

// E0216 - invalid-cast
// | Invalid cast
void E0216(ASTNS::AST *ast, IR::ASTValue v, IR::Type const *newty)
{
    Error e = Error(Error::MsgType::ERROR, ast, "E0216 - invalid-cast");
    e.underline(Error::Underline(ast, '^')
        .error(format("invalid cast from % to %", v.type(), newty))
    );
    e.report();
}

// E0217 - conflict-tys-binary-op
// | Conflicting types to binary operator
void E0217(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &op)
{
    Error e = Error(Error::MsgType::ERROR, op, "E0217 - conflict-tys-binary-op");
    e.underline(Error::Underline(lhs, '~')
        .note(format("%", lhs.type()))
    );
    e.underline(Error::Underline(rhs, '~')
        .note(format("%", rhs.type()))
    );
    e.underline(Error::Underline(op, '^')
        .error("conflicting types to binary operator")
    );
    e.report();
}

// E0218 - redecl-param
// | Redeclaraion of parameter in function declaration
void E0218(Token const &name, IR::Register const *prev)
{
    Error e = Error(Error::MsgType::ERROR, name, "E0218 - redecl-param");
    e.underline(Error::Underline(name, '^')
        .error("redeclaration of parameter")
    );
    e.underline(Error::Underline(prev->defAST(), '~')
        .note("previous declaration")
    );
    e.report();
}

// E0219 - cond-not-bool
// | Using a non-bool value as a condition
void E0219(IR::ASTValue &v)
{
    Error e = Error(Error::MsgType::ERROR, v, "E0219 - cond-not-bool");
    e.underline(Error::Underline(v, '^')
        .error(format("usage of % as condition", v.type()))
    );
    IR::DeclaredValue * asdeclared;
    if (( asdeclared = dynamic_cast<IR::DeclaredValue*>( v.val)))
    e.underline(Error::Underline(asdeclared->defAST(), '~')
        .note("declared here")
    );
    e.report();
}

// E0220 - no-else-not-void
// | If expression with non-void true expression and no else case
void E0220(IR::ASTValue const &truev, Token const &iftok)
{
    Error e = Error(Error::MsgType::ERROR, iftok, "E0220 - no-else-not-void");
    e.underline(Error::Underline(iftok, '^')
        .error("if expression with non-void true expression and no else case")
    );
    e.underline(Error::Underline(truev, '~')
        .note(format("%", truev.type()))
    );
    e.report();
}

// W0000 - Wextra-semi
// | Extra semicolon
void W0000(Token const &semi)
{
    Error e = Error(Error::MsgType::WARNING, semi, "W0000 - Wextra-semi");
    e.underline(Error::Underline(semi, '^')
        .warning("unnecessary semicolon")
    );
    e.report();
}

// This code was autogenerated - see the utils/ directory
// ERRCPP END
