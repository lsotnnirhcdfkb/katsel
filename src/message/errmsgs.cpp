#include "ir/instruction.h"
#include "ir/module.h"
#include "utils/format.h"
#include "message/errmsgs.h"
#include "errors.h"
#include "ast/ast.h"
#include "ir/function.h"

// ERRCPP START
// E0000 - unexpected-char
// | The lexer found an unexpected character that could not begin
// | a token.
void E0000(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0000", "unexpected-char");
    e.underline(Underline(tok, '^')
        .error("unexpected character")
    );
    e.report();
}

// E0001 - unterm-charlit
// | The lexer found an unterminated character literal.
void E0001(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0001", "unterm-charlit");
    e.underline(Underline(tok, '^')
        .error("unterminated character literal")
    );
    e.report();
}

// E0002 - unterm-strlit
// | The lexer found a newline in a string literal, thereby
// | making it unterminated.
void E0002(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0002", "unterm-strlit");
    e.underline(Underline(tok, '^')
        .error("unterminated string literal")
    );
    e.report();
}

// E0003 - invalid-numlit-base
// | The lexer found an number literal that has an invalid base.
void E0003(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0003", "invalid-numlit-base");
    e.underline(Underline(tok, '^')
        .error("invalid number literal base")
    );
    e.report();
}

// E0004 - nondecimal-floatlit
// | The lexer found a non-decimal floating point literal.
void E0004(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0004", "nondecimal-floatlit");
    e.underline(Underline(tok, '^')
        .error("non-decimal floating-point literal")
    );
    e.report();
}

// E0005 - invalid-char-for-base
// | Invalid character in number literal for base
void E0005(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0005", "invalid-char-for-base");
    e.underline(Underline(tok, '^')
        .error("invalid character in number literal for base")
    );
    e.report();
}

// E0006 - intlit-no-digits
// | Number literal with no digits
void E0006(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0006", "intlit-no-digits");
    e.underline(Underline(tok, '^')
        .error("Number literal with no digits")
    );
    e.report();
}

// E0007 - multichar-charlit
// | Character literal with more than one character
void E0007(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0007", "multichar-charlit");
    e.underline(Underline(tok, '^')
        .error("character literal with more than one character")
    );
    e.report();
}

// E0008 - unterm-multiline-comment
// | Unterminated multiline comment
void E0008(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0008", "unterm-multiline-comment");
    e.underline(Underline(tok, '^')
        .error("unterminated multiline comment")
    );
    e.report();
}

// E0009 - dedent-nomatch
// | Dedent level does not match any other indentation level
void E0009(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0009", "dedent-nomatch");
    e.underline(Underline(tok, '^')
        .error("dedent to unknown level")
    );
    e.report();
}

// E0010 - char-after-backslash
// | Non-newline after line continuation backslash
void E0010(Span const &tok) {
    Error e = Error(MsgType::ERROR, tok, "E0010", "char-after-backslash");
    e.underline(Underline(tok, '^')
        .error("non-newline after line continuation backslash")
    );
    e.report();
}

// E0011 - unrecoverable-invalid-syntax
// | The parser found an unrecoverable syntax error.
void E0011(Span const &lookahead, std::string lookahead_type_name, Span const &lasttok, std::vector<std::string> const &expectations) {
    Error e = Error(MsgType::ERROR, lookahead, "E0011", "unrecoverable-invalid-syntax");
    e.underline(Underline(lookahead, '^')
        .error(format("unexpected {}", lookahead_type_name))
    );
auto un (Underline(lasttok, '~'));
for (std::string const &expectation : expectations)
    un.hint(expectation);
e.underline(un);
    e.report();
}

// E0012 - simple-invalid-syntax
// | The parser found a syntax error and recovered by inserting,
// | substituting, or removing a single token.
void E0012(Span const &lookahead, std::string lookahead_type_name, Span const &lasttok, std::string const &bestfix, std::vector<std::string> const &expectations) {
    Error e = Error(MsgType::ERROR, lookahead, "E0012", "simple-invalid-syntax");
    e.underline(Underline(lookahead, '^')
        .error(format("unexpected {}", lookahead_type_name))
        .note(bestfix)
    );
auto un (Underline(lasttok, '~'));
for (std::string const &expectation : expectations)
    un.hint(expectation);
e.underline(un);
    e.report();
}

// E0013 - panicking-invalid-syntax
// | The parser found a syntax error and recovered via panic mode
// | error recovery.
void E0013(Span const &lookahead, std::string lookahead_type_name, Span const &lasttok, Span const &panicuntil, std::string const &panicuntil_type_name, std::vector<std::string> const &expectations) {
    Error e = Error(MsgType::ERROR, lookahead, "E0013", "panicking-invalid-syntax");
    e.underline(Underline(lookahead, '^')
        .error(format("unexpected {}", lookahead_type_name))
    );
    e.underline(Underline(panicuntil, '-')
        .note(format("parser panicked until {}", panicuntil_type_name))
    );
auto un (Underline(lasttok, '~'));
for (std::string const &expectation : expectations)
    un.hint(expectation);
e.underline(un);
    e.report();
}

// E0014 - lhs-unsupported-op
// | Left hand side of binary expression does not support
// | operator
void E0014(IR::ASTValue const &lhs, Located<ASTNS::BinaryOperator> const &op) {
    Error e = Error(MsgType::ERROR, op.span, "E0014", "lhs-unsupported-op");
    e.underline(Underline(lhs, '^')
        .note(format("lhs is of type {}", lhs.type()))
    );
    e.underline(Underline(op.span, '^')
        .error("unsupported binary operator for left operand")
    );
    e.report();
}

// E0015 - unary-unsupported-op
// | Operand of unary expression does not support operator
void E0015(IR::ASTValue const &operand, Located<ASTNS::UnaryOperator> const &op) {
    Error e = Error(MsgType::ERROR, op.span, "E0015", "unary-unsupported-op");
    e.underline(Underline(operand, '^')
        .note(format("operand is of type {}", operand.type()))
    );
    e.underline(Underline(op.span, '^')
        .error("unsupported unary operator")
    );
    e.report();
}

// E0016 - call-noncallable
// | Non-callable value called
void E0016(IR::ASTValue const &func, Span const &oparn) {
    Error e = Error(MsgType::ERROR, oparn, "E0016", "call-noncallable");
    e.underline(Underline(func, '^')
        .error("calling of non-callable value")
        .note(format("value of type {}", func.type()))
    );
    e.report();
}

// E0017 - incorrect-arg
// | Incorrect argument to function call
void E0017(IR::ASTValue const &arg, IR::Type const &expected) {
    Error e = Error(MsgType::ERROR, arg, "E0017", "incorrect-arg");
    e.underline(Underline(arg, '^')
        .error("invalid argument to function call")
        .note(format("argument is of type {}", arg.type()))
        .note(format("function expects {}", expected))
    );
    e.report();
}

// E0018 - confl-tys-ifexpr
// | Conflicting types for branches of if expression
void E0018(IR::ASTValue const &truev, IR::ASTValue const &falsev, Span const &iftok, Span const &elsetok) {
    Error e = Error(MsgType::ERROR, iftok, "E0018", "confl-tys-ifexpr");
    e.underline(Underline(iftok, '^')
        .error("conflicting types for branches of if expression")
    );
    e.underline(Underline(elsetok, '-')
    );
    e.underline(Underline(truev, '~')
        .note(format("{}", truev.type()))
    );
    e.underline(Underline(falsev, '~')
        .note(format("{}", falsev.type()))
    );
    e.report();
}

// E0019 - assign-conflict-tys
// | Assignment target and value do not have same type
void E0019(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Span const &eq) {
    Error e = Error(MsgType::ERROR, eq, "E0019", "assign-conflict-tys");
    e.underline(Underline(eq, '^')
        .error("conflicting types for assignment")
    );
    e.underline(Underline(lhs, '~')
        .note(format("{}", lhs.type()))
    );
    e.underline(Underline(rhs, '~')
        .note(format("{}", rhs.type()))
    );
    e.report();
}

// E0020 - conflict-ret-ty
// | Conflicting return types
void E0020(IR::ASTValue const &val, IR::Function const &f) {
    Error e = Error(MsgType::ERROR, val, "E0020", "conflict-ret-ty");
    e.underline(Underline(val, '^')
        .error("conflicting return type")
        .note(format("returning {}", val.type()))
    );
    e.underline(Underline(*f._def_ast->retty, '~')
        .note(format("function returns {}", *f.ty->ret))
    );
    e.report();
}

// E0021 - no-deref
// | Cannot dereference non-pointer
void E0021(Span const &op, IR::ASTValue const &val) {
    Error e = Error(MsgType::ERROR, val, "E0021", "no-deref");
    e.underline(Underline(op, '^')
        .error(format("dereferencing of non-pointer type {}", val.type()))
    );
    e.underline(Underline(val, '~')
    );
    e.report();
}

// E0022 - conflict-var-init-ty
// | Conflicting type for variable initialization
void E0022(Span const &eq, Span const &name, ASTNS::Type const &type_ast, IR::ASTValue const &init, IR::Type const &expected_type) {
    Error e = Error(MsgType::ERROR, eq, "E0022", "conflict-var-init-ty");
    e.underline(Underline(eq, '~')
    );
    e.underline(Underline(name, '~')
    );
    e.underline(Underline(init, '^')
        .error("conflicting types for variable initialization")
        .note(format("{}", init.type()))
    );
    e.underline(Underline(type_ast, '~')
        .note(format("{}", expected_type))
    );
    e.report();
}

// E0023 - invalid-cast
// | Invalid cast
void E0023(ASTNS::AST const &ast, IR::ASTValue v, IR::Type const &newty) {
    Error e = Error(MsgType::ERROR, ast, "E0023", "invalid-cast");
    e.underline(Underline(ast, '^')
        .error(format("invalid cast from {} to {}", v.type(), newty))
    );
    e.report();
}

// E0024 - conflict-tys-binary-op
// | Conflicting types to binary operator
void E0024(IR::ASTValue const &lhs, IR::ASTValue const &rhs, Located<ASTNS::BinaryOperator> const &op) {
    Error e = Error(MsgType::ERROR, op.span, "E0024", "conflict-tys-binary-op");
    e.underline(Underline(lhs, '~')
        .note(format("{}", lhs.type()))
    );
    e.underline(Underline(rhs, '~')
        .note(format("{}", rhs.type()))
    );
    e.underline(Underline(op.span, '^')
        .error("conflicting types to binary operator")
    );
    e.report();
}

// E0025 - cond-not-bool
// | Using a non-bool value as a condition
void E0025(IR::ASTValue const &v) {
    Error e = Error(MsgType::ERROR, v, "E0025", "cond-not-bool");
    e.underline(Underline(v, '^')
        .error(format("usage of {} as condition", v.type()))
    );
    e.report();
}

// E0026 - ptr-arith-rhs-not-num
// | Cannot do pointer arithmetic with non-integer as right-hand-
// | side of expression
void E0026(IR::ASTValue const &lhs, Located<ASTNS::BinaryOperator> const &optok, IR::ASTValue const &rhs) {
    Error e = Error(MsgType::ERROR, optok.span, "E0026", "ptr-arith-rhs-not-num");
    e.underline(Underline(lhs, '~')
    );
    e.underline(Underline(rhs, '~')
        .note(format("{}", rhs.type()))
    );
    e.underline(Underline(optok.span, '^')
        .error("pointer arithmetic requires an integral right-hand operand")
    );
    e.report();
}

// E0027 - no-else-not-void
// | If expression with non-void true expression and no else case
void E0027(IR::ASTValue const &truev, Span const &iftok) {
    Error e = Error(MsgType::ERROR, iftok, "E0027", "no-else-not-void");
    e.underline(Underline(iftok, '^')
        .error("if expression with non-void true expression and no else case")
    );
    e.underline(Underline(truev, '~')
        .note(format("{}", truev.type()))
    );
    e.report();
}

// E0028 - typeless-this
// | 'this' parameter used outside of impl or class block
void E0028(ASTNS::ThisParam const &p) {
    Error e = Error(MsgType::ERROR, p, "E0028", "typeless-this");
    e.underline(Underline(p, '^')
        .error("'this' parameter not allowed outside of impl or class block")
    );
    e.report();
}

// E0029 - wrong-num-args
// | Wrong number of arguments to function call
void E0029(IR::Function const &func, ASTNS::AST const &func_ref_ast, Span const &oparn, std::vector<IR::ASTValue> const &args) {
    Error e = Error(MsgType::ERROR, oparn, "E0029", "wrong-num-args");
    e.underline(Underline(oparn, '^')
        .error("wrong number of arguments to function call")
    );
    e.underline(Underline(func_ref_ast, '~')
    );
    e.underline(Underline(func.def_ast(), '~')
        .note(format("function expects {} arguments, but got {} arguments", func.ty->paramtys.size(), args.size()))
    );
    e.report();
}

// E0030 - redecl-sym
// | Symbol was redeclared
void E0030(Span const &name, IR::Value const &val) {
    Error e = Error(MsgType::ERROR, name, "E0030", "redecl-sym");
    e.underline(Underline(name, '^')
        .error("redeclaration of symbol")
    );
    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(&val)) {
        if (!dynamic_cast<ASTNS::ImplicitDecl const *>(&as_declared->def_ast())) {
            e.underline(Underline(as_declared->def_ast(), '~')
                .note("previous declaration"));
       }
    }
    e.report();
}

// E0031 - undecl-symb
// | Usage of undeclared symbol
void E0031(Span const &path) {
    Error e = Error(MsgType::ERROR, path, "E0031", "undecl-symb");
    e.underline(Underline(path, '^')
        .error("undeclared symbol")
    );
    e.report();
}

// E0032 - redecl-param
// | Redeclaraion of parameter in function declaration
void E0032(ASTNS::ParamB const &param, IR::Register const &prev) {
    Error e = Error(MsgType::ERROR, param, "E0032", "redecl-param");
    e.underline(Underline(param, '^')
        .error("redeclaration of parameter")
    );
    e.underline(Underline(prev.def_ast(), '~')
        .note("previous declaration")
    );
    e.report();
}

// E0033 - redecl-var
// | Redeclaration of variable
void E0033(Span const &name, IR::Register const &prev) {
    Error e = Error(MsgType::ERROR, name, "E0033", "redecl-var");
    e.underline(Underline(name, '^')
        .error("redeclaration of variable")
    );
    e.underline(Underline(prev.def_ast(), '~')
        .note("previous declaration")
    );
    e.report();
}

// E0034 - not-a-type
// | Expected a type but path resolved to something else
void E0034(Span const &notty, ASTNS::AST const &decl_ast) {
    Error e = Error(MsgType::ERROR, notty, "E0034", "not-a-type");
    e.underline(Underline(notty, '^')
        .error("not a type")
    );
    e.underline(Underline(decl_ast, '~')
        .note("declared here")
    );
    e.report();
}

// E0035 - no-member-in
// | No member of a certain name within another member
void E0035(IR::DeclSymbol const &prev, Span const &current) {
    Error e = Error(MsgType::ERROR, current, "E0035", "no-member-in");
    e.underline(Underline(current, '^')
        .error(format("no member called {} in {}", current.stringify(), prev))
    );
    e.report();
}

// E0036 - no-this
// | Usage of 'this' outside method
void E0036(Span const &th) {
    Error e = Error(MsgType::ERROR, th, "E0036", "no-this");
    e.underline(Underline(th, '^')
        .error("usage of 'this' outside method")
    );
    e.report();
}

// E0037 - no-method
// | Accessing a method that doesn't exist
void E0037(IR::ASTValue const &op, Span const &name) {
    Error e = Error(MsgType::ERROR, name, "E0037", "no-method");
    e.underline(Underline(name, '^')
        .error(format("no method called '{}' on value of type {}", name.stringify(), op.type()))
    );
    e.report();
}

// E0038 - no-field
// | Accessing a field that doesn't exist
void E0038(IR::ASTValue const &op, Span const &name) {
    Error e = Error(MsgType::ERROR, name, "E0038", "no-field");
    e.underline(Underline(name, '^')
        .error(format("no field called '{}' on value of type {}", name.stringify(), op.type()))
    );
    e.report();
}

// E0039 - addrof-not-lvalue
// | Taking an address of a non-lvalue is impossible
void E0039(Span const &op, IR::ASTValue const &val) {
    Error e = Error(MsgType::ERROR, val, "E0039", "addrof-not-lvalue");
    e.underline(Underline(op, '^')
        .error("taking address of non-lvalue")
    );
    e.underline(Underline(val, '~')
    );
    e.report();
}

// E0040 - assign-invalid-lhs
// | Invalid assignment target
void E0040(Span const &eq, IR::ASTValue const &lhs) {
    Error e = Error(MsgType::ERROR, eq, "E0040", "assign-invalid-lhs");
    e.underline(Underline(eq, '^')
        .error("non-lvalue assignment")
    );
    e.underline(Underline(lhs, '~')
    );
    e.report();
}

// E0041 - assign-not-mut
// | Cannot assign to non-mutable lvalue
void E0041(IR::ASTValue const &v, Span const &eq, IR::Instrs::DerefPtr const &target_deref) {
    Error e = Error(MsgType::ERROR, v, "E0041", "assign-not-mut");
    e.underline(Underline(eq, '^')
        .error("cannot assign to immutable lvalue")
    );
    e.underline(Underline(v, '~')
    );
    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(target_deref.ptr.val.as_raw())) {
        if (!dynamic_cast<ASTNS::ImplicitDecl const *>(&as_declared->def_ast())) {
            e.underline(Underline(as_declared->def_ast(), '~')
                .note("variable declared immutable here"));
       }
    }
    e.report();
}

// E0042 - mut-addrof-nonmut-op
// | Cannot take a mutable pointer to non-mutable lvalue
void E0042(Span const &op, IR::Instrs::DerefPtr const &as_deref) {
    Error e = Error(MsgType::ERROR, op, "E0042", "mut-addrof-nonmut-op");
    e.underline(Underline(op, '^')
        .error("cannot take mutable pointer to non-mutable lvalue")
    );
    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(as_deref.ptr.val.as_raw())) {
        if (!dynamic_cast<ASTNS::ImplicitDecl const *>(&as_declared->def_ast())) {
            e.underline(Underline(as_declared->def_ast(), '~')
                .note("value declared immutable here"));
       }
    }
    e.report();
}

// E0043 - no-suppress
// | Cannot suppress an expression that is not the implicit
// | return value of a block
void E0043(Span const &dollar) {
    Error e = Error(MsgType::ERROR, dollar, "E0043", "no-suppress");
    e.underline(Underline(dollar, '^')
        .error("implicit return suppression not allowed here")
    );
    e.report();
}

// E0044 - this-not-first
// | 'this' parameter is not the first parameter of a method
void E0044(ASTNS::ThisParam const &ast) {
    Error e = Error(MsgType::ERROR, ast, "E0044", "this-not-first");
    e.underline(Underline(ast, '^')
        .error("'this' parameter must be the first parameter of a method")
    );
    e.report();
}

// W0000 - Wextra-semi
// | Extra semicolon
void W0000(Span const &semi) {
    Error e = Error(MsgType::WARNING, semi, "W0000", "Wextra-semi");
    e.underline(Underline(semi, '^')
        .warning("unnecessary semicolon")
    );
    e.report();
}

// W0001 - Wimmut-noinit
// | Uninitialized immutable variable
void W0001(ASTNS::VarStmtItem const &ast) {
    Error e = Error(MsgType::WARNING, ast, "W0001", "Wimmut-noinit");
    e.underline(Underline(ast, '^')
        .warning("uninitialized immutable variable will never be initialized")
    );
    e.report();
}

// W0002 - Wblock-no-indent
// | Braced block without an indent
void W0002(Span const &obrace, Span const &cbrace) {
    Error e = Error(MsgType::WARNING, obrace, "W0002", "Wblock-no-indent");
    e.underline(Underline(obrace, '^')
        .warning("braced block without indent")
    );
    e.underline(Underline(cbrace, '~')
        .note("closing brace here")
    );
    e.report();
}
// ERRCPP END
