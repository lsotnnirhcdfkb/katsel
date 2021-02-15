#include "message/errors.h"
#include "message/ansistuff.h"
#include "sections.h"
#include "ir/instruction.h"
#include "ir/module.h"
#include "utils/format.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/function.h"
#include "ir/type.h"

// ERRCPP START
// E0000 - unexpected-char
// | The lexer found an unexpected character that could not begin
// | a token.
void E0000(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0000", "unexpected-char");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "unexpected character", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0001 - unterm-charlit
// | The lexer found an unterminated character literal.
void E0001(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0001", "unterm-charlit");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "unterminated character literal", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0002 - unterm-strlit
// | The lexer found a newline in a string literal, thereby
// | making it unterminated.
void E0002(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0002", "unterm-strlit");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "unterminated string literal", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0003 - invalid-numlit-base
// | The lexer found an number literal that has an invalid base.
void E0003(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0003", "invalid-numlit-base");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "invalid number literal base", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0004 - nondecimal-floatlit
// | The lexer found a non-decimal floating point literal.
void E0004(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0004", "nondecimal-floatlit");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "non-decimal floating-point literal", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0005 - invalid-char-for-base
// | Invalid character in number literal for base
void E0005(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0005", "invalid-char-for-base");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "invalid character in number literal for base", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0006 - intlit-no-digits
// | Number literal with no digits
void E0006(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0006", "intlit-no-digits");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "Number literal with no digits", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0007 - multichar-charlit
// | Character literal with more than one character
void E0007(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0007", "multichar-charlit");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "character literal with more than one character", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0008 - unterm-multiline-comment
// | Unterminated multiline comment
void E0008(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0008", "unterm-multiline-comment");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "unterminated multiline comment", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0009 - dedent-nomatch
// | Dedent level does not match any other indentation level
void E0009(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0009", "dedent-nomatch");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "dedent to unknown level", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0010 - char-after-backslash
// | Non-newline after line continuation backslash
void E0010(Span const &tok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, tok, "E0010", "char-after-backslash");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { tok, '^', "non-newline after line continuation backslash", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0011 - unrecoverable-invalid-syntax
// | The parser found a syntax error and could not recover.
void E0011(Span const &next, std::string lookahead_type_name, Span const &lasttok, std::initializer_list<std::string> const &expectations) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, next, "E0011", "unrecoverable-invalid-syntax");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { next, '^', format("unexpected {}", lookahead_type_name), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0012 - simple-invalid-syntax
// | The parser found a syntax error and recovered by inserting a
// | single token.
void E0012(Span const &next, std::string lookahead_type_name, Span const &lasttok, std::initializer_list<std::string> const &expectations, std::string const &inserted_type) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, next, "E0012", "simple-invalid-syntax");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { next, '^', format("unexpected {}", lookahead_type_name), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { next, '^', format("parser recovered by inserting {} before this {}", inserted_type, lookahead_type_name), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0013 - skipping-invalid-syntax
// | The parser found a syntax error and recovered by replacing a
// | sequence of tokens with a single token.
void E0013(Span const &next, std::string lookahead_type_name, Span const &lasttok, std::initializer_list<std::string> const &expectations, Span const &replaced, std::string const &replacement_type) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, next, "E0013", "skipping-invalid-syntax");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { next, '^', format("unexpected {}", lookahead_type_name), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { replaced, '~', format("parser recovered by replacing this with {}", replacement_type), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0014 - lhs-unsupported-op
// | Left hand side of binary expression does not support
// | operator
void E0014(Located<NNPtr<IR::Value>> const &lhs, Span const &op) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, op, "E0014", "lhs-unsupported-op");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { lhs.span, '^', format("lhs is of type {}", lhs.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { op, '^', "unsupported binary operator for left operand", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0015 - unary-unsupported-op
// | Operand of unary expression does not support operator
void E0015(Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, op.span, "E0015", "unary-unsupported-op");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { operand.span, '^', format("operand is of type {}", operand.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { op.span, '^', "unsupported unary operator", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0016 - call-noncallable
// | Non-callable value called
void E0016(Located<NNPtr<IR::Value>> const &func, Span const &oparn) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, oparn, "E0016", "call-noncallable");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { func.span, '^', "calling of non-callable value", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { func.span, '^', format("value of type {}", func.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0017 - incorrect-arg
// | Incorrect argument to function call
void E0017(Located<NNPtr<IR::Value>> const &arg, IR::Type const &expected) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, arg.span, "E0017", "incorrect-arg");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { arg.span, '^', "invalid argument to function call", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { arg.span, '^', format("argument is of type {}", arg.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { arg.span, '^', format("function expects {}", expected), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0018 - confl-tys-ifexpr
// | Conflicting types for branches of if expression
void E0018(Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, iftok, "E0018", "confl-tys-ifexpr");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { iftok, '^', "conflicting types for branches of if expression", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { truev.span, '~', format("{}", truev.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { falsev.span, '~', format("{}", falsev.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0019 - assign-conflict-tys
// | Assignment target and value do not have same type
void E0019(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, eq, "E0019", "assign-conflict-tys");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { eq, '^', "conflicting types for assignment", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { lhs.span, '~', format("{}", lhs.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { rhs.span, '~', format("{}", rhs.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0020 - conflict-ret-ty
// | Conflicting return types
void E0020(Located<NNPtr<IR::Value>> const &val, IR::Function const &f) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, val.span, "E0020", "conflict-ret-ty");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { val.span, '^', "conflicting return type", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { val.span, '^', format("returning {}", val.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { f.def_span(), '~', format("function returns {}", *f.ty->ret), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0021 - no-deref
// | Cannot dereference non-pointer
void E0021(Span const &op, Located<NNPtr<IR::Value>> const &val) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, val.span, "E0021", "no-deref");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { op, '^', format("dereferencing of non-pointer type {}", val.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0022 - conflict-var-init-ty
// | Conflicting type for variable initialization
void E0022(Span const &eq, Span const &name, ASTNS::Type const &type_ast, Located<NNPtr<IR::Value>> const &init, IR::Type const &expected_type) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, eq, "E0022", "conflict-var-init-ty");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { init.span, '^', "conflicting types for variable initialization", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { init.span, '^', format("{}", init.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { type_ast, '~', format("{}", expected_type), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0023 - invalid-cast
// | Invalid cast
void E0023(ASTNS::AST const &ast, Located<NNPtr<IR::Value>> v, IR::Type const &newty) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, ast, "E0023", "invalid-cast");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { ast, '^', format("invalid cast from {} to {}", v.value->type(), newty), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0024 - conflict-tys-binary-op
// | Conflicting types to binary operator
void E0024(Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &op) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, op, "E0024", "conflict-tys-binary-op");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { lhs.span, '~', format("{}", lhs.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { rhs.span, '~', format("{}", rhs.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { op, '^', "conflicting types to binary operator", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0025 - cond-not-bool
// | Using a non-bool value as a condition
void E0025(Located<NNPtr<IR::Value>> const &v) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, v.span, "E0025", "cond-not-bool");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { v.span, '^', format("usage of {} as condition", v.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0026 - ptr-arith-rhs-not-num
// | Cannot do pointer arithmetic with non-integer as right-hand-
// | side of expression
void E0026(Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, optok.span, "E0026", "ptr-arith-rhs-not-num");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { rhs.span, '~', format("{}", rhs.value->type()), A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { optok.span, '^', "pointer arithmetic requires an integral right-hand operand", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0027 - no-else-not-void
// | If expression with non-void true expression and no else case
void E0027(Located<NNPtr<IR::Value>> const &truev, Span const &iftok) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, iftok, "E0027", "no-else-not-void");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { iftok, '^', "if expression with non-void true expression and no else case", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { truev.span, '~', format("{}", truev.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0028 - typeless-this
// | 'this' parameter used outside of impl or class block
void E0028(ASTNS::ThisParam const &p) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, p, "E0028", "typeless-this");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { p, '^', "'this' parameter not allowed outside of impl or class block", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0029 - wrong-num-args
// | Wrong number of arguments to function call
void E0029(IR::Function const &func, ASTNS::AST const &func_ref_ast, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, oparn, "E0029", "wrong-num-args");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { oparn, '^', "wrong number of arguments to function call", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { func.def_span(), '~', format("function expects {} arguments, but got {} arguments", func.ty->paramtys.size(), args.size()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0030 - redecl-sym
// | Symbol was redeclared
void E0030(Span const &name, IR::Value const &val) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, name, "E0030", "redecl-sym");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { name, '^', "redeclaration of symbol", A_BOLD });
    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(&val))
        sect->messages.push_back(Errors::Sections::Underlines::Message { as_declared->def_span(), '~', "previous declaration", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0031 - undecl-symb
// | Usage of undeclared symbol
void E0031(Span const &path) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, path, "E0031", "undecl-symb");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { path, '^', "undeclared symbol", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0032 - redecl-param
// | Redeclaraion of parameter in function declaration
void E0032(ASTNS::ParamB const &param, IR::Register const &prev) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, param, "E0032", "redecl-param");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { param, '^', "redeclaration of parameter", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { prev.def_span(), '~', "previous declaration", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0033 - redecl-var
// | Redeclaration of variable
void E0033(Span const &name, IR::Register const &prev) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, name, "E0033", "redecl-var");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { name, '^', "redeclaration of variable", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { prev.def_span(), '~', "previous declaration", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0034 - not-a-type
// | Expected a type but path resolved to something else
void E0034(Span const &notty, ASTNS::AST const &decl_ast) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, notty, "E0034", "not-a-type");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { notty, '^', "not a type", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { decl_ast, '~', "declared here", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0035 - no-member-in
// | No member of a certain name within another member
void E0035(IR::DeclSymbol const &prev, Span const &current) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, current, "E0035", "no-member-in");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { current, '^', format("no member called {} in {}", current.stringify(), prev), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0036 - no-this
// | Usage of 'this' outside method
void E0036(Span const &th) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, th, "E0036", "no-this");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { th, '^', "usage of 'this' outside method", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0037 - no-method
// | Accessing a method that doesn't exist
void E0037(Located<NNPtr<IR::Value>> const &op, Span const &name) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, name, "E0037", "no-method");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { name, '^', format("no method called '{}' on value of type {}", name.stringify(), op.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0038 - no-field
// | Accessing a field that doesn't exist
void E0038(Located<NNPtr<IR::Value>> const &op, Span const &name) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, name, "E0038", "no-field");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { name, '^', format("no field called '{}' on value of type {}", name.stringify(), op.value->type()), A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0039 - addrof-not-lvalue
// | Taking an address of a non-lvalue is impossible
void E0039(Span const &op, Located<NNPtr<IR::Value>> const &val) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, val.span, "E0039", "addrof-not-lvalue");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { op, '^', "taking address of non-lvalue", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0040 - assign-invalid-lhs
// | Invalid assignment target
void E0040(Span const &eq, Located<NNPtr<IR::Value>> const &lhs) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, eq, "E0040", "assign-invalid-lhs");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { eq, '^', "non-lvalue assignment", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0041 - assign-not-mut
// | Cannot assign to non-mutable lvalue
void E0041(Located<NNPtr<IR::Value>> const &v, Span const &eq, IR::Register const &reg) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, v.span, "E0041", "assign-not-mut");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { eq, '^', "cannot assign to immutable lvalue", A_BOLD });
    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(&reg))
        sect->messages.push_back(Errors::Sections::Underlines::Message { as_declared->def_span(), '~', "variable declared immutable here", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0042 - mut-addrof-nonmut-op
// | Cannot take a mutable pointer to non-mutable lvalue
void E0042(Span const &op, IR::Register const &reg) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, op, "E0042", "mut-addrof-nonmut-op");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { op, '^', "cannot take mutable pointer to non-mutable lvalue", A_BOLD });
    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>(&reg))
        sect->messages.push_back(Errors::Sections::Underlines::Message { as_declared->def_span(), '~', "value declared immutable here", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0043 - no-suppress
// | Cannot suppress an expression that is not the implicit
// | return value of a block
void E0043(Span const &dollar) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, dollar, "E0043", "no-suppress");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { dollar, '^', "implicit return suppression not allowed here", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// E0044 - this-not-first
// | 'this' parameter is not the first parameter of a method
void E0044(ASTNS::ThisParam const &ast) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::ERROR, ast, "E0044", "this-not-first");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { ast, '^', "'this' parameter must be the first parameter of a method", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// W0000 - Wextra-semi
// | Extra semicolon
void W0000(Span const &semi) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::WARNING, semi, "W0000", "Wextra-semi");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { semi, '^', "unnecessary semicolon", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// W0001 - Wimmut-noinit
// | Uninitialized immutable variable
void W0001(ASTNS::VarStmtItem const &ast) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::WARNING, ast, "W0001", "Wimmut-noinit");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { ast, '^', "uninitialized immutable variable will never be initialized", A_BOLD });
    e.section(std::move(sect));
    e.report();
}

// W0002 - Wblock-no-indent
// | Braced block without an indent
void W0002(Span const &obrace, Span const &cbrace) {
    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::WARNING, obrace, "W0002", "Wblock-no-indent");
    auto sect = std::make_unique<Errors::Sections::Underlines>();
    sect->messages.push_back(Errors::Sections::Underlines::Message { obrace, '^', "braced block without indent", A_BOLD });
    sect->messages.push_back(Errors::Sections::Underlines::Message { cbrace, '~', "closing brace here", A_BOLD });
    e.section(std::move(sect));
    e.report();
}
// ERRCPP END
