#!/usr/bin/env python3

import textwrap
import itertools

# Msg class {{{1
class Msg:
    def __init__(self, name, *, desc, inputs, location, highlights):
        self.code = None
        self.name = name
        self.desc = desc
        self.inputs = inputs
        self.location = location
        self.highlights = highlights
# types of highlights {{{1
class SimpleHighlight:
    def __init__(self, location, under, messages):
        self.location = location
        self.under = under
        self.messages = messages
    def generate(self):
        output = []
        for message in self.messages:
            if len(message) == 2:
                output.append(f'    sect->messages.push_back(Errors::Sections::Underlines::Message {{ {self.location}, \'{self.under}\', {message[1]}, A_BOLD }});\n')
            else:
                output.append(f'    sect->messages.push_back(Errors::Sections::Underlines::Message {{ {self.location}, \'{self.under}\', format({", ".join(message[1:])}), A_BOLD }});\n')
        return ''.join(output)
class ValueDeclHighlight:
    def __init__(self, val, valuename, fallbackloc, under, type_, message=None):
        self.val = val
        self.valuename = valuename
        self.fallbackloc = fallbackloc
        self.under = under
        self.type = type_
        self.message = message
    def generate(self):
        if self.message is not None:
            message = self.message
        else:
            message = 'format("{} declared here", {self.valuename})'

        output = (f'    if (IR::DeclaredValue const *as_declared = dynamic_cast<IR::DeclaredValue const *>({self.val}))\n'
                  f'        sect->messages.push_back(Errors::Sections::Underlines::Message {{ as_declared->def_span(), \'{self.under}\', {message}, A_BOLD }});\n')

        return output
# constants {{{1
UNDER0 = '^'
UNDER1 = '~'
UNDER2 = '-'

PADAMT = 4
RANGEMULT = 100
# errors to generate {{{1
errors = [
        Msg('unexpected-char',
            desc='The lexer found an unexpected character that could not begin a token.',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unexpected character"')]),
            ]),
        Msg('unterm-charlit',
            desc='The lexer found an unterminated character literal.',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unterminated character literal"')]),
            ]),
        Msg('unterm-strlit',
            desc='The lexer found a newline in a string literal, thereby making it unterminated.',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unterminated string literal"')]),
            ]),

        Msg('invalid-numlit-base',
            desc='The lexer found an number literal that has an invalid base.',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"invalid number literal base"')]),
            ]),
        Msg('nondecimal-floatlit',
            desc='The lexer found a non-decimal floating point literal.',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"non-decimal floating-point literal"')]),
            ]),
        Msg('invalid-char-for-base',
            desc='Invalid character in number literal for base',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"invalid character in number literal for base"')]),
            ]),
        Msg('intlit-no-digits',
            desc='Number literal with no digits',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"Number literal with no digits"')]),
            ]),
        Msg('multichar-charlit',
            desc='Character literal with more than one character',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"character literal with more than one character"')]),
            ]),
        Msg('unterm-multiline-comment',
            desc='Unterminated multiline comment',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unterminated multiline comment"')]),
            ]),
        Msg('dedent-nomatch',
            desc='Dedent level does not match any other indentation level',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"dedent to unknown level"')]),
            ]),
        Msg('char-after-backslash',
            desc='Non-newline after line continuation backslash',
            inputs='Span const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"non-newline after line continuation backslash"')]),
            ]),
        Msg('expected',
            desc='Expected something',
            inputs='Span const &expected, std::string const &name', location='expected',
            highlights=[
                SimpleHighlight('expected', UNDER0, [('error', '"expected {}"', 'name')]),
            ]),
        Msg('lhs-unsupported-op',
            desc='Left hand side of binary expression does not support operator',
            inputs='Located<NNPtr<IR::Value>> const &lhs, Span const &op', location='op',
            highlights=[
                SimpleHighlight('lhs.span', UNDER0, [('note', '"lhs is of type {}"', 'lhs.value->type()')]),
                SimpleHighlight('op', UNDER0, [('error', '"unsupported binary operator for left operand"')]),
            ]),
        Msg('unary-unsupported-op',
            desc='Operand of unary expression does not support operator',
            inputs='Located<NNPtr<IR::Value>> const &operand, Located<ASTNS::UnaryOperator> const &op', location='op.span',
            highlights=[
                SimpleHighlight('operand.span', UNDER0, [('note', '"operand is of type {}"', 'operand.value->type()')]),
                SimpleHighlight('op.span', UNDER0, [('error', '"unsupported unary operator"')]),
            ]),
        Msg('call-noncallable',
            desc='Non-callable value called',
            inputs='Located<NNPtr<IR::Value>> const &func, Span const &oparn', location='oparn',
            highlights=[
                SimpleHighlight('func.span', UNDER0, [('error', '"calling of non-callable value"'), ('note', '"value of type {}"', 'func.value->type()')]),
            ]),
        Msg('incorrect-arg',
            desc='Incorrect argument to function call',
            inputs='Located<NNPtr<IR::Value>> const &arg, IR::Type const &expected', location='arg.span',
            highlights=[
                SimpleHighlight('arg.span', UNDER0, [('error', '"invalid argument to function call"'), ('note', '"argument is of type {}"', 'arg.value->type()'), ('note', '"function expects {}"', 'expected')]),
            ]),
        Msg('confl-tys-ifexpr',
            desc='Conflicting types for branches of if expression',
            inputs='Located<NNPtr<IR::Value>> const &truev, Located<NNPtr<IR::Value>> const &falsev, Span const &iftok, Span const &elsetok', location='iftok',
            highlights=[
                SimpleHighlight('iftok', UNDER0, [('error', '"conflicting types for branches of if expression"')]),
                SimpleHighlight('elsetok', UNDER2, []),
                SimpleHighlight('truev.span', UNDER1, [('note', '"{}"', 'truev.value->type()')]),
                SimpleHighlight('falsev.span', UNDER1, [('note', '"{}"', 'falsev.value->type()')]),
            ]),
        Msg('assign-conflict-tys',
            desc='Assignment target and value do not have same type',
            inputs='Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &eq', location='eq',
            highlights=[
                SimpleHighlight('eq', UNDER0, [('error', '"conflicting types for assignment"')]),
                SimpleHighlight('lhs.span', UNDER1, [('note', '"{}"', 'lhs.value->type()')]),
                SimpleHighlight('rhs.span', UNDER1, [('note', '"{}"', 'rhs.value->type()')]),
            ]),
        Msg('conflict-ret-ty',
            desc='Conflicting return types',
            inputs='Located<NNPtr<IR::Value>> const &val, IR::Function const &f', location='val.span',
            highlights=[
                SimpleHighlight('val.span', UNDER0, [('error', '"conflicting return type"'), ('note', '"returning {}"', 'val.value->type()')]),
                SimpleHighlight('f.def_span()', UNDER1, [('note', '"function returns {}"', '*f.ty->ret')]),
            ]),
        Msg('no-deref',
            desc='Cannot dereference non-pointer',
            inputs='Span const &op, Located<NNPtr<IR::Value>> const &val', location='val.span',
            highlights=[
                SimpleHighlight('op', UNDER0, [('error', '"dereferencing of non-pointer type {}"', 'val.value->type()')]),
                SimpleHighlight('val.span', UNDER1, []),
            ]),
        Msg('conflict-var-init-ty',
            desc='Conflicting type for variable initialization',
            inputs='Span const &eq, Span const &name, ASTNS::Type const &type_ast, Located<NNPtr<IR::Value>> const &init, IR::Type const &expected_type', location='eq',
            highlights=[
                SimpleHighlight('eq', UNDER1, []),
                SimpleHighlight('name', UNDER1, []),
                SimpleHighlight('init.span', UNDER0, [('error', '"conflicting types for variable initialization"'), ('note', '"{}"', 'init.value->type()')]),
                SimpleHighlight('type_ast', UNDER1, [('note', '"{}"', 'expected_type')]),
            ]),
        Msg('invalid-cast',
            desc='Invalid cast',
            inputs='ASTNS::AST const &ast, Located<NNPtr<IR::Value>> v, IR::Type const &newty', location='ast',
            highlights=[
                SimpleHighlight('ast', UNDER0, [('error', '"invalid cast from {} to {}"', 'v.value->type()', 'newty')]),
            ]),
        Msg('conflict-tys-binary-op',
            desc='Conflicting types to binary operator',
            inputs='Located<NNPtr<IR::Value>> const &lhs, Located<NNPtr<IR::Value>> const &rhs, Span const &op', location='op',
            highlights=[
                SimpleHighlight('lhs.span', UNDER1, [('note', '"{}"', 'lhs.value->type()')]),
                SimpleHighlight('rhs.span', UNDER1, [('note', '"{}"', 'rhs.value->type()')]),
                SimpleHighlight('op', UNDER0, [('error', '"conflicting types to binary operator"')]),
            ]),
        Msg('cond-not-bool',
            desc='Using a non-bool value as a condition',
            inputs='Located<NNPtr<IR::Value>> const &v', location='v.span',
            highlights=[
                SimpleHighlight('v.span', UNDER0, [('error', '"usage of {} as condition"', 'v.value->type()')]),
            ]),
        Msg('ptr-arith-rhs-not-num',
            desc='Cannot do pointer arithmetic with non-integer as right-hand-side of expression',
            inputs='Located<NNPtr<IR::Value>> const &lhs, Located<ASTNS::BinaryOperator> const &optok, Located<NNPtr<IR::Value>> const &rhs', location='optok.span',
            highlights=[
                SimpleHighlight('lhs.span', UNDER1, []),
                SimpleHighlight('rhs.span', UNDER1, [('note', '"{}"', 'rhs.value->type()')]),
                SimpleHighlight('optok.span', UNDER0, [('error', '"pointer arithmetic requires an integral right-hand operand"')]),
            ]),
        Msg('no-else-not-void',
            desc='If expression with non-void true expression and no else case',
            inputs='Located<NNPtr<IR::Value>> const &truev, Span const &iftok', location='iftok',
            highlights=[
                SimpleHighlight('iftok', UNDER0, [('error', '"if expression with non-void true expression and no else case"')]),
                SimpleHighlight('truev.span', UNDER1, [('note', '"{}"', 'truev.value->type()')]),
            ]),
        Msg('typeless-this',
            desc='\'this\' parameter used outside of impl or class block',
            inputs='ASTNS::ThisParam const &p', location='p',
            highlights=[
                SimpleHighlight('p', UNDER0, [('error', '"\'this\' parameter not allowed outside of impl or class block"')]),
            ]),
        Msg('wrong-num-args',
            desc='Wrong number of arguments to function call',
            inputs='IR::Function const &func, ASTNS::AST const &func_ref_ast, Span const &oparn, std::vector<Located<NNPtr<IR::Value>>> const &args', location='oparn',
            highlights=[
                SimpleHighlight('oparn', UNDER0, [('error', '"wrong number of arguments to function call"')]),
                SimpleHighlight('func_ref_ast', UNDER1, []),
                SimpleHighlight('func.def_span()', UNDER1, [('note', '"function expects {} arguments, but got {} arguments"', 'func.ty->paramtys.size()', 'args.size()')]),
            ]),
        Msg('redecl-sym',
            desc='Symbol was redeclared',
            inputs='Span const &name, IR::Value const &val', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"redeclaration of symbol"')]),
                ValueDeclHighlight('&val', '', None, UNDER1, 'note', '"previous declaration"'),
            ]),
        Msg('undecl-symb',
            desc='Usage of undeclared symbol',
            inputs='Span const &path', location='path',
            highlights=[
                SimpleHighlight('path', UNDER0, [('error', '"undeclared symbol"')]),
            ]),
        Msg('redecl-param',
            desc='Redeclaraion of parameter in function declaration',
            inputs='ASTNS::ParamB const &param, IR::Register const &prev', location='param',
            highlights=[
                SimpleHighlight('param', UNDER0, [('error', '"redeclaration of parameter"')]),
                SimpleHighlight('prev.def_span()', UNDER1, [('note', '"previous declaration"')]),
            ]),
        Msg('redecl-var',
            desc='Redeclaration of variable',
            inputs='Span const &name, IR::Register const &prev', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"redeclaration of variable"')]),
                SimpleHighlight('prev.def_span()', UNDER1, [('note', '"previous declaration"')]),
            ]),
        Msg('not-a-type',
            desc='Expected a type but path resolved to something else',
            inputs='Span const &notty, ASTNS::AST const &decl_ast', location='notty',
            highlights=[
                SimpleHighlight('notty', UNDER0, [('error', '"not a type"')]),
                SimpleHighlight('decl_ast', UNDER1, [('note', '"declared here"')]),
            ]),
        Msg('no-member-in',
            desc='No member of a certain name within another member',
            inputs='IR::DeclSymbol const &prev, Span const &current', location='current',
            highlights=[
                SimpleHighlight('current', UNDER0, [('error', '"no member called {} in {}"', 'current.stringify()', 'prev')]),
            ]),
        Msg('no-this',
            desc='Usage of \'this\' outside method',
            inputs='Span const &th', location='th',
            highlights=[
                SimpleHighlight('th', UNDER0, [('error', '"usage of \'this\' outside method"')]),
            ]),
        Msg('no-method',
            desc='Accessing a method that doesn\'t exist',
            inputs='Located<NNPtr<IR::Value>> const &op, Span const &name', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"no method called \'{}\' on value of type {}"', 'name.stringify()', 'op.value->type()')]),
            ]),
        Msg('no-field',
            desc='Accessing a field that doesn\'t exist',
            inputs='Located<NNPtr<IR::Value>> const &op, Span const &name', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"no field called \'{}\' on value of type {}"', 'name.stringify()', 'op.value->type()')]),
            ]),
        Msg('addrof-not-lvalue',
            desc='Taking an address of a non-lvalue is impossible',
            inputs='Span const &op, Located<NNPtr<IR::Value>> const &val', location='val.span',
            highlights=[
                SimpleHighlight('op', UNDER0, [('error', '"taking address of non-lvalue"')]),
                SimpleHighlight('val.span', UNDER1, []),
            ]),
        Msg('assign-invalid-lhs',
            desc='Invalid assignment target',
            inputs='Span const &eq, Located<NNPtr<IR::Value>> const &lhs', location='eq',
            highlights=[
                SimpleHighlight('eq', UNDER0, [('error', '"non-lvalue assignment"')]),
                SimpleHighlight('lhs.span', UNDER1, []),
            ]),
        Msg('assign-not-mut',
            desc='Cannot assign to non-mutable lvalue',
            inputs='Located<NNPtr<IR::Value>> const &v, Span const &eq, IR::Register const &reg', location='v.span',
            highlights=[
                SimpleHighlight('eq', UNDER0, [('error', '"cannot assign to immutable lvalue"')]),
                SimpleHighlight('v.span', UNDER1, []),
                ValueDeclHighlight('&reg', 'lvalue', None, UNDER1, 'note', '"variable declared immutable here"'),
            ]),
        Msg('mut-addrof-nonmut-op',
            desc='Cannot take a mutable pointer to non-mutable lvalue',
            inputs='Span const &op, IR::Register const &reg', location='op',
            highlights=[
                SimpleHighlight('op', UNDER0, [('error', '"cannot take mutable pointer to non-mutable lvalue"')]),
                ValueDeclHighlight('&reg', 'value', None, UNDER1, 'note', '"value declared immutable here"'),
            ]),
        Msg('no-suppress',
            desc='Cannot suppress an expression that is not the implicit return value of a block',
            inputs='Span const &dollar', location='dollar',
            highlights=[
                SimpleHighlight('dollar', UNDER0, [('error', '"implicit return suppression not allowed here"')]),
            ]),
        Msg('this-not-first',
            desc='\'this\' parameter is not the first parameter of a method',
            inputs='ASTNS::ThisParam const &ast', location='ast',
            highlights=[
                SimpleHighlight('ast', UNDER0, [('error', '"\'this\' parameter must be the first parameter of a method"')]),
            ]),
]
warnings = [
    Msg('extra-semi',
        desc='Extra semicolon',
        inputs='Span const &semi', location='semi',
        highlights=[
            SimpleHighlight('semi', UNDER0, [('warning', '"unnecessary semicolon"')]),
        ]),
    Msg('immut-noinit',
        desc='Uninitialized immutable variable',
        inputs='ASTNS::VarStmt const &ast', location='ast',
        highlights=[
            SimpleHighlight('ast', UNDER0, [('warning', '"uninitialized immutable variable will never be initialized"')]),
        ]),
    Msg('block-no-indent',
        desc='Braced block without an indent',
        inputs='Span const &obrace, Span const &cbrace', location='obrace',
        highlights=[
            SimpleHighlight('obrace', UNDER0, [('warning', '"braced block without indent"')]),
            SimpleHighlight('cbrace', UNDER1, [('note', '"closing brace here"')]),
        ]),
]
# fill numbers {{{1
def fill(msgs):
    i = 0
    for msg in msgs:
        msg.code = i
        i += 1
fill(errors)
fill(warnings)
# generating {{{1
def gen_h():
    output = []

    output.append(     '\n')

    for error in errors:
        code = str(error.code).zfill(PADAMT)
        output.append(f'// E{code} - {error.name}\n')
        output.append(f'#define ERR_{error.name.upper().replace("-", "_")} E{code}\n')
        output.append(f'void E{code}({error.inputs});\n')
        output.append( '\n')

    output.append('// ===> warnings <===\n\n')
    for warning in warnings:
        code = str(warning.code).zfill(PADAMT)
        output.append(f'// W{code} - W{warning.name}\n')
        output.append(f'#define WARN_{warning.name.upper().replace("-", "_")} W{code}\n')
        output.append(f'void W{code}({warning.inputs});\n')
        output.append( '\n')

    return ''.join(output)

def gen_cpp():
    def gen_message(code, name, msgtype, location, description, inputs, highlights):
        output.append(        f'// {code} - {name}\n')
        desc_wrapped = ''.join('// | ' + line + '\n' for line in textwrap.wrap(description, 60))
        output.append(        desc_wrapped)
        output.append(        f'void {code}({inputs}) {{\n')
        output.append(        f'    Errors::SimpleError e = Errors::SimpleError(Errors::SimpleError::Type::{msgtype}, {location}, "{code}", "{name}");\n')
        output.append(         '    auto sect = std::make_unique<Errors::Sections::Underlines>();\n')

        for hi in highlights:
            output.append(hi.generate())

        output.append(         '    e.section(std::move(sect));\n')
        output.append(         '    e.report();\n')

        output.append(         '}\n\n')

    output = []

    for error in errors:
        code = 'E' + str(error.code).zfill(PADAMT)
        name = error.name
        msgtype = 'ERROR'
        location = error.location
        description = error.desc
        inputs = error.inputs
        highlights = error.highlights
        gen_message(code, name, msgtype, location, description, inputs, highlights)

    for warning in warnings:
        code = 'W' + str(warning.code).zfill(PADAMT)
        name = 'W' + warning.name
        msgtype = 'WARNING'
        location = warning.location
        description = warning.desc
        inputs = warning.inputs
        highlights = warning.highlights
        gen_message(code, name, msgtype, location, description, inputs, highlights)

    return ''.join(output)
