#!/usr/bin/env python3

import textwrap
import itertools

# Msg class {{{1
class Msg:
    def __init__(self, name, *, desc, inputs, location, highlights, extra=None):
        self.code = None
        self.category = None
        self.name = name
        self.desc = desc
        self.inputs = inputs
        self.location = location
        self.highlights = highlights
        self.extra = extra
# types of highlights {{{1
class SimpleHighlight:
    def __init__(self, location, under, messages):
        self.location = location
        self.under = under
        self.messages = messages
    def generate(self):
        output = [f'    e.underline(Error::Underline({self.location}, \'{self.under}\')\n']
        for message in self.messages:
            if len(message) == 2:
                output.append(f'        .{message[0]}({message[1]})\n')
            else:
                output.append(f'        .{message[0]}(format({", ".join(message[1:])}))\n')
        output.append('    );\n')
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
            fallbackmessage = self.message
        else:
            message = 'format("% declared here", {self.valuename})'
            fallbackmessage = 'format("% implicitly declared", {self.valuename})'

        output = (f'    if (IR::DeclaredValue *asDeclared = dynamic_cast<IR::DeclaredValue*>({self.val})) {{\n'
                   '        if (!dynamic_cast<ASTNS::ImplicitDecl*>(asDeclared->defAST())) {\n'
                  f'            e.underline(Error::Underline(asDeclared->defAST(), \'{self.under}\')\n'
                  f'                .{self.type}({message}));\n')
        if self.fallbackloc is not None:
            output += ('        } else {\n'
                      f'            e.underline(Error::Underline({self.fallbackloc}, \'{self.under}\')\n'
                      f'                .{self.type}({fallbackmessage}));\n'
                       '        }\n')
        else:
            output += ('       }\n')

        output += '    }\n'
        return output
# constants {{{1
UNDER0 = '^'
UNDER1 = '~'
UNDER2 = '-'

PADAMT = 4
RANGEMULT = 100
# errors to generate {{{1
errors = {
    # syntax errors {{{
    'syntax error': [
        Msg('unexpected-char',
            desc='The lexer found an unexpected character that could not begin a token.',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unexpected character"')]),
            ]),
        Msg('unterm-charlit',
            desc='The lexer found an unterminated character literal.',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unterminated character literal"')]),
            ]),
        Msg('unterm-strlit',
            desc='The lexer found a newline in a string literal, thereby making it unterminated.',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unterminated string literal"')]),
            ]),
        Msg('invalid-intlit-base',
            desc='The lexer found an integer literal that has an invalid base.',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"invalid integer literal base"')]),
            ]),
        Msg('nondecimal-floatlit',
            desc='The lexer found a non-decimal floating point literal.',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"invalid integer literal base"')]),
            ]),
        Msg('invalid-char-floatlit',
            desc='Invalid numeric character for floating point literal',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"invalid character in floating point literal"')]),
            ]),
        Msg('invalid-char-for-base',
            desc='Invalid numberic character in integer literal for base',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"invalid character in integer literal for base"')]),
            ]),
        Msg('intlit-no-digits',
            desc='Integer literal with no digits',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"integer literal with no digits"')]),
            ]),
        Msg('multichar-charlit',
            desc='Character literal with more than one character',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"character literal with more than one character"')]),
            ]),
        Msg('unterm-multiline-comment',
            desc='Unterminated multiline comment',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"unterminated multiline comment"')]),
            ]),
        Msg('dedent-nomatch',
            desc='Dedent level does not match any other indentation level',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"dedent to unknown level"')]),
            ]),
        Msg('char-after-backslash',
            desc='Non-newline after line continuation backslash',
            inputs='Token const &tok', location='tok',
            highlights=[
                SimpleHighlight('tok', UNDER0, [('error', '"non-newline after line continuation backslash"')]),
            ]),
        Msg('unrecoverable-invalid-syntax',
            desc='The parser found an unrecoverable syntax error.',
            inputs='Token const &lookahead, Token const &lasttok, std::vector<std::string> const &expectations', location='lookahead',
            highlights=[
                SimpleHighlight('lookahead', UNDER0, [('error', '"unexpected %"', 'lookahead.type')]),
            ],
            extra=(
                "auto un (Error::Underline(lasttok, '~'));\n"
                'for (std::string const &expectation : expectations)\n'
                '    un.hint(expectation);\n'
                'e.underline(un);\n')),
        Msg('simple-invalid-syntax',
            desc='The parser found a syntax error and recovered by inserting, substituting, or removing a single token.',
            inputs='Token const &lookahead, Token const &lasttok, std::string const &bestfix, std::vector<std::string> const &expectations', location='lookahead',
            highlights=[
                SimpleHighlight('lookahead', UNDER0, [('error', '"unexpected %"', 'lookahead.type'), ('note', 'bestfix')]),
            ],
            extra=(
                "auto un (Error::Underline(lasttok, '~'));\n"
                'for (std::string const &expectation : expectations)\n'
                '    un.hint(expectation);\n'
                'e.underline(un);\n')),
        Msg('panicking-invalid-syntax',
            desc='The parser found a syntax error and recovered via panic mode error recovery.',
            inputs='Token const &lookahead, Token const &lasttok, Token const &panicuntil, std::vector<std::string> const &expectations', location='lookahead',
            highlights=[
                SimpleHighlight('lookahead', UNDER0, [('error', '"unexpected %"', 'lookahead.type')]),
                SimpleHighlight('panicuntil', UNDER2, [('note', '"parser panicked until %"', 'panicuntil.type')]),
            ],
            extra=(
                "auto un (Error::Underline(lasttok, '~'));\n"
                'for (std::string const &expectation : expectations)\n'
                '    un.hint(expectation);\n'
                'e.underline(un);\n')),
    ],
    # }}}
    # type errors {{{
    'type error': [
        Msg('lhs-unsupported-op',
            desc='Left hand side of binary expression does not support operator',
            inputs='IR::ASTValue const &lhs, Token const &op', location='op',
            highlights=[
                SimpleHighlight('lhs', UNDER0, [('note', '"lhs is of type %"', 'lhs.type()')]),
                SimpleHighlight('op', UNDER0, [('error', '"unsupported binary operator for left operand"')]),
            ]),
        Msg('unary-unsupported-op',
            desc='Operand of unary expression does not support operator',
            inputs='IR::ASTValue const &operand, Token const &_operator', location='_operator',
            highlights=[
                SimpleHighlight('operand', UNDER0, [('note', '"operand is of type %"', 'operand.type()')]),
                SimpleHighlight('_operator', UNDER0, [('error', '"unsupported unary operator"')]),
            ]),
        Msg('call-noncallable',
            desc='Non-callable value called',
            inputs='IR::ASTValue const &func, Token const &oparn', location='oparn',
            highlights=[
                SimpleHighlight('func', UNDER0, [('error', '"calling of non-callable value"'), ('note', '"value of type %"', 'func.type()')]),
            ]),
        Msg('incorrect-arg',
            desc='Incorrect argument to function call',
            inputs='IR::ASTValue const &arg, IR::Type const *expected', location='arg',
            highlights=[
                SimpleHighlight('arg', UNDER0, [('error', '"invalid argument to function call"'), ('note', '"argument is of type %"', 'arg.type()'), ('note', '"function expects %"', 'expected')]),
            ]),
        Msg('confl-tys-ifexpr',
            desc='Conflicting types for branches of if expression',
            inputs='IR::ASTValue const &truev, IR::ASTValue const &falsev, Token const &iftok, Token const &elsetok', location='iftok',
            highlights=[
                SimpleHighlight('iftok', UNDER0, [('error', '"conflicting types for branches of if expression"')]),
                SimpleHighlight('elsetok', UNDER2, []),
                SimpleHighlight('truev', UNDER1, [('note', '"%"', 'truev.type()')]),
                SimpleHighlight('falsev', UNDER1, [('note', '"%"', 'falsev.type()')]),
            ]),
        Msg('assign-conflict-tys',
            desc='Assignment target and value do not have same type',
            inputs='IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &eq', location='eq',
            highlights=[
                SimpleHighlight('eq', UNDER0, [('error', '"conflicting types for assignment"')]),
                SimpleHighlight('lhs', UNDER1, [('note', '"%"', 'lhs.type()')]),
                SimpleHighlight('rhs', UNDER1, [('note', '"%"', 'rhs.type()')]),
            ]),
        Msg('conflict-ret-ty',
            desc='Conflicting return types',
            inputs='IR::ASTValue const &val, IR::Function *f', location='val',
            highlights=[
                SimpleHighlight('val', UNDER0, [('error', '"conflicting return type"'), ('note', '"returning %"', 'val.type()')]),
                SimpleHighlight('f->defAST()->retty.get()', UNDER1, [('note', '"function returns %"', 'f->ty->ret')]),
            ]),
        Msg('no-deref',
            desc='Cannot dereference non-pointer',
            inputs='Token const &op, IR::ASTValue const &val', location='val',
            highlights=[
                SimpleHighlight('op', UNDER0, [('error', '"dereferencing of non-pointer type %"', 'val.type()')]),
                SimpleHighlight('val', UNDER1, []),
            ]),
        Msg('conflict-var-init-ty',
            desc='Conflicting type for variable initialization',
            inputs='Token const &eq, Token const &name, ASTNS::Type *typeAST, IR::ASTValue const &init, IR::Type const *expectedType', location='eq',
            highlights=[
                SimpleHighlight('eq', UNDER1, []),
                SimpleHighlight('name', UNDER1, []),
                SimpleHighlight('init', UNDER0, [('error', '"conflicting types for variable initialization"'), ('note', '"%"', 'init.type()')]),
                SimpleHighlight('typeAST', UNDER1, [('note', '"%"', 'expectedType')]),
            ]),
        Msg('invalid-cast',
            desc='Invalid cast',
            inputs='ASTNS::AST *ast, IR::ASTValue v, IR::Type const *newty', location='ast',
            highlights=[
                SimpleHighlight('ast', UNDER0, [('error', '"invalid cast from % to %"', 'v.type()', 'newty')]),
            ]),
        Msg('conflict-tys-binary-op',
            desc='Conflicting types to binary operator',
            inputs='IR::ASTValue const &lhs, IR::ASTValue const &rhs, Token const &op', location='op',
            highlights=[
                SimpleHighlight('lhs', UNDER1, [('note', '"%"', 'lhs.type()')]),
                SimpleHighlight('rhs', UNDER1, [('note', '"%"', 'rhs.type()')]),
                SimpleHighlight('op', UNDER0, [('error', '"conflicting types to binary operator"')]),
            ]),
        Msg('cond-not-bool',
            desc='Using a non-bool value as a condition',
            inputs='IR::ASTValue &v', location='v',
            highlights=[
                SimpleHighlight('v', UNDER0, [('error', '"usage of % as condition"', 'v.type()')]),
            ]),
        Msg('ptr-arith-rhs-not-num',
            desc='Cannot do pointer arithmetic with non-integer as right-hand-side of expression',
            inputs='IR::ASTValue const &lhs, Token const &optok, IR::ASTValue const &rhs', location='optok',
            highlights=[
                SimpleHighlight('lhs', UNDER1, []),
                SimpleHighlight('rhs', UNDER1, [('note', '"%"', 'rhs.type()')]),
                SimpleHighlight('optok', UNDER0, [('error', '"pointer arithmetic requires an integral right-hand operand"')]),
            ]),
        Msg('no-else-not-void',
            desc='If expression with non-void true expression and no else case',
            inputs='IR::ASTValue const &truev, Token const &iftok', location='iftok',
            highlights=[
                SimpleHighlight('iftok', UNDER0, [('error', '"if expression with non-void true expression and no else case"')]),
                SimpleHighlight('truev', UNDER1, [('note', '"%"', 'truev.type()')]),
            ]),
    ],
    # }}}
    # name errors {{{
    'name error': [
        Msg('redecl-sym',
            desc='Symbol was redeclared',
            inputs='Token const &name, IR::Value *val', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"redeclaration of symbol"')]),
                ValueDeclHighlight('val', '', None, UNDER1, 'note', '"previous declaration"'),
            ]),
        Msg('undecl-symb',
            desc='Usage of undeclared symbol',
            inputs='Location const &path', location='path',
            highlights=[
                SimpleHighlight('path', UNDER0, [('error', '"undeclared symbol"')]),
            ]),
        Msg('redecl-param',
            desc='Redeclaraion of parameter in function declaration',
            inputs='Token const &name, IR::Instrs::Register const *prev', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"redeclaration of parameter"')]),
                SimpleHighlight('prev->defAST()', UNDER1, [('note', '"previous declaration"')]),
            ]),
        Msg('redecl-var',
            desc='Redeclaration of variable',
            inputs='Token const &name, IR::Instrs::Register const *prev', location='name',
            highlights=[
                SimpleHighlight('name', UNDER0, [('error', '"redeclaration of variable"')]),
                SimpleHighlight('prev->defAST()', UNDER1, [('note', '"previous declaration"')]),
            ]),
        Msg('not-a-type',
            desc='Expected a type but path resolved to something else',
            inputs='Location const &notty, ASTNS::AST *declAST', location='notty',
            highlights=[
                SimpleHighlight('notty', UNDER0, [('error', '"not a type"')]),
                SimpleHighlight('declAST', UNDER1, [('note', '"declared here"')]),
            ]),
        Msg('no-item-in',
            desc='No item of a certain name within another name',
            inputs='IR::DeclSymbol const *prev, Token const &current', location='current',
            highlights=[
                SimpleHighlight('current', UNDER0, [('error', '"no item called % in %"', 'current', 'prev')]),
            ]),
    ],
    # }}}
    # value errors {{{
    'value error': [
        Msg('addrof-not-lvalue',
            desc='Taking an address of a non-lvalue is impossible',
            inputs='Token const &op, IR::ASTValue const &val', location='val',
            highlights=[
                SimpleHighlight('op', UNDER0, [('error', '"taking address of non-lvalue"')]),
                SimpleHighlight('val', UNDER1, []),
            ]),
        Msg('assign-invalid-lhs',
            desc='Invalid assignment target',
            inputs='Token const &eq, IR::ASTValue const &lhs', location='eq',
            highlights=[
                SimpleHighlight('eq', UNDER0, [('error', '"non-lvalue assignment"')]),
                SimpleHighlight('lhs', UNDER1, []),
            ]),
        Msg('assign-not-mut',
            desc='Cannot assign to non-mutable lvalue',
            inputs='IR::ASTValue const &v, Token const &eq, IR::Instrs::DerefPtr *targetDeref', location='v',
            highlights=[
                SimpleHighlight('eq', UNDER0, [('error', '"cannot assign to immutable lvalue"')]),
                SimpleHighlight('v', UNDER1, []),
                ValueDeclHighlight('targetDeref->ptr.val', 'lvalue', None, UNDER1, 'note', '"variable declared immutable here"'),
            ]),
        Msg('mut-addrof-nonmut-op',
            desc='Cannot take a mutable pointer to non-mutable lvalue',
            inputs='Token const &op, IR::Instrs::DerefPtr *asDeref', location='op',
            highlights=[
                SimpleHighlight('op', UNDER0, [('error', '"cannot take mutable pointer to non-mutable lvalue"')]),
                ValueDeclHighlight('asDeref->ptr.val', 'value', None, UNDER1, 'note', '"value declared immutable here"'),
            ]),
        ],
    # }}}
    # count errors {{{
    'count error': [
        Msg('wrong-num-args',
            desc='Wrong number of arguments to function call',
            inputs='IR::ASTValue const &func, Token const &oparn, std::vector<IR::ASTValue> const &args', location='oparn',
            highlights=[
                SimpleHighlight('oparn', UNDER0, [('error', '"wrong number of arguments to function call"')]),
                SimpleHighlight('func', UNDER1, []),
                SimpleHighlight('static_cast<IR::Function*>(func.val)->defAST()', UNDER1, [('note', '"function expects % arguments, but got % arguments"', 'static_cast<IR::FunctionType*>(func.type())->paramtys.size()', 'args.size()')]),
            ]),
    ],
    # }}}
    # miscellaneous errors {{{
    'miscellaneous error': [
        Msg('no-suppress',
            desc='Cannot suppress an expression that is not the implicit return value of a block',
            inputs='Location const &dot', location='dot',
            highlights=[
                SimpleHighlight('dot', UNDER0, [('error', '"implicit return suppression not allowed here"')]),
            ]),
    ],
    # }}}
}
warnings = {
    'code style': [
        Msg('extra-semi',
            desc='Extra semicolon',
            inputs='Token const &semi', location='semi',
            highlights=[
                SimpleHighlight('semi', UNDER0, [('warning', '"unnecessary semicolon"')]),
            ]),
    ]
}
# filling in error numbers {{{1
def fill_numbers(msgs):
    num = 0
    for category, category_msgs in msgs.items():
        for msg in category_msgs:
            msg.code = num
            msg.category = category
            num += 1
        num //= RANGEMULT
        num += 1
        num *= RANGEMULT

fill_numbers(errors)
fill_numbers(warnings)
# generating {{{1
def gen_h():
    output = []

    output.append(     '\n')

    for cat, errs in errors.items():
        output.append(f'// === {cat} ===\n\n')
        for error in errs:
            code = str(error.code).zfill(PADAMT)
            output.append(f'// E{code} - {error.name}\n')
            output.append(f'#define ERR_{error.name.upper().replace("-", "_")} E{code}\n')
            output.append(f'void E{code}({error.inputs});\n')
            output.append( '\n')

    output.append('// ===> warnings <===\n\n')
    for cat, warns in warnings.items():
        output.append(f'// === {cat} ===\n\n')
        for warning in warns:
            code = str(warning.code).zfill(PADAMT)
            output.append(f'// W{code} - W{warning.name}\n')
            output.append(f'#define WARN_{warning.name.upper().replace("-", "_")} W{code}\n')
            output.append(f'void W{code}({warning.inputs});\n')
            output.append( '\n')

    return ''.join(output)

def gen_cpp():
    def gen_message(code, name, msgtype, location, description, inputs, highlights, extra):
        output.append(        f'// {code} - {name}\n')
        desc_wrapped = ''.join('// | ' + line + '\n' for line in textwrap.wrap(description, 60))
        output.append(        desc_wrapped)
        output.append(        f'void {code}({inputs}) {{\n')
        output.append(        f'    Error e = Error(Error::MsgType::{msgtype}, {location}, "{code} ({name})");\n')

        for hi in highlights:
            output.append(hi.generate())
        if extra is not None:
            output.append(extra)

        output.append(         '    e.report();\n')

        output.append(         '}\n\n')

    output = []

    for error in itertools.chain.from_iterable(errors.values()):
        code = 'E' + str(error.code).zfill(PADAMT)
        name = error.name
        msgtype = 'ERROR'
        location = error.location
        description = error.desc
        inputs = error.inputs
        highlights = error.highlights
        extra = error.extra
        gen_message(code, name, msgtype, location, description, inputs, highlights, extra)

    for warning in itertools.chain.from_iterable(warnings.values()):
        code = 'W' + str(warning.code).zfill(PADAMT)
        name = 'W' + warning.name
        msgtype = 'WARNING'
        location = warning.location
        description = warning.desc
        inputs = warning.inputs
        highlights = warning.highlights
        extra = warning.extra
        gen_message(code, name, msgtype, location, description, inputs, highlights, extra)

    return ''.join(output)
