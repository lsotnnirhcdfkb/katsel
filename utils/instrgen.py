import helpers
# Instruction class {{{1
class Instruction:
    def __init__(self, name, type_, fields, assertions=None):
        self.name = name
        self.type = type_
        self.fields = helpers.Field.process(fields)
        self.assertions = assertions if assertions is not None else []
    def base(self):
        return 'Instruction'
class Br:
    def __init__(self, name, fields, assertions=None):
        self.name = name
        self.fields = helpers.Field.process(fields)
        self.assertions = assertions if assertions is not None else []
    def base(self):
        return 'Br'
# assertion shorthands {{{1
def type_must_be(var_type, must_be):
    return f'dynamic_cast<{must_be} *>(&{var_type})'
def operands_equal(*operands):
    return [f'&{x} == &{y}' for x, y in zip(operands, operands[1:])] # zip ends when one sequence ends
def type_is_integral(v):
    return f'{type_must_be(v, "IntType const")} || {type_must_be(v, "GenericIntType const")}'
def type_is_floating(v):
    return f'{type_must_be(v, "FloatType const")} || {type_must_be(v, "GenericFloatType const")}'
# instructions {{{1
instructions = [
    Instruction('Copy',
        'val.value->type().context.get_void_type()',
        'IR::Register&|target ! Located<NNPtr<Value>>|val',
    [
    ]),

    Instruction('Or',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('lhs.value->type()', 'BoolType const'), *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('And',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('lhs.value->type()', 'BoolType const'), *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('Not',
        'op.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|op',
    [
        type_must_be('op.value->type()', 'BoolType const'),
    ]),
    Instruction('ICmpNE',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpEQ',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpLT',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpGT',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpLE',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpGE',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('IAdd',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('ISub',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('IMult',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('IDiv',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('IMod',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('INeg',
        'op.value->type()',
        'Located<NNPtr<Value>>|op',
    [
        type_is_integral('op.value->type()')
    ]),
    Instruction('FCmpNE',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpEQ',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpLT',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpGT',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpLE',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpGE',
        'lhs.value->type().context.get_bool_type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FAdd',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FSub',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FMult',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FDiv',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FMod',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FNeg',
        'op.value->type()',
        'Located<NNPtr<Value>>|op',
    [
        type_is_floating('op.value->type()')
    ]),
    Instruction('BitXor',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('BitOr',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('BitAnd',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('BitNot',
        'op.value->type()',
        'Located<NNPtr<Value>>|op',
    [
        type_is_integral('op.value->type()')
    ]),
    Instruction('ShiftR',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        type_is_integral('lhs.value->type()')
    ]),
    Instruction('ShiftL',
        'lhs.value->type()',
        'Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_is_integral('lhs.value->type()'),
        type_is_integral('lhs.value->type()')
    ]),
    Instruction('NoOpCast',
        '*newt',
        'Located<NNPtr<Value>>|op ! NNPtr<Type const>|newt',
    [
    ]),
    Instruction('IntToInt',
        '*newt',
        'Located<NNPtr<Value>>|op ! NNPtr<IntType const>|newt',
    [
        type_is_integral('op.value->type()')
    ]),
    Instruction('IntToFloat',
        '*newt',
        'Located<NNPtr<Value>>|op ! NNPtr<FloatType const>|newt',
    [
        type_is_integral('op.value->type()')
    ]),
    Instruction('FloatToFloat',
        '*newt',
        'Located<NNPtr<Value>>|op ! NNPtr<FloatType const>|newt',
    [
        type_is_floating('op.value->type()')
    ]),
    Instruction('FloatToInt',
        '*newt',
        'Located<NNPtr<Value>>|op ! NNPtr<IntType const>|newt',
    [
        type_is_floating('op.value->type()')
    ]),
    Instruction('Call',
        '*f->ty->ret',
        'NNPtr<Function const>|f ! std::vector<Located<NNPtr<Value>>>|args',
    [
        'args.size() == f->ty->paramtys.size()'
    ]),
    Instruction('Addrof',
        'reg.type().context.get_pointer_type(mut, reg.type())',
        'IR::Register&|reg ! bool|mut',
    [
        'static_cast<int>(mut) <= static_cast<int>(reg.mut)'
    ]),
    Instruction('DerefPtr',
        '*static_cast<PointerType const *>(&ptr.value->type())->ty',
        'Located<NNPtr<Value>>|ptr',
    [
        type_must_be('ptr.value->type()', 'PointerType const')
    ]),
    Instruction('PtrArith',
        'ptr.value->type()',
        'Located<NNPtr<Value>>|ptr ! Located<NNPtr<Value>>|offset',
    [
        type_must_be('ptr.value->type()', 'PointerType const'),
        type_is_integral('offset.value->type()')
    ]),

    Br('Return',
        'Located<NNPtr<Value>>|value',
    [
    ]),
    Br('GotoBr',
        'NNPtr<Block>|to',
    [
    ]),
    Br('CondBr',
        'Located<NNPtr<Value>>|v ! NNPtr<Block>|true_b ! NNPtr<Block>|false_b',
    [
        type_must_be('v.value->type()', 'BoolType const')
    ]),
]

# generating stuff {{{1
def gen_decls():
    output = []

    for instruction in instructions:
        output.append( f'class {instruction.name} : public {instruction.base()} {{\n')

        output.append(    ( 'public:\n'
                           f'    {instruction.name}({helpers.Field.as_params(instruction.fields)});\n'
                           f'    void instr_accept({instruction.base()}Visitor &v) const override;\n'))

        if isinstance(instruction, Instruction):
            output.append( f'    IR::Type const &type() const override;\n')

        output.append(       helpers.Field.as_fields(instruction.fields, indent=4))
        output.append(       '};\n')

    return ''.join(output)
def gen_fwd():
    output = []

    for instruction in instructions:
        output.append( f'class {instruction.name};\n')

    return ''.join(output)
def gen_defs():
    output = []

    for instruction in instructions:
        output.append(        f'IR::Instrs::{instruction.name}::{instruction.name}({helpers.Field.as_params(instruction.fields)}): {helpers.Field.as_init_list(instruction.fields)} {{\n')
        for assertion in instruction.assertions:
            output.append(    f'    ASSERT({assertion})\n')
        output.append(         '}\n')
        output.append(        f'void IR::Instrs::{instruction.name}::instr_accept({instruction.base()}Visitor &v) const {{ v.instr_visit(*this); }}\n')

        if isinstance(instruction, Instruction):
            output.append(    f'IR::Type const &IR::Instrs::{instruction.name}::type() const {{ return {instruction.type}; }}\n')

        output.append(         '\n')

    return ''.join(output)

def gen_method_decls(base):
    output = []
    for instr in instructions:
        if instr.base() == base:
            output.append(f'void instr_visit(IR::Instrs::{instr.name} const &i) override;\n')

    return ''.join(output)

def gen_pure_method_decls(base):
    output = []
    for instr in instructions:
        if instr.base() == base:
            output.append(f'virtual void instr_visit(IR::Instrs::{instr.name} const &i) = 0;\n')

    return ''.join(output)
