import helpers
# Instruction class {{{1
class Instruction:
    def __init__(self, name, fields, assertions=None, declared=False):
        self.name = name
        self.fields = helpers.Field.process(fields)
        self.assertions = assertions if assertions is not None else []

    def base(self):
        return 'Instruction'
class Br(Instruction):
    def __init__(self, name, fields, assertions=None):
        super().__init__(name, fields, assertions)
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
def target_equal(ty):
    return f'&target.type() == &{ty}'
# instructions {{{1
instructions = [
    Instruction('Copy',
        'IR::Register&|target ! Located<NNPtr<Value>>|val',
    [
        target_equal('val.value->type()'),
    ]),

    Instruction('Or',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_must_be('lhs.value->type()', 'BoolType const'), *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('And',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_must_be('lhs.value->type()', 'BoolType const'), *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('Not',
        'IR::Register&|target ! Located<NNPtr<Value>>|op',
    [
        type_must_be('target.type()', 'BoolType const'),
    ]),
    Instruction('ICmpNE',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpEQ',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpLT',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpGT',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpLE',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('ICmpGE',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.value->type()', 'rhs.value->type()')
    ]),
    Instruction('IAdd',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('ISub',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('IMult',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('IDiv',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('IMod',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('INeg',
        'IR::Register&|target ! Located<NNPtr<Value>>|op',
    [
        target_equal('op.value->type()'),
        type_is_integral('op.value->type()')
    ]),
    Instruction('FCmpNE',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpEQ',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpLT',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpGT',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpLE',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FCmpGE',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FAdd',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FSub',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FMult',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FDiv',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FMod',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_floating('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('FNeg',
        'IR::Register&|target ! Located<NNPtr<Value>>|op',
    [
        target_equal('op.value->type()'),
        type_is_floating('op.value->type()')
    ]),
    Instruction('BitXor',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('BitOr',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('BitAnd',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        *operands_equal('lhs.value->type()',
        'rhs.value->type()')
    ]),
    Instruction('BitNot',
        'IR::Register&|target ! Located<NNPtr<Value>>|op',
    [
        target_equal('op.value->type()'),
        type_is_integral('op.value->type()')
    ]),
    Instruction('ShiftR',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        type_is_integral('lhs.value->type()')
    ]),
    Instruction('ShiftL',
        'IR::Register&|target ! Located<NNPtr<Value>>|lhs ! Located<NNPtr<Value>>|rhs',
    [
        target_equal('lhs.value->type()'),
        type_is_integral('lhs.value->type()'),
        type_is_integral('lhs.value->type()')
    ]),
    Instruction('NoOpCast',
        'IR::Register&|target ! Located<NNPtr<Value>>|op ! NNPtr<Type const>|newt',
    [
        target_equal('*newt'),
    ]),
    Instruction('IntToInt',
        'IR::Register&|target ! Located<NNPtr<Value>>|op ! NNPtr<IntType const>|newt',
    [
        target_equal('*newt'),
        type_is_integral('op.value->type()')
    ]),
    Instruction('IntToFloat',
        'IR::Register&|target ! Located<NNPtr<Value>>|op ! NNPtr<FloatType const>|newt',
    [
        target_equal('*newt'),
        type_is_integral('op.value->type()')
    ]),
    Instruction('FloatToFloat',
        'IR::Register&|target ! Located<NNPtr<Value>>|op ! NNPtr<FloatType const>|newt',
    [
        target_equal('*newt'),
        type_is_floating('op.value->type()')
    ]),
    Instruction('FloatToInt',
        'IR::Register&|target ! Located<NNPtr<Value>>|op ! NNPtr<IntType const>|newt',
    [
        target_equal('*newt'),
        type_is_floating('op.value->type()')
    ]),
    Instruction('Call',
        'IR::Register&|target ! NNPtr<Function const>|f ! std::vector<Located<NNPtr<Value>>>|args',
    [
        target_equal('*f->ty->ret'),
        'args.size() == f->ty->paramtys.size()'
    ]),
    Instruction('Addrof',
        'IR::Register&|target ! IR::Register&|reg ! bool|mut',
    [
        'static_cast<int>(mut) <= static_cast<int>(reg.mut)'
    ]),
    Instruction('DerefPtr',
        'IR::Register&|target ! Located<NNPtr<Value>>|ptr',
    [
        type_must_be('ptr.value->type()', 'PointerType const')
    ]),
    Instruction('PtrArith',
        'IR::Register&|target ! Located<NNPtr<Value>>|ptr ! Located<NNPtr<Value>>|offset',
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
                           f'    void accept({instruction.base()}Visitor &v) const override;\n'))

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
        output.append(        f'void IR::Instrs::{instruction.name}::accept({instruction.base()}Visitor &v) const {{ v.visit(*this); }}\n')

        output.append(         '\n')

    return ''.join(output)

def gen_method_decls(base):
    output = []
    for instr in instructions:
        if instr.base() == base:
            output.append(f'void visit(IR::Instrs::{instr.name} const &i) override;\n')

    return ''.join(output)

def gen_pure_method_decls(base):
    output = []
    for instr in instructions:
        if instr.base() == base:
            output.append(f'virtual void visit(IR::Instrs::{instr.name} const &i) = 0;\n')

    return ''.join(output)
