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
        'IR::Register&|target ! ASTValue|val',
    [
        target_equal('val.type()'),
    ]),

    Instruction('Or',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_must_be('lhs.type()', 'BoolType const'), *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('And',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_must_be('lhs.type()', 'BoolType const'), *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('Not',
        'IR::Register&|target ! ASTValue|op',
    [
        type_must_be('target.type()', 'BoolType const'),
    ]),
    Instruction('ICmpNE',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpEQ',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpLT',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpGT',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpLE',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpGE',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('IAdd',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('ISub',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('IMult',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('IDiv',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('IMod',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('INeg',
        'IR::Register&|target ! ASTValue|op',
    [
        target_equal('op.type()'),
        type_is_integral('op.type()')
    ]),
    Instruction('FCmpNE',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpEQ',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpLT',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpGT',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpLE',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpGE',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('target.type()', 'BoolType const'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FAdd',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FSub',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FMult',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FDiv',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FMod',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FNeg',
        'IR::Register&|target ! ASTValue|op',
    [
        target_equal('op.type()'),
        type_is_floating('op.type()')
    ]),
    Instruction('BitXor',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('BitOr',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('BitAnd',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('BitNot',
        'IR::Register&|target ! ASTValue|op',
    [
        target_equal('op.type()'),
        type_is_integral('op.type()')
    ]),
    Instruction('ShiftR',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        type_is_integral('lhs.type()')
    ]),
    Instruction('ShiftL',
        'IR::Register&|target ! ASTValue|lhs ! ASTValue|rhs',
    [
        target_equal('lhs.type()'),
        type_is_integral('lhs.type()'),
        type_is_integral('lhs.type()')
    ]),
    Instruction('NoOpCast',
        'IR::Register&|target ! ASTValue|op ! NNPtr<Type const>|newt',
    [
        target_equal('*newt'),
    ]),
    Instruction('IntToInt',
        'IR::Register&|target ! ASTValue|op ! NNPtr<IntType const>|newt',
    [
        target_equal('*newt'),
        type_is_integral('op.type()')
    ]),
    Instruction('IntToFloat',
        'IR::Register&|target ! ASTValue|op ! NNPtr<FloatType const>|newt',
    [
        target_equal('*newt'),
        type_is_integral('op.type()')
    ]),
    Instruction('FloatToFloat',
        'IR::Register&|target ! ASTValue|op ! NNPtr<FloatType const>|newt',
    [
        target_equal('*newt'),
        type_is_floating('op.type()')
    ]),
    Instruction('FloatToInt',
        'IR::Register&|target ! ASTValue|op ! NNPtr<IntType const>|newt',
    [
        target_equal('*newt'),
        type_is_floating('op.type()')
    ]),
    Instruction('Call',
        'IR::Register&|target ! NNPtr<Function const>|f ! std::vector<ASTValue>|args',
    [
        target_equal('*f->ty->ret'),
        'args.size() == f->ty->paramtys.size()'
    ]),
    Instruction('Addrof',
        'IR::Register&|target ! NNPtr<DerefPtr const>|deref ! bool|mut',
    [
        'static_cast<int>(mut) <= static_cast<int>(static_cast<PointerType const *>(&deref->ptr.type())->mut)'
    ]),
    Instruction('DerefPtr',
        'IR::Register&|target ! ASTValue|ptr',
    [
        type_must_be('ptr.type()', 'PointerType const')
    ]),
    Instruction('PtrArith',
        'IR::Register&|target ! ASTValue|ptr ! ASTValue|offset',
    [
        type_must_be('ptr.type()', 'PointerType const'),
        type_is_integral('offset.type()')
    ]),

    Br('Return',
        'ASTValue|value',
    [
    ]),
    Br('GotoBr',
        'NNPtr<Block>|to',
    [
    ]),
    Br('CondBr',
        'ASTValue|v ! NNPtr<Block>|true_b ! NNPtr<Block>|false_b',
    [
        type_must_be('v.type()', 'BoolType const')
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
