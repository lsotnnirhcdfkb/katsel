import helpers
# Instruction class {{{1
class Instruction:
    def __init__(self, name, type_, fields, assertions=None, declared=False):
        self.name = name
        self.type = type_
        self.fields = helpers.Field.process(fields)
        self.assertions = assertions if assertions is not None else []
        self.declared = declared

        if self.declared:
            self.fields.insert(0, helpers.Field('NNPtr<ASTNS::AST>', '_def_ast'))

    def base(self):
        return 'Instruction'
class Br(Instruction):
    def __init__(self, name, fields, assertions=None):
        super().__init__(name, False, fields, assertions)
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
    Instruction('Store', 'target.type().context.get_void_type()',
        'ASTValue|target ! ASTValue|value ! bool|init',
    [
        type_must_be('target.type()', 'PointerType const'),
        'static_cast<PointerType const *>(&target.type())->ty.as_raw() == &value.type()',
        'init || static_cast<PointerType const *>(&target.type())->mut',
    ]),
    Instruction('Phi', 'prevs[0].second.type()',
        'std::vector<std::pair<NNPtr<Block const>, ASTValue>>|prevs',
    [
        'prevs.size() > 0',
    ]),
    Instruction('Register', 'ty->context.get_pointer_type(mut, *ty)',
        'NNPtr<Type const>|ty ! bool|mut',
    [
    ], declared=True),
    Instruction('Or', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('lhs.type()', 'BoolType const'), *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('And', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_must_be('lhs.type()', 'BoolType const'), *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('Not', 'op.type().context.get_bool_type()',
        'ASTValue|op',
    [
    ]),
    Instruction('ICmpNE', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpEQ', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpLT', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpGT', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpLE', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('ICmpGE', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        *operands_equal('lhs.type()', 'rhs.type()')
    ]),
    Instruction('IAdd', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('ISub', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('IMult', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('IDiv', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('IMod', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('INeg', 'op.type()',
        'ASTValue|op',
    [
        type_is_integral('op.type()')
    ]),
    Instruction('FCmpNE', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpEQ', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpLT', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpGT', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpLE', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FCmpGE', 'lhs.type().context.get_bool_type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FAdd', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FSub', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FMult', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FDiv', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FMod', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_floating('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('FNeg', 'op.type()',
        'ASTValue|op',
    [
        type_is_floating('op.type()')
    ]),
    Instruction('BitXor', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('BitOr', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('BitAnd', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        *operands_equal('lhs.type()',
        'rhs.type()')
    ]),
    Instruction('BitNot', 'op.type()',
        'ASTValue|op',
    [
        type_is_integral('op.type()')
    ]),
    Instruction('ShiftR', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        type_is_integral('lhs.type()')
    ]),
    Instruction('ShiftL', 'lhs.type()',
        'ASTValue|lhs ! ASTValue|rhs',
    [
        type_is_integral('lhs.type()'),
        type_is_integral('lhs.type()')
    ]),
    Instruction('NoOpCast', '*newt',
        'ASTValue|op ! NNPtr<Type const>|newt',
    [
    ]),
    Instruction('IntToInt', '*newt',
        'ASTValue|op ! NNPtr<IntType const>|newt',
    [
        type_is_integral('op.type()')
    ]),
    Instruction('IntToFloat', '*newt',
        'ASTValue|op ! NNPtr<FloatType const>|newt',
    [
        type_is_integral('op.type()')
    ]),
    Instruction('FloatToFloat', '*newt',
        'ASTValue|op ! NNPtr<FloatType const>|newt',
    [
        type_is_floating('op.type()')
    ]),
    Instruction('FloatToInt', '*newt',
        'ASTValue|op ! NNPtr<IntType const>|newt',
    [
        type_is_floating('op.type()')
    ]),
    Instruction('Call', '*f->ty->ret',
        'NNPtr<Function const>|f ! std::vector<ASTValue>|args',
    [
        'args.size() == f->ty->paramtys.size()'
    ]),
    Instruction('Addrof', 'deref->type().context.get_pointer_type(mut, deref->type())',
        'NNPtr<DerefPtr const>|deref ! bool|mut',
    [
        'static_cast<int>(mut) <= static_cast<int>(static_cast<PointerType const *>(&deref->ptr.type())->mut)'
    ]),
    Instruction('DerefPtr', '*static_cast<PointerType const *>(&ptr.type())->ty',
        'ASTValue|ptr',
    [
        type_must_be('ptr.type()', 'PointerType const')
    ]),
    Instruction('PtrArith', 'ptr.type()',
        'ASTValue|ptr ! ASTValue|offset',
    [
        type_must_be('ptr.type()',
        'PointerType const'),
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
        if instruction.declared:
            output.append( f'class {instruction.name} : public {instruction.base()}, public DeclaredValue {{\n')
        else:
            output.append( f'class {instruction.name} : public {instruction.base()} {{\n')

        output.append(    ( 'public:\n'
                           f'    {instruction.name}({helpers.Field.as_params(instruction.fields)});\n'
                           f'    void accept({instruction.base()}Visitor &v) const override;\n'))
        if instruction.base() == 'Instruction':
            output.append( f'    IR::Type const &type() const override;\n')

        if instruction.declared:
            output.append(  '    ASTNS::AST& def_ast() const override;\n')

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

        if instruction.base() == 'Instruction':
            output.append(    f'IR::Type const &IR::Instrs::{instruction.name}::type() const {{ return {instruction.type}; }}\n')

            if instruction.declared:
                output.append(f'ASTNS::AST& IR::Instrs::{instruction.name}::def_ast() const {{ return *_def_ast; }}\n')

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
