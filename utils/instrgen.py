# Instruction class {{{1
class Instruction:
    def __init__(self, name, fields, assertions=None):
        self.name = name
        self.fields = list(map(Field, fields))
        self.assertions = assertions if assertions is not None else []
    def base(self):
        return 'Instruction'
class Br(Instruction):
    def __init__(self, name, fields, assertions=None):
        super().__init__(name, fields, assertions)
    def base(self):
        return 'Br'

class Field:
    def __init__(self, fromstr):
        if len(splitted := fromstr.split(' ')) == 2:
            self.type_, self.name = splitted
        else:
            raise Exception('incorrect number of fields to get make field')

    def format(self):
        if self.type_.endswith('*'):
            return f'{self.type_[:-1].rstrip()} *{self.name}'
        else:
            return f'{self.type_} {self.name}'
# assertion shorthands {{{1
def type_must_be(var_type, must_be):
    return f'dynamic_cast<{must_be}*>({var_type})'
def operands_equal(*operands):
    return [f'{x} == {y}' for x, y in zip(operands, operands[1:])] # zip ends when one sequence ends
def target_equal(ty):
    return f'target->type() == {ty}'
def type_is_integral(v):
    return f'{type_must_be(v, "IntType")} || {type_must_be(v, "GenericIntType")}'
def type_is_floating(v):
    return f'{type_must_be(v, "FloatType")} || {type_must_be(v, "GenericFloatType")}'
# instructions {{{1
instructions = [
    Instruction('Store'       , ['Register* target', 'ASTValue value'                                      ], [target_equal('value.type()')]),
    Instruction('Phi'         , ['TempRegister* target', 'std::vector<std::pair<Block*,ASTValue>> prevs'   ], []),

    Instruction('Or'          , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), target_equal('lhs.type()'), type_must_be('lhs.type()', 'BoolType'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('And'         , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), target_equal('lhs.type()'), type_must_be('lhs.type()', 'BoolType'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('Not'         , ['TempRegister* target', 'ASTValue op'                                     ], [type_must_be('target->type()', 'BoolType'), target_equal('op.type()')]),

    Instruction('ICmpNE'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpEQ'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpLT'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpGT'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpLE'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpGE'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IAdd'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , target_equal('lhs.type()'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ISub'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , target_equal('lhs.type()'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IMult'       , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , target_equal('lhs.type()'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IDiv'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , target_equal('lhs.type()'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IMod'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , target_equal('lhs.type()'), type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('INeg'        , ['TempRegister* target', 'ASTValue op'                                     ], [type_is_integral('target->type()') , target_equal('op.type()'), type_is_integral('op.type()')]),

    Instruction('FCmpNE'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType') , type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpEQ'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType') , type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpLT'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType') , type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpGT'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType') , type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpLE'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType') , type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpGE'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_must_be('target->type()', 'BoolType') , type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FAdd'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_floating('target->type()'), target_equal('lhs.type()'), type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FSub'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_floating('target->type()'), target_equal('lhs.type()'), type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FMult'       , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_floating('target->type()'), target_equal('lhs.type()'), type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FDiv'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_floating('target->type()'), target_equal('lhs.type()'), type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FMod'        , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_floating('target->type()'), target_equal('lhs.type()'), type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FNeg'        , ['TempRegister* target', 'ASTValue op'                                     ], [type_is_floating('target->type()'), target_equal('op.type()'), type_is_floating('op.type()')]),

    Instruction('BitXor'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('BitOr'       , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('BitAnd'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()') , type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('BitNot'      , ['TempRegister* target', 'ASTValue op'                                     ], [type_is_integral('target->type()') , type_is_integral('op.type()')]),

    Instruction('ShiftR'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()'), target_equal('lhs.type()')]),
    Instruction('ShiftL'      , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [type_is_integral('target->type()'), target_equal('lhs.type()')]),

    Instruction('NoOpCast'    , ['TempRegister* target', 'ASTValue op', 'Type* newt'                       ], []),

    Instruction('IntToInt'    , ['TempRegister* target', 'ASTValue op', 'IntType* newt'                    ], [type_is_integral('op.type()'), target_equal('newt')]),
    Instruction('IntToFloat'  , ['TempRegister* target', 'ASTValue op', 'FloatType* newt'                  ], [type_is_integral('op.type()'), target_equal('newt')]),
    Instruction('FloatToFloat', ['TempRegister* target', 'ASTValue op', 'FloatType* newt'                  ], [type_is_floating('op.type()'), target_equal('newt')]),
    Instruction('FloatToInt'  , ['TempRegister* target', 'ASTValue op', 'IntType* newt'                    ], [type_is_floating('op.type()'), target_equal('newt')]),

    Instruction('Call'        , ['TempRegister* target', 'Function* f', 'std::vector<ASTValue> args'       ], [target_equal('f->ty->ret'), 'args.size() == f->ty->paramtys.size()']),

    Br('Return'      , ['Register* value'                                            ], []),
    Br('GotoBr'      , ['Block* to'                                                  ], []),
    Br('CondBr'      , ['ASTValue v', 'Block* trueB', 'Block* falseB'                ], [type_must_be('v.type()', 'BoolType')]),
]

# generating stuff {{{1
def as_constructor(fields):
    return ', '.join(map(lambda x: x.format(), fields))
def as_fields(fields):
    return ''.join(map(lambda x: '        ' + x.format() + ';\n', fields))
def as_init_list(fields):
    return ', '.join(map(lambda f: f'{f.name}({f.name})', fields))

def gen_decls():
    output = []

    for instruction in instructions:
        output.append((f'    class {instruction.name} : public {instruction.base()}\n'
                        '    {\n'
                        '    public:\n'
                       f'        {instruction.name}({as_constructor(instruction.fields)});\n'
                       f'        void accept({instruction.base()}Visitor *v) override;\n'
                       f'{as_fields(instruction.fields)}'
                        '    };\n'))

    return ''.join(output)
def gen_defs():
    output = []

    for instruction in instructions:
        output.append(    f'IR::Instrs::{instruction.name}::{instruction.name}({as_constructor(instruction.fields)}): {as_init_list(instruction.fields)}\n')
        output.append(     '{\n')
        for assertion in instruction.assertions:
            output.append(f'    ASSERT({assertion})\n')
        output.append(     '}\n')
        output.append(    f'void IR::Instrs::{instruction.name}::accept({instruction.base()}Visitor *v) {{ v->visit{instruction.name}(this); }}\n')

    return ''.join(output)
def gen_cfg_dotter():
    output = []
    for instr in instructions:
        if isinstance(instr, Br):
            output.append(f'void IR::CFGDotter::visit{instr.name}(IR::Instrs::{instr.name} *i)\n')
            output.append( '{\n')
            for field in instr.fields:
                if field.type_ == 'Block*':
                    output.append(f'    ostream << "        branch" << i << " -> " << "block" << i->{field.name} << " [label=\\"{field.name}\\"]\\n";\n')
            output.append( '}\n')

    return ''.join(output)

def gen_method_decls(base):
    output = []
    for instr in instructions:
        if instr.base() == base:
            output.append(f'void visit{instr.name}(IR::Instrs::{instr.name} *i) override;\n')

    return ''.join(output)

def gen_pure_method_decls(base):
    output = []
    for instr in instructions:
        if instr.base() == base:
            output.append(f'virtual void visit{instr.name}(IR::Instrs::{instr.name} *i) = 0;\n')

    return ''.join(output)
