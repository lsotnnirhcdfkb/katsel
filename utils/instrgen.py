# Instruction class {{{1
class Instruction:
    def __init__(self, name, type_, fields, assertions=None, declared=False):
        self.name = name
        self.type = type_
        self.fields = list(map(Field, fields))
        self.assertions = assertions if assertions is not None else []
        self.declared = declared
    def base(self):
        return 'Instruction'
class Br(Instruction):
    def __init__(self, name, fields, assertions=None):
        super().__init__(name, False, fields, assertions)
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
def type_is_integral(v):
    return f'{type_must_be(v, "IntType")} || {type_must_be(v, "GenericIntType")}'
def type_is_floating(v):
    return f'{type_must_be(v, "FloatType")} || {type_must_be(v, "GenericFloatType")}'
# instructions {{{1
instructions = [
    Instruction('Store'       , 'target.type()->context.getVoidType()'          , ['ASTValue target', 'ASTValue value'            ], [type_must_be('target.type()', 'PointerType'), 'static_cast<PointerType*>(target.type())->ty == value.type()']),
    Instruction('Phi'         , 'prevs[0].second.type()'                        , ['std::vector<std::pair<Block*,ASTValue>> prevs'], ['prevs.size() > 0']),

    Instruction('Register'    , 'ty->context.getPointerType(ty)'                , ['Type* ty',                                    ], [], declared=True),

    Instruction('Or'          , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_must_be('lhs.type()', 'BoolType'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('And'         , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_must_be('lhs.type()', 'BoolType'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('Not'         , 'op.type()->context.getBoolType()'              , ['ASTValue op'                                  ], []),

    Instruction('ICmpNE'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpEQ'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpLT'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpGT'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpLE'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpGE'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IAdd'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('ISub'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IMult'       , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IDiv'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('IMod'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('INeg'        , 'op.type()'                                     , ['ASTValue op'                                  ], [type_is_integral('op.type()')]),

    Instruction('FCmpNE'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpEQ'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpLT'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpGT'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpLE'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpGE'      , 'lhs.type()->context.getBoolType()'             , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FAdd'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FSub'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FMult'       , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FDiv'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FMod'        , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_floating('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('FNeg'        , 'op.type()'                                     , ['ASTValue op'                                  ], [type_is_floating('op.type()')]),

    Instruction('BitXor'      , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('BitOr'       , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('BitAnd'      , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), *operands_equal('lhs.type()', 'rhs.type()')]),
    Instruction('BitNot'      , 'op.type()'                                     , ['ASTValue op'                                  ], [type_is_integral('op.type()')]),

    Instruction('ShiftR'      , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), type_is_integral('lhs.type()')]),
    Instruction('ShiftL'      , 'lhs.type()'                                    , ['ASTValue lhs', 'ASTValue rhs'                 ], [type_is_integral('lhs.type()'), type_is_integral('lhs.type()')]),

    Instruction('NoOpCast'    , 'newt'                                          , ['ASTValue op', 'Type* newt'                    ], []),

    Instruction('IntToInt'    , 'newt'                                          , ['ASTValue op', 'IntType* newt'                 ], [type_is_integral('op.type()')]),
    Instruction('IntToFloat'  , 'newt'                                          , ['ASTValue op', 'FloatType* newt'               ], [type_is_integral('op.type()')]),
    Instruction('FloatToFloat', 'newt'                                          , ['ASTValue op', 'FloatType* newt'               ], [type_is_floating('op.type()')]),
    Instruction('FloatToInt'  , 'newt'                                          , ['ASTValue op', 'IntType* newt'                 ], [type_is_floating('op.type()')]),

    Instruction('Call'        , 'f->ty->ret'                                    , ['Function* f', 'std::vector<ASTValue> args'    ], ['args.size() == f->ty->paramtys.size()']),

    Instruction('Addrof'      , 'op->type()->context.getPointerType(op->type())', ['Register* op'                                 ], []),
    Instruction('DerefPtr'    , 'static_cast<PointerType*>(ptr.type())->ty'     , ['ASTValue ptr'                                 ], [type_must_be('ptr.type()', 'PointerType')]),

    Br('Return'      , ['Register* value'                                            ], []),
    Br('GotoBr'      , ['Block* to'                                                  ], []),
    Br('CondBr'      , ['ASTValue v', 'Block* trueB', 'Block* falseB'                ], [type_must_be('v.type()', 'BoolType')]),
]

# generating stuff {{{1
def as_constructor(instruction, fields):
    fields = [field.format() for field in fields]
    if instruction.declared:
        fields.insert(0, 'ASTNS::AST *_defAST')

    return ', '.join(fields)
def as_fields(fields):
    return ''.join('        ' + field.format() + ';\n' for field in fields)
def as_init_list(instruction, fields):
    fields = [f'{f.name}({f.name})' for f in fields]
    if instruction.declared:
        fields.insert(0, '_defAST(_defAST)')

    return ', '.join(fields)

def gen_decls():
    output = []

    for instruction in instructions:
        output.append( f'    class {instruction.name};\n')

    for instruction in instructions:
        if instruction.declared:
            output.append( f'    class {instruction.name} : public {instruction.base()}, public DeclaredValue\n')
        else:
            output.append( f'    class {instruction.name} : public {instruction.base()}\n')

        output.append(    ( '    {\n'
                            '    public:\n'
                           f'        {instruction.name}({as_constructor(instruction, instruction.fields)});\n'
                           f'        void accept({instruction.base()}Visitor *v) override;\n'))
        if instruction.base() == 'Instruction':
            output.append((f'        std::string stringify() const override;\n'
                           f'        IR::Type* type() const override;\n'))

        if instruction.declared:
            output.append(  '        ASTNS::AST* defAST() const override;\n')
            output.append(  '    private:\n')
            output.append(  '        ASTNS::AST* _defAST;\n')
            output.append(  '    public:\n')

        output.append(    (f'{as_fields(instruction.fields)}'
                            '    };\n'))

    return ''.join(output)
def gen_defs():
    output = []

    for instruction in instructions:
        output.append(        f'IR::Instrs::{instruction.name}::{instruction.name}({as_constructor(instruction, instruction.fields)}): {as_init_list(instruction, instruction.fields)}\n')
        output.append(         '{\n')
        for assertion in instruction.assertions:
            output.append(    f'    ASSERT({assertion})\n')
        output.append(         '}\n')
        output.append(        f'void IR::Instrs::{instruction.name}::accept({instruction.base()}Visitor *v) {{ v->visit{instruction.name}(this); }}\n')

        if instruction.base() == 'Instruction':
            output.append(    f'IR::Type* IR::Instrs::{instruction.name}::type() const {{ return {instruction.type}; }}\n')
            output.append(    f'std::string IR::Instrs::{instruction.name}::stringify() const {{ return format("%%%", this); }}\n')

            if instruction.declared:
                output.append(f'ASTNS::AST* IR::Instrs::{instruction.name}::defAST() const {{ return _defAST; }}\n')

        output.append(         '\n')

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
