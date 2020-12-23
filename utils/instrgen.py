# Instruction class {{{1
class Instruction:
    def __init__(self, name, base, fields, assertions=[]):
        self.name = name
        self.base = base
        self.fields = list(map(Field, fields))
        self.assertions = assertions

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
def typeMustBe(vtype, mustbe):
    return f'dynamic_cast<{mustbe}*>({vtype})'
def operandsEqual(*operands):
    return [f'{x} == {y}' for x, y in zip(operands, operands[1:])] # zip ends when one sequence ends
def targetEqual(ty):
    return f'target->type() == {ty}'
def typeIsIntegral(v):
    return f'{typeMustBe(v, "IntType")} || {typeMustBe(v, "GenericIntType")}'
def typeIsFloating(v):
    return f'{typeMustBe(v, "FloatType")} || {typeMustBe(v, "GenericFloatType")}'
# instructions {{{1
instructions = [
    Instruction('Store'       , 'Instruction' , ['Register* target', 'ASTValue value'                                      ], [targetEqual('value.type()')]),
    Instruction('Phi'         , 'Instruction' , ['TempRegister* target', 'std::vector<std::pair<Block*,ASTValue>> prevs'   ], []),

    Instruction('Or'          , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), targetEqual('lhs.type()'), typeMustBe('lhs.type()', 'BoolType'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('And'         , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), targetEqual('lhs.type()'), typeMustBe('lhs.type()', 'BoolType'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('Not'         , 'Instruction' , ['TempRegister* target', 'ASTValue op'                                     ], [typeMustBe('target->type()', 'BoolType'), targetEqual('op.type()')]),

    Instruction('ICmpNE'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpEQ'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpLT'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpGT'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpLE'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('ICmpGE'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('IAdd'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , targetEqual('lhs.type()'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('ISub'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , targetEqual('lhs.type()'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('IMult'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , targetEqual('lhs.type()'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('IDiv'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , targetEqual('lhs.type()'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('IMod'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , targetEqual('lhs.type()'), typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('INeg'        , 'Instruction' , ['TempRegister* target', 'ASTValue op'                                     ], [typeIsIntegral('target->type()') , targetEqual('op.type()'), typeIsIntegral('op.type()')]),

    Instruction('FCmpNE'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType') , typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpEQ'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType') , typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpLT'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType') , typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpGT'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType') , typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpLE'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType') , typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FCmpGE'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeMustBe('target->type()', 'BoolType') , typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FAdd'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsFloating('target->type()'), targetEqual('lhs.type()'), typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FSub'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsFloating('target->type()'), targetEqual('lhs.type()'), typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FMult'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsFloating('target->type()'), targetEqual('lhs.type()'), typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FDiv'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsFloating('target->type()'), targetEqual('lhs.type()'), typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FMod'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsFloating('target->type()'), targetEqual('lhs.type()'), typeIsFloating('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('FNeg'        , 'Instruction' , ['TempRegister* target', 'ASTValue op'                                     ], [typeIsFloating('target->type()'), targetEqual('op.type()'), typeIsFloating('op.type()')]),

    Instruction('BitXor'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('BitOr'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('BitAnd'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()') , typeIsIntegral('lhs.type()'), *operandsEqual('lhs.type()', 'rhs.type()')]),
    Instruction('BitNot'      , 'Instruction' , ['TempRegister* target', 'ASTValue op'                                     ], [typeIsIntegral('target->type()') , typeIsIntegral('op.type()')]),

    Instruction('ShiftR'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()'), targetEqual('lhs.type()')]),
    Instruction('ShiftL'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], [typeIsIntegral('target->type()'), targetEqual('lhs.type()')]),

    Instruction('NoOpCast'    , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'Type* newt'                       ], []),

    Instruction('IntToInt'    , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'IntType* newt'                    ], [typeIsIntegral('op.type()'), targetEqual('newt')]),
    Instruction('IntToFloat'  , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'FloatType* newt'                  ], [typeIsIntegral('op.type()'), targetEqual('newt')]),
    Instruction('FloatToFloat', 'Instruction' , ['TempRegister* target', 'ASTValue op', 'FloatType* newt'                  ], [typeIsFloating('op.type()'), targetEqual('newt')]),
    Instruction('FloatToInt'  , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'IntType* newt'                    ], [typeIsFloating('op.type()'), targetEqual('newt')]),

    Instruction('Call'        , 'Instruction' , ['TempRegister* target', 'Function* f', 'std::vector<ASTValue> args'       ], [targetEqual('f->ty->ret'), 'args.size() == f->ty->paramtys.size()']),

    Instruction('Return'      , 'Br'          , ['Register* value'                                            ], []),
    Instruction('GotoBr'      , 'Br'          , ['Block* to'                                                  ], []),
    Instruction('CondBr'      , 'Br'          , ['ASTValue v', 'Block* trueB', 'Block* falseB'                ], [typeMustBe('v.type()', 'BoolType')]),
]

# generating stuff {{{1
def asConstructor(fields):
    return ', '.join(map(lambda x: x.format(), fields))
def asFields(fields):
    return ''.join(map(lambda x: '        ' + x.format() + ';\n', fields))
def asInitializerList(fields):
    return ', '.join(map(lambda f: f'{f.name}({f.name})', fields))

def genDecls():
    output = []

    for instruction in instructions:
        output.append((f'    class {instruction.name} : public {instruction.base}\n'
                        '    {\n'
                        '    public:\n'
                       f'        {instruction.name}({asConstructor(instruction.fields)});\n'
                       f'        void accept({instruction.base}Visitor *v) override;\n'
                       f'{asFields(instruction.fields)}'
                        '    };\n'))

    return ''.join(output)
def genDefs():
    output = []

    for instruction in instructions:
        output.append(    f'IR::Instrs::{instruction.name}::{instruction.name}({asConstructor(instruction.fields)}): {asInitializerList(instruction.fields)}\n')
        output.append(     '{\n')
        for assertion in instruction.assertions:
            output.append(f'    ASSERT({assertion})\n')
        output.append(     '}\n')
        output.append(    f'void IR::Instrs::{instruction.name}::accept({instruction.base}Visitor *v) {{ v->visit{instruction.name}(this); }}\n')

    return ''.join(output)
def genCFGDotter():
    output = []
    for instr in instructions:
        if instr.base == 'Br':
            output.append(f'void IR::CFGDotter::visit{instr.name}(IR::Instrs::{instr.name} *i)\n')
            output.append( '{\n')
            for field in instr.fields:
                if field.type_ == 'Block*':
                    output.append(f'    ostream << "        branch" << i << " -> " << "block" << i->{field.name} << " [label=\\"{field.name}\\"]\\n";\n')
            output.append( '}\n')

    return ''.join(output)

def genMethodDecls(base):
    output = []
    for instr in instructions:
        if instr.base == base:
            output.append(f'void visit{instr.name}(IR::Instrs::{instr.name} *i) override;\n')

    return ''.join(output)

def genPureVirtualMethodDecls(base):
    output = []
    for instr in instructions:
        if instr.base == base:
            output.append(f'virtual void visit{instr.name}(IR::Instrs::{instr.name} *i) = 0;\n')

    return ''.join(output)
