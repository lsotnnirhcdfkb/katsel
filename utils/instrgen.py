
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

# instructions {{{1
instructions = [
    Instruction('Store'       , 'Instruction' , ['Register* target', 'ASTValue value'                                      ], ['target->type() == value.type()']),
    Instruction('Phi'         , 'Instruction' , ['TempRegister* target', 'std::vector<std::pair<Block*,ASTValue>> prevs'   ], []),
    Instruction('Or'          , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('And'         , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('CmpNE'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('CmpEQ'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('CmpLT'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('CmpGT'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('CmpLE'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('CmpGE'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('BitXor'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('BitOr'       , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('BitAnd'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('BitNot'      , 'Instruction' , ['TempRegister* target', 'ASTValue op'                                     ], ['op.type() == target->type()', 'dynamic_cast<BuiltinType*>(op.type())']),
    Instruction('ShiftR'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('ShiftL'      , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('Add'         , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('Sub'         , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('Mult'        , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('Div'         , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('Mod'         , 'Instruction' , ['TempRegister* target', 'ASTValue lhs', 'ASTValue rhs'                    ], ['lhs.type() == rhs.type()', 'lhs.type() == target->type()', 'dynamic_cast<BuiltinType*>(lhs.type())']),
    Instruction('Neg'         , 'Instruction' , ['TempRegister* target', 'ASTValue op'                                     ], ['op.type() == target->type()', 'dynamic_cast<BuiltinType*>(op.type())']),
    Instruction('Trunc'       , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'BuiltinType* newt'                ], ['target->type() == newt', 'dynamic_cast<BuiltinType*>(op.type())', 'static_cast<BuiltinType*>(op.type())->isFloating() == newt->isFloating()']),
    Instruction('Ext'         , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'BuiltinType* newt'                ], ['target->type() == newt', 'dynamic_cast<BuiltinType*>(op.type())', 'static_cast<BuiltinType*>(op.type())->isFloating() == newt->isFloating()']),
    Instruction('IntToFloat'  , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'BuiltinType* newt'                ], ['target->type() == newt', 'dynamic_cast<BuiltinType*>(op.type())', '!static_cast<BuiltinType*>(op.type())->isFloating()', 'newt->isFloating()']),
    Instruction('FloatToInt'  , 'Instruction' , ['TempRegister* target', 'ASTValue op', 'BuiltinType* newt'                ], ['target->type() == newt', 'dynamic_cast<BuiltinType*>(op.type())', 'static_cast<BuiltinType*>(op.type())->isFloating()', '!newt->isFloating()']),
    Instruction('Return'      , 'Instruction' , ['Register* value'                                                         ], []),
    Instruction('Call'        , 'Instruction' , ['TempRegister* reg', 'Function* f', 'std::vector<ASTValue> args'          ], ['reg->type() == f->ty->ret']),
    Instruction('GotoBr'      , 'Br'          , ['Block* to'                                                  ], []),
    Instruction('CondBr'      , 'Br'          , ['ASTValue v', 'Block* trueB', 'Block* falseB'                ], []),
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
