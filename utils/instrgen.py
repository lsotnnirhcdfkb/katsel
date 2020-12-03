
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
    Instruction('Store'       , 'Instruction' , ['Register* target', 'ASTValue value'                         ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'target->type() == value.type()']),
    Instruction('Or'          , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('And'         , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('CmpNE'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()']),
    Instruction('CmpEQ'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()']),
    Instruction('CmpLT'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()']),
    Instruction('CmpGT'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()']),
    Instruction('CmpLE'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()']),
    Instruction('CmpGE'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()']),
    Instruction('BitXor'      , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('BitOr'       , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('BitAnd'      , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('BitNot'      , 'Instruction' , ['Register* target', 'ASTValue op'                            ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'op.type() == target->type()']),
    Instruction('ShiftR'      , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('ShiftL'      , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('Add'         , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('Sub'         , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('Mult'        , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('Div'         , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('Mod'         , 'Instruction' , ['Register* target', 'ASTValue lhs', 'ASTValue rhs'           ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'lhs.type() == rhs.type()', 'lhs.type() == target->type()']),
    Instruction('Neg'         , 'Instruction' , ['Register* target', 'ASTValue op'                            ], ['!dynamic_cast<IR::VoidType*>(target->type())', 'op.type() == target->type()']),
    Instruction('Trunc'       , 'Instruction' , ['Register* target', 'ASTValue op', 'Type* newt'              ], ['target->type() == newt']),
    Instruction('Ext'         , 'Instruction' , ['Register* target', 'ASTValue op', 'Type* newt'              ], ['target->type() == newt']),
    Instruction('IntToFloat'  , 'Instruction' , ['Register* target', 'ASTValue op', 'Type* newt'              ], ['target->type() == newt']),
    Instruction('FloatToInt'  , 'Instruction' , ['Register* target', 'ASTValue op', 'Type* newt'              ], ['target->type() == newt']),
    Instruction('Return'      , 'Instruction' , ['Register* value'                                            ], []),
    Instruction('Call'        , 'Instruction' , ['Register* reg', 'Function* f', 'std::vector<ASTValue> args' ], ['reg->type() == f->ty->ret']),
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
