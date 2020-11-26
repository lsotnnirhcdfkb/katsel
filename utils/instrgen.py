
import re

# Instruction class {{{1
class Instruction:
    def __init__(self, name, base, *fields):
        self.name = name
        self.base = base
        self.fields = list(map(Field, fields))

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
    Instruction('Store', 'Instruction', 'Register* target', 'ASTValue value'),
    Instruction('Or', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('And', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('CmpNE', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('CmpEQ', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('CmpLT', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('CmpGT', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('CmpLE', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('CmpGE', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('BitXor', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('BitOr', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('BitAnd', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('BitNot', 'Instruction', 'Register* target', 'ASTValue op'),
    Instruction('ShiftR', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('ShiftL', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('Add', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('Sub', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('Mult', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('Div', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('Mod', 'Instruction', 'Register* target', 'ASTValue lhs', 'ASTValue rhs'),
    Instruction('Neg', 'Instruction' , 'Register* target', 'ASTValue op'),
    Instruction('Trunc', 'Instruction', 'Register* target', 'ASTValue op', 'Type* newt'),
    Instruction('Ext', 'Instruction', 'Register* target', 'ASTValue op', 'Type* newt'),
    Instruction('IntToFloat', 'Instruction', 'Register* target', 'ASTValue op', 'Type* newt'),
    Instruction('FloatToInt', 'Instruction', 'Register* target', 'ASTValue op', 'Type* newt'),
    Instruction('Return', 'Instruction', 'Register* value'),
    Instruction('Call', 'Instruction', 'Register* reg', 'Function* f', 'std::vector<ASTValue> args'),
    Instruction('GotoBr', 'Br', 'Block* to'),
    Instruction('CondBr', 'Br', 'ASTValue v', 'Block* trueB', 'Block* falseB'),
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
        output.append(f'IR::Instrs::{instruction.name}::{instruction.name}({asConstructor(instruction.fields)}): {asInitializerList(instruction.fields)} {{}}\n')
        output.append(f'void IR::Instrs::{instruction.name}::accept({instruction.base}Visitor *v) {{ v->visit{instruction.name}(this); }}\n')

    return ''.join(output)
def genCFGDotter():
    output = []
    for instr in instructions:
        if instr.base == 'Br':
            output.append(f'void IR::CFGDotter::visit{instr.name}(IR::Instrs::{instr.name} *i)\n')
            output.append( '{\n')
            for field in instr.fields:
                if field.type_ == 'Block*':
                    output.append(f'    ostream << "        branch" << i << " -> " << "block" << i->{field.name} << " [label=\\"{field.name}\\"]" << std::endl;;\n')
            output.append( '}\n')

    return ''.join(output)
