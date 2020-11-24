
import re

# Instruction class {{{1
class Instruction:
    def __init__(self, name, base, *fields):
        self.name = name
        self.base = base
        self.fields = list(map(Field, fields))

class Field:

    def __init__(self, fromstr):
        if len(splitted := fromstr.split(' ')) == 3:
            self.type_, self.name, self.printMethod = splitted
        else:
            self.type_, self.name = splitted
            self.printMethod = 'stringify'

    def format(self):
        if self.type_.endswith('*'):
            return f'{self.type_[:-1].rstrip()} *{self.name}'
        else:
            return f'{self.type_} {self.name}'

    def prin(self):
        if self.printMethod == 'stringify':
            return f'ostream << i->{self.name}->stringify();'
        if self.printMethod == 'nullablestringify':
            return f'ostream << (i->{self.name} ? i->{self.name}->stringify() : "void");'
        elif self.printMethod == 'name':
            return f'ostream << i->{self.name}->name;'
        elif self.printMethod == 'iterval':
            return f'''for (IR::Value const *v : i->{self.name})
{{
    ostream << v->stringify() << " ";
}}'''
        else:
            raise Exception(f'invalid print method {self.printMethod}')

# instructions {{{1
instructions = [
    Instruction('Store', 'Instruction', 'Register* target', 'Value* value'),
    Instruction('Or', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('And', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpNE', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpEQ', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpULT', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpUGT', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpULE', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpUGE', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpNE', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpEQ', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpULT', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpUGT', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpULE', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpUGE', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitXor', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitOr', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitAnd', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitNot', 'Instruction', 'Register* target', 'Value* op'),
    Instruction('ShiftR', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('ShiftL', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Add', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Sub', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Mult', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Div', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Mod', 'Instruction', 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Neg', 'Instruction' , 'Register* target', 'Value* op'),
    Instruction('Trunc', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('ZeroExt', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('SignExt', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatTrunc', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatExt', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('SIntToFloat', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('UIntToFloat', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatToSInt', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatToUInt', 'Instruction', 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('Return', 'Instruction', 'Value* value nullablestringify'),
    Instruction('Call', 'Instruction', 'Register* reg nullablestringify', 'Function* f', 'std::vector<Value*> args iterval'),
    Instruction('GotoBr', 'Br', 'Block* b name'),
    Instruction('CondBr', 'Br', 'Value* v', 'Block* trueb name', 'Block* falseb name'),
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
def genPrinter():
    output = []
    for instr in instructions:
        output.append(f'void IR::Printer::visit{instr.name}(IR::Instrs::{instr.name} *i)\n')
        output.append( '{\n')
        output.append(f'    ostream << "{instr.name.lower()} ";\n')
        for i, field in enumerate(instr.fields):
            if i > 0:
                output.append('    ostream << " ";\n')
            output.append('    ')
            output.append(field.prin())
            output.append('\n')
        output.append('    ostream << std::endl;\n')
        output.append( '}\n')

    return ''.join(output)
def genCFGDotter():
    output = []
    for instr in instructions:
        if instr.base == 'Br':
            output.append(f'void IR::CFGDotter::visit{instr.name}(IR::Instrs::{instr.name} *i)\n')
            output.append( '{\n')
            for field in instr.fields:
                if field.type_ == 'Block*':
                    output.append(f'    ostream << "branch" << this << " -> " << "block" << i->{field.name}->name << std::endl;;\n')
            output.append( '}\n')

    return ''.join(output)
