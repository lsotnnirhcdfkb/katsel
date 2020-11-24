
import re

# Instruction class {{{1
class Instruction:
    def __init__(self, name, base, autogen, *fields):
        self.name = name
        self.base = base
        if autogen is not None:
            self.autogen, self.autogeninstr, self.autogenargs = autogen
        else:
            self.autogen = False
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
    Instruction('Store', 'Instruction', None, 'Register* target', 'Value* value'),
    Instruction('Or', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('And', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpNE', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpEQ', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpULT', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpUGT', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpULE', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('IntCmpUGE', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpNE', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpEQ', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpULT', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpUGT', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpULE', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('FloatCmpUGE', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitXor', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitOr', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitAnd', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('BitNot', 'Instruction', None, 'Register* target', 'Value* op'),
    Instruction('ShiftR', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('ShiftL', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Add', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Sub', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Mult', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Div', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Mod', 'Instruction', None, 'Register* target', 'Value* lhs', 'Value* rhs'),
    Instruction('Neg', 'Instruction' , None, 'Register* target', 'Value* op'),
    Instruction('Trunc', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('ZeroExt', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('SignExt', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatTrunc', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatExt', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('SIntToFloat', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('UIntToFloat', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatToSInt', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('FloatToUInt', 'Instruction', None, 'Register* target', 'Value* op', 'Type* newt'),
    Instruction('Return', 'Instruction', None, 'Value* value nullablestringify'),
    Instruction('Call', 'Instruction', None, 'Register* reg nullablestringify', 'Function* f', 'std::vector<Value*> args iterval'),
    Instruction('GotoBr', 'Br', None, 'Block* to name'),
    Instruction('CondBr', 'Br', None, 'Value* v', 'Block* trueB name', 'Block* falseB name'),
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
                    output.append(f'    ostream << "        branch" << i << " -> " << "block" << i->{field.name} << " [label=\\"{field.name}\\"]" << std::endl;;\n')
            output.append( '}\n')

    return ''.join(output)
def genLowers():
    output = []
    for instr in instructions:
        if instr.autogen:
            output.append(    f'void Lower::Lowerer::visit{instr.name}(IR::Instrs::{instr.name} *instr)\n')
            output.append(     '{\n')
            output.append(    f'    builder.Create{instr.autogeninstr}(')
            for i, arg in enumerate(instr.autogenargs):
                if i > 0:
                    output.append(', ')
                output.append(f'lower(instr->{arg})')
            output.append(    ');\n')
            output.append(     '}\n')

    return ''.join(output)
