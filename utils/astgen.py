#!/usr/bin/env python3

import parsegen
# Classes {{{1
# ASTClass {{{2
class ASTClass:
    def __init__(self, name, fields, forms):
        self.name = name
        self.fields = fields
        self.forms = forms
# ASTBaseClass {{{2
class ASTBaseClass:
    def __init__(self):
        self.name = 'AST'
# ASTField {{{2
class ASTField:
    def __init__(self, type_, name):
        self.type_ = type_
        self.name = name

    def __eq__(self, other):
        return self.type_ == other.type_ and self.name == other.name
# Classes to generate {{{1
asts = [ASTBaseClass()]
_astnames = set()
_asts = []
for rule in parsegen._grammar:
    if 'skip' not in rule or not rule['skip']:
        _astnames.add(rule['symbol'])

for astname in sorted(_astnames):
    fields = []
    forms = []
    for rule in parsegen._grammar:
        if rule['symbol'] == astname:
            if 'skip' in rule and rule['skip']:
                continue

            form = []
            for sym in rule['expansion'].split(' '):
                s, v = sym.split(':')
                if v == '_':
                    continue
                ty = 'std::unique_ptr<AST>' if s.startswith('$') else 'Token'

                field = ASTField(ty, v)
                if field not in fields:
                    fields.append(field)
                elif fields[fields.index(field)].type_ != ty:
                    raise Exception(f'conflicting types for variable {v}: {fields[fi].type_}')

                form.append(field)

            if len(form) and form not in forms:
                forms.append(form)

    asts.append(ASTClass(astname, fields, forms))
# Generating methods {{{1
# helpers {{{2
def stringifyForm(form):
    return ''.join(map(lambda f: 'A' if f.type_.startswith('std::unique_ptr<') else 'T', form))

# Generating AST stuff {{{2
# Generate AST declarations {{{3
def genASTDecls():
    output = []
    for ast in asts:
        output.append(f'    class {ast.name};\n')

    for ast in asts:
        if type(ast) != ASTBaseClass:
            output.append(f'    class {ast.name} : public AST\n')

            output.append( '    {\n')
            output.append( '    public:\n')

            for form in ast.forms:
                output.append(f'        {ast.name}({", ".join(f"{field.type_} {field.name}" for field in form)});\n')

            output.append( '        enum class Form\n')
            output.append( '        {\n')
            for form in ast.forms:
                output.append(f'            {stringifyForm(form)},\n')
            output.append( '        };\n')

            for field in ast.fields:
                output.append(f'        {field.type_} {field.name};\n')
            output.append(f'        Form form;\n')

            output.append(f'        virtual void accept(ASTVisitor *v);\n')

            output.append( '    };\n')
        else:
            output.append( '    class AST\n')
            output.append( '    {\n')
            output.append( '    public:\n')
            output.append( '        virtual ~AST() {}\n')
            output.append( '        virtual void accept(ASTVisitor *v) = 0;\n')
            output.append( '    };\n')

    return ''.join(output)
# Generate AST definitions {{{3
def genASTDefs():
    output = ['#include "parse/ast.h"\n']
    for ast in asts:
        if type(ast) != ASTBaseClass:
            for form in ast.forms:
                output.append(f'ASTNS::{ast.name}::{ast.name}({", ".join(f"{field.type_} {field.name}" for field in form)}): ')

                initializerList = []
                for field in form:
                    if field.type_.startswith('std::unique_ptr'):
                        initializerList.append(f'{field.name}(std::move({field.name}))')
                    else:
                        initializerList.append(f'{field.name}({field.name})')

                initializerList.append(f'form(ASTNS::{ast.name}::Form::{stringifyForm(form)})')

                output.append(', '.join(initializerList))
                output.append(' {}\n')

            output.append(f'void ASTNS::{ast.name}::accept(ASTVisitor *v) {{ v->visit{ast.name}(this); }}\n')

    return ''.join(output)
# Generating Visitor stuff {{{2
# Generate AST forward declarations {{{3
def genASTForwDecls():
    output = []
    for ast in asts:
        output.append(f'class {ast.name};\n')
    return ''.join(output)
# Generate visitor method declarations {{{3
def genVisitorMethods(base=False):
    output = []
    for ast in asts:
        if type(ast) == ASTBaseClass:
            continue

        if base:
            output.append(f'virtual void visit{ast.name}(ASTNS::{ast.name} *ast) = 0;\n')
        else:
            output.append(f'void visit{ast.name}(ASTNS::{ast.name} *ast) override;\n')

    return ''.join(output)
# Generate location visitor impls {{{3
def genLocVisit():
    output = []
    for ast in asts:
        if type(ast) == ASTBaseClass:
            continue

        output.append(        f'void LocationVisitor::visit{ast.name}(ASTNS::{ast.name} *ast)\n')
        output.append(         '{\n')
        output.append(         '    switch (ast->form)\n')
        output.append(         '    {\n')
        for form in ast.forms:
            output.append(    f'        case ASTNS::{ast.name}::Form::{stringifyForm(form)}:\n')
            firstfield = form[0]
            lastfield = form[-1]

            if firstfield.type_.startswith('std::unique_ptr'):
                output.append(f'            retl = getL(ast->{firstfield.name}.get());\n')
                output.append(f'            retf = getF(ast->{firstfield.name}.get());\n')
            else:
                output.append(f'            retl = ast->{firstfield.name}.start;\n')
                output.append(f'            retf = ast->{firstfield.name}.sourcefile;\n')

            if lastfield.type_.startswith('std::unique_ptr'):
                output.append(f'            retr = getR(ast->{lastfield.name}.get());\n')
            else:
                output.append(f'            retr = ast->{lastfield.name}.end;\n')

            output.append(     '            break;\n')
        output.append(         '    }\n')
        output.append(         '}\n')

    return ''.join(output)
# Generating printing stuff {{{2
# Genearte print visitor {{{3
def genPrintVisitorMethods():
    output = []
    for ast in asts:
        if type(ast) == ASTBaseClass:
            continue

        output.append(            f'void PrintVisitor::visit{ast.name}(ASTNS::{ast.name} *a)\n')
        output.append(             '{\n')
        output.append(            f'    pai("{ast.name}\\n");\n')
        output.append(            f'    ++indent;\n')
        output.append(             '    switch (a->form)\n')
        output.append(             '    {\n')
        for form in ast.forms:
            output.append(        f'        case ASTNS::{ast.name}::Form::{stringifyForm(form)}:\n')
            for field in form:
                output.append(    f'            pai("{field.name} =");\n')
                if field.type_.startswith('std::unique_ptr'):
                    output.append(f'            if (a->{field.name})\n')
                    output.append( '            {\n')
                    output.append( '                ++indent;\n')
                    output.append( '                pai("\\n");\n')
                    output.append(f'                a->{field.name}->accept(this);\n')
                    output.append( '                --indent;\n')
                    output.append( '            }\n')
                    output.append( '            else\n')
                    output.append( '            {\n')
                    output.append( '                pai(" nullptr\\n");\n')
                    output.append( '            }\n')
                else:
                    output.append( '            pai(" [");\n')
                    output.append(f'            pai(std::string(a->{field.name}.start, a->{field.name}.end));\n')
                    output.append( '            pai("]\\n");\n')
            output.append(        f'            break;\n')
        output.append(             '    }\n')
        output.append(            f'    --indent;\n')
        output.append(             '}\n')

    return ''.join(output)
# Generate dot visitor {{{3
def genDotVisitorMethods():
    output = []
    for ast in asts:
        if type(ast) == ASTBaseClass:
            continue

        output.append(            f'void DotVisitor::visit{ast.name}(ASTNS::{ast.name} *a)\n')
        output.append(             '{\n')
        output.append(             '    std::string thisid = curid();\n')
        output.append(             '    switch (a->form)\n')
        output.append(             '    {\n')
        for form in ast.forms:
            output.append(        f'        case ASTNS::{ast.name}::Form::{stringifyForm(form)}:\n')
            output.append(        f'            std::cout << thisid << " [label=<<table border=\\"0\\" cellborder=\\"1\\" cellspacing=\\"0\\"><tr><td port=\\"__heading\\" colspan=\\"{len(form)}\\">{ast.name} ({stringifyForm(form)})</td></tr><tr>";\n')
            for field in form:
                output.append(    f'            std::cout << "<td port=\\"{field.name}\\">{field.name}</td>";\n')

            output.append(        f'            std::cout << "</tr></table>>]\\n";\n')

            for field in form:
                output.append(     '            {\n')
                if field.type_.startswith('std::unique_ptr'):
                    output.append(f'                    if (a->{field.name})\n')
                    output.append( '                    {\n')
                    output.append(f'                        a->{field.name}->accept(this);\n')
                    output.append(f'                        connect(thisid, "{field.name}", lastid);\n')
                    output.append( '                    }\n')
                    output.append( '                    else\n')
                    output.append( '                    {\n')
                    output.append(f'                        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");\n')
                    output.append(f'                        connect(thisid, "{field.name}", nullptrnodeid);\n')
                    output.append( '                    }\n')
                else:
                    output.append(f'                    std::string tokennodeid = makeTextNode("Token", a->{field.name}.stringify());\n')
                    output.append(f'                    connect(thisid, "{field.name}", tokennodeid);\n')
                output.append(     '            }\n')
            output.append(         '            break;\n')

        output.append(             '    }\n')
        output.append(             '    lastid = std::move(thisid);\n')
        output.append(             '}\n')

    return ''.join(output)
