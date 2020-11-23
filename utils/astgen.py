#!/usr/bin/env python3

import parsegen
# Classes {{{1
# ASTClass {{{2
class ASTClass:
    def __init__(self, name, fields, forms, base, skiponly):
        self.name = name
        self.fields = fields
        self.forms = forms
        self.base = base
        self.skiponly = skiponly
# ASTBaseClass {{{2
class ASTBaseClass:
    def __init__(self, name):
        self.name = name
# ASTSuperBaseClass {{{2
class ASTSuperBaseClass:
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
asts = [ASTSuperBaseClass()]
_bases = set()
_astbases = {}
_astnames = set()
_asts = []
for rule in parsegen.grammar:
    if str(rule.symbol) == 'augment': continue
    _astnames.add(str(rule.symbol))
    _bases.add(rule.base)
    _astbases[str(rule.symbol)] = rule.base

for base in sorted(_bases):
    asts.append(ASTBaseClass(base))

for astname in sorted(_astnames):
    fields = []
    forms = []
    matchedrules = [rule for rule in parsegen.grammar if str(rule.symbol) == astname]

    base = matchedrules[0].base
    skiponly = all([r.skip for r in matchedrules])
    if skiponly:
        asts.append(ASTClass(astname, [], [], base, True))
        continue

    for rule in matchedrules:
        if rule.skip:
            continue

        form = []
        for sym, vname in zip(rule.expansion, rule.vnames):
            ty = f'std::unique_ptr<{_astbases[str(sym)]}>' if type(sym) == parsegen.NonTerminal else 'Token'

            field = ASTField(ty, vname)
            if field not in fields:
                fields.append(field)
            elif fields[fields.index(field)].type_ != ty:
                raise Exception(f'conflicting types for variable {v}: {fields[fi].type_}')

            form.append(field)

        if len(form) and form not in forms:
            forms.append(form)

    asts.append(ASTClass(astname, fields, forms, base, False))
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
        if type(ast) == ASTClass:
            output.append(f'    class {ast.name} : public {ast.base}\n')

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

            if not ast.skiponly:
                output.append(f'        virtual void accept(ASTNS::{ast.base}Visitor *v);\n')

            output.append( '    };\n')
        elif type(ast) == ASTBaseClass:
            output.append(f'    class {ast.name} : public AST\n')
            output.append( '    {\n')
            output.append( '    public:\n')
            output.append(f'        virtual ~{ast.name}() {{}}\n')
            output.append(f'        virtual void accept(ASTNS::{ast.name}Visitor *v) = 0;\n')
            output.append( '    };\n')
        else:
            output.append( '    class AST\n')
            output.append( '    {\n')
            output.append( '    public:\n')
            output.append( '        virtual ~AST() {}\n')
            output.append( '    };\n')

    return ''.join(output)
# Generate AST definitions {{{3
def genASTDefs():
    output = ['#include "ast/ast.h"\n']
    for ast in asts:
        if type(ast) == ASTClass and not ast.skiponly:
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

            output.append(f'void ASTNS::{ast.name}::accept(ASTNS::{ast.base}Visitor *v) {{ v->visit{ast.name}(this); }}\n')

    return ''.join(output)
# Generating Visitor stuff {{{2
# Generate AST forward declarations {{{3
def genASTForwDecls():
    output = []
    for ast in asts:
        output.append(f'class {ast.name};\n')
    return ''.join(output)
# Generate visitor classes {{{3
def genVisitorClasses():
    output = []
    for ast in asts:
        if type(ast) != ASTBaseClass:
            continue

        output.append(f'class {ast.name}Visitor\n')
        output.append( '{\n')
        output.append( 'public:\n')
        output.append(f'    virtual ~{ast.name}Visitor() {{}}\n')
        for _ast in asts:
            if type(_ast) == ASTClass and _ast.base == ast.name and not _ast.skiponly:
                output.append(f'    virtual void visit{_ast.name}(ASTNS::{_ast.name} *ast) = 0;\n')
        output.append( '};\n')

    return ''.join(output)
# Generate overrided functions for visitor classes {{{3
def genVisitorMethods(*bases):
    output = []
    for ast in asts:
        if type(ast) != ASTClass:
            continue

        if (ast.base in bases or bases == ('all',)) and not ast.skiponly:
            output.append(f'void visit{ast.name}(ASTNS::{ast.name} *ast) override;\n')

    return ''.join(output)
# Generate location visitor impls {{{3
def genLocVisit():
    output = []
    for ast in asts:
        if type(ast) != ASTClass or ast.skiponly:
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
        if type(ast) != ASTClass or ast.skiponly:
            continue

        output.append(            f'void ASTNS::PrintVisitor::visit{ast.name}(ASTNS::{ast.name} *a)\n')
        output.append(             '{\n')
        output.append(             '    pai("{}\\n{{\\n");\n'.format(ast.name))
        output.append(             '    ++indent;\n')
        output.append(             '    switch (a->form)\n')
        output.append(             '    {\n')
        for form in ast.forms:
            output.append(        f'        case ASTNS::{ast.name}::Form::{stringifyForm(form)}:\n')
            for field in form:
                output.append(    f'            pai("{field.name} = ");\n')
                if field.type_.startswith('std::unique_ptr'):
                    output.append(f'            if (a->{field.name})\n')
                    output.append( '            {\n')
                    output.append(f'                a->{field.name}->accept(this);\n')
                    output.append( '            }\n')
                    output.append( '            else\n')
                    output.append( '            {\n')
                    output.append( '                pai("nullptr\\n");\n')
                    output.append( '            }\n')
                else:
                    output.append( '            pai("[");\n')
                    output.append(f'            pai(std::string(a->{field.name}.start, a->{field.name}.end));\n')
                    output.append( '            pai("]\\n");\n')
            output.append(         '            break;\n')
        output.append(             '    }\n')
        output.append(             '    --indent;\n')
        output.append(             '    pai("}\\n");\n')
        output.append(             '}\n')

    return ''.join(output)
# Generate dot visitor {{{3
def genDotVisitorMethods():
    output = []
    for ast in asts:
        if type(ast) != ASTClass or ast.skiponly:
            continue

        output.append(            f'void ASTNS::DotVisitor::visit{ast.name}(ASTNS::{ast.name} *a)\n')
        output.append(             '{\n')
        output.append(             '    std::string thisid = curid();\n')
        output.append(             '    switch (a->form)\n')
        output.append(             '    {\n')
        for form in ast.forms:
            output.append(        f'        case ASTNS::{ast.name}::Form::{stringifyForm(form)}:\n')
            output.append(        f'            ostream << thisid << " [label=<<table border=\\"0\\" cellborder=\\"1\\" cellspacing=\\"0\\"><tr><td port=\\"__heading\\" colspan=\\"{len(form)}\\">{ast.name} ({stringifyForm(form)})</td></tr><tr>";\n')
            for field in form:
                output.append(    f'            ostream << "<td port=\\"{field.name}\\">{field.name}</td>";\n')

            output.append(        f'            ostream << "</tr></table>>]\\n";\n')

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
