#!/usr/bin/env python3
## @file astgen
#  Generate AST classes
#  Generate ast.h, ast.cpp, forward declarations to go in visitor.h, visitBlahAST methods, and generate BlankVisitor methods

# class: AstClass {{{1
## Class representing an AST Class
class AstClass:
    ## @var name
    # The name of the AST class that this represents

    ## @var fields
    # The fields that this AST class has

    ## @var doc
    # The docuemntation to be generated with this AST class

    ## The constructor
    def __init__(self, name, fields=[], doc=''):
        self.name = name + 'AST'
        self.fields = fields
        self.doc = doc 

    # AstClass.printHFile {{{2
    ## Return the generated string that can go into ast.h
    def printHFile(self):
        output = []

        if len(self.doc):
            output.append(f'    /// {self.doc}\n')

        output.append(    f'    class {self.name} : public AST\n')
        output.append(     '    {\n')
        output.append(     '    public:\n')
        output.append(     '        /// The constructor for this class\n')
        for param in self.fields:
            output.append(f'        /// @param {param.name} {param.doc}\n')
        output.append(    f'        {self.name}({self.genConstructArgs()});\n')
        output.append(     '        /// The accept method that calls the correct visitor method on this AST\n')
        output.append(     '        void accept(Visitor *v) override;\n')
        output.append('    \n')
        for field in self.fields:
            output.append(f'        /// {field.doc}\n')
            output.append(f'        {field.printHFile(False)};\n')
        output.append(     '    };\n')

        return ''.join(output)

    # AstClass.printCppFile {{{2
    ## Return the generated string that includes the constructors and the accept methods of all the AST Classes
    def printCppFile(self):
        output = []
        output.append(f'ASTs::{self.name}::{self.name}({self.genConstructArgs()})')

        initializerList = []
        constructorBody = []

        for field in self.fields:
            if field.type_ == AstField.TTOKEN:
                initializerList.append(f'{field.name}({field.name})')
            elif field.type_ == AstField.TUPTR:
                initializerList.append(f'{field.name}(std::move({field.name}))')
            elif field.type_ == AstField.TVECTOR:
                constructorBody.append((
                            f'    this->{field.name}.reserve({field.name}.size());\n'
                            f'    for (std::unique_ptr<ASTs::AST> &ast : {field.name})\n'
                             '    {\n'
                            f'        this->{field.name}.push_back(std::move(ast));\n'
                             '    }\n'
                            ))

        if len(initializerList):
            output.append(f': {", ".join(initializerList)}')

        if len(constructorBody):
            output.append('\n{\n' + "\n".join(constructorBody) + '}\n')
        else:
            output.append(' {}\n')

        output.append(f'void ASTs::{self.name}::accept(Visitor *v) {{ v->visit{self.name}(this); }}\n')
        return ''.join(output)

    # AstClass helper methods {{{2
    ## Construct the arguement list for the constructors
    def genConstructArgs(self):
        return ", ".join(f.printHFile(True) for f in self.fields)

# class: AstBaseClass {{{1
## Class representing the pure virtual AST class
class AstBaseClass:
    ## @var name
    # A placeholder field so that the printing methods can work properly

    ## The constructor
    def __init__(self):
        self.name = 'AST'

    ## Return a string containing what this class should become in ast.h
    def printHFile(self):
        return (
        '    /// A base AST class\n'
        '    class AST\n'
        '    {\n'
        '    public:\n'
        '        /// The virtual constructor\n'
        '        virtual ~AST() {}\n'
        '    \n'
        '        /// A pure virtual accept method that each AST class is supposed to implement to call the right visitor method\n'
        '        virtual void accept(Visitor *v) = 0;\n'
        '    };\n'
        )
    
    ## Return nothing because this class shouldn't appear in ast.cpp
    def printCppFile(self):
        return ''

# class: AstField {{{1
## A class representing a field in an instance of AstClass
class AstField:
    ## An "enum" constant meaning `Token`
    TTOKEN = 0
    ## An "enum" constant meaning `std::vector<std::unique_ptr<ASTs::AST>>`
    TVECTOR = 1
    ## An "enum" constant meaning `std::unique_ptr<ASTs::AST>`
    TUPTR = 2

    ## A dict containing each type constant what it should generate as
    TTOCTYPE = {
        TTOKEN: 'Token',
        TVECTOR: 'std::vector<std::unique_ptr<ASTs::AST>>',
        TUPTR: 'std::unique_ptr<ASTs::AST>',
    }

    ## @var name
    # The name of the field

    ## @var type_
    # The type of the field

    ## @var doc
    # The documentation to be generated with this field

    ## The constructor
    def __init__(self, name, type_, doc):
        self.name = name
        self.type_ = type_
        self.doc = doc

    ## Return what this field should generate into
    def printHFile(self, isArgPrint):
        if isArgPrint and self.type_ == AstField.TVECTOR:
            return f'{AstField.TTOCTYPE[self.type_]} &{self.name}'
        else:
            return f'{AstField.TTOCTYPE[self.type_]} {self.name}'

# generation functions: ast.h ast.cpp files {{{1
# print ast.h file {{{2
## Print what should go in ast.h
def astHFile():
    hfile = []
    for class_ in astClasses:
        hfile.append(class_.printHFile() + '\n')

    return ''.join(hfile)

# print ast.cpp file {{{2
## Print what should go in ast.cpp
def astCppFile():
    cppfile = []
    cppfile.append('/// @file ast.cpp\n')
    cppfile.append('/// AST method declarations\n')
    cppfile.append('\n')
    cppfile.append('#include "ast.h"\n')
    for class_ in astClasses:
        cppfile.append(class_.printCppFile() + '\n')

    return ''.join(cppfile)

# generation functions: visitor files {{{1
# generate ast forward declarations for visitor.h {{{2
## Print the list of forward declarations that go in the top of visitor.h
def forwardDecl():
    return ''.join([f'    class {c.name};\n' for c in astClasses])
# generate visitAST methods {{{2
## Print the visitSomethingAST methods to go in a class
# @param isBase If the generated class is the base AST class so that it can generate either `= 0;` or `override;`
# @param doc The documentation to go along with this method
def visitASTMethods(isBase, doc, indent=4):
    output = []
    indentstr = ' ' * indent
    for astClass in astClasses:
        if astClass.name == 'AST':
            continue

        output.append(f'{indentstr}/// {doc.format(astClass.name)}\n')
        output.append(f'{indentstr}/// @param ast The ast to visit\n')

        if isBase:
            output.append(f'{indentstr}virtual ')
        else:
            output.append(f'{indentstr}')

        output.append(f'void visit{astClass.name}(const ASTs::{astClass.name} *ast)')

        if isBase:
            output.append(' = 0;\n')
        else:
            output.append(' override;\n')

    return ''.join(output)
# generate BlankVisitor method definitions {{{2
## Print the BlankVisitor::visitSomethingAST() methods
def blankVisitorDefinitions():
    output = []
    for astClass in astClasses:
        if astClass.name == 'AST':
            continue
        output.append(f'void BlankVisitor::visit{astClass.name}(const ASTs::{astClass.name} *ast) {{}}\n')
    return ''.join(output)
# lists: ast classes to generate {{{1
## The list of AST classes to generate
astClasses = [
    AstBaseClass(),
    AstClass(
            name='Binary',
            fields=[
                AstField('op', AstField.TTOKEN, 'The operator'),
                AstField('last', AstField.TUPTR, 'The left operand'),
                AstField('rast', AstField.TUPTR, 'The right operator'),
            ],
            doc='An AST for binary operators'
            ),
    AstClass(
            name='TernaryOp',
            fields=[
                AstField('conditional', AstField.TUPTR, 'The conditional expression'),
                AstField('trueast', AstField.TUPTR, 'The expression that this evaluates to if the codnditional is true'),
                AstField('falseast', AstField.TUPTR, 'The expression that this evaluates to if the codnditional is false'),
            ],
            doc='An AST for the ternary operator (?:)',
            ),
    AstClass(
            name='Unary',
            fields=[
                AstField('op', AstField.TTOKEN, 'The operator'),
                AstField('ast', AstField.TUPTR, 'The operand'),
            ],
            doc='An AST for unary operators'
            ),
    AstClass(
            name='Primary',
            fields=[
                AstField('value', AstField.TTOKEN, 'The value'),
            ],
            doc='An AST for primary tokens (literals etc.)'
            ),
    AstClass(
            name='ExprStmt',
            fields=[
                AstField('ast', AstField.TUPTR, 'The expression of this statement'),
            ],
            doc='An AST for an expression statement'
            ),
    AstClass(
            name='Program',
            fields=[
                AstField('asts', AstField.TVECTOR, 'The asts that this program has'),
            ],
            doc='An AST representing an entire program'
            ),
    AstClass(
            name='Function',
            fields=[
                AstField('type', AstField.TUPTR, 'The return type of the function'),
                AstField('name', AstField.TTOKEN, 'The name of the function'),
                AstField('params', AstField.TUPTR, 'The parameters of the function'),
                AstField('body', AstField.TUPTR, 'The body of the function'),
            ],
            doc='An AST representing a function declaration or definition'
            ),
    AstClass(
            name='Block',
            fields=[
                AstField('stmts', AstField.TVECTOR, 'The statements in the block'),
            ],
            doc='An AST representing a code block'
            ),
    AstClass(
            name='Type',
            fields=[
                AstField('type', AstField.TTOKEN, 'The type token'),
            ],
            doc='An AST for a type'
            ),
    AstClass(
            name='Param',
            fields=[
                AstField('type', AstField.TUPTR, 'The type of the parameter'),
                AstField('paramname', AstField.TTOKEN, 'The name of the parameter'),
            ],
            doc='An AST representing a parameter'
            ),
    AstClass(
            name='Params',
            fields=[
                AstField('params', AstField.TVECTOR, 'A vector of parameters'),
            ],
            doc='An AST representing a parameter list'
            ),
    AstClass(
            name='VarStmt',
            fields=[
                AstField('type', AstField.TUPTR, 'The type of the variable'),
                AstField('name', AstField.TTOKEN, 'The name of the variable'),
                AstField('expression', AstField.TUPTR, 'The expression being assigned to the variable'),
            ],
            doc='An AST representing a variable declaration statement'
            ),
    AstClass(
            name='Assign',
            fields=[
                AstField('lhs', AstField.TUPTR, 'The thing to assign to'),
                AstField('rhs', AstField.TUPTR, 'The expression to assign'),
                AstField('equalSign', AstField.TTOKEN, 'A token to error at in case there is an error'),
            ],
            doc='An AST representing an assignment expression'
            ),
    AstClass(
            name='VariableRef',
            fields=[
                AstField('var', AstField.TTOKEN, 'The variable being referenced'),
            ],
            doc='An AST for a variable reference'
            ),
    AstClass(
            name='LValue',
            fields=[
                AstField('expr', AstField.TUPTR, 'The expression that evaluates to an lvalue'),
            ],
            doc='An AST for any expression that is guaranteed to evaluatae to an lvalue',
            ),
    AstClass(
            name='ReturnStmt',
            fields=[
                AstField('expr', AstField.TUPTR, 'The expression to return'),
            ],
            doc='An AST representing a return statement'
            ),
    AstClass(
            name='Arg',
            fields=[
                AstField('expr', AstField.TUPTR, 'The expression that the argument is'),
            ],
            doc='An AST representing an arguemnt passed into a function call'
            ),
    AstClass(
            name='Args',
            fields=[
                AstField('args', AstField.TVECTOR, 'A vector of arguments'),
            ],
            doc='An AST representing arguments passed into a function call'
            ),
    AstClass(
            name='Call',
            fields=[
                AstField('varrefast', AstField.TUPTR, 'The variable reference that is being called'),
                AstField('arglistast', AstField.TUPTR, 'The argument list for the function call'),
                AstField('oparn', AstField.TTOKEN, 'The opening parentheses to throw an error at'),
            ],
            doc='An AST representing a function call'
            ),
]

