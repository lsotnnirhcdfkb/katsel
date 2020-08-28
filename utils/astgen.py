#!/usr/bin/env python3
## @package astgen
#  Generate AST classes
#
#  Generate ast.h, ast.cpp, forward declarations to go in visitor.h, visitBlahAST methods, and generate BlankVisitor methods

# class: AstClass {{{1
## Class representing an AST Class
class AstClass:
    ## @var name
    # the name of the AST class that this represents

    ## @var fields
    # the fields that this AST class has

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
            output.append(f'/// {self.doc}\n')

        output.append(    f'class {self.name} : public AST\n')
        output.append(     '{\n')
        output.append(     'public:\n')
        output.append(     '    /// The constructor for this class\n')
        for param in self.fields:
            output.append(f'    /// @param {param.name} {param.doc}\n')
        output.append(    f'    {self.name}({self.genConstructArgs()});\n')
        output.append(     '    /// The accept method that calls the correct visitor method on this AST\n')
        output.append(     '    void accept(Visitor *v) override;\n')
        output.append('\n')
        for field in self.fields:
            output.append(f'    /// {field.doc}\n')
            output.append(f'    {field.printHFile(False)};\n')
        output.append(     '};\n')

        return ''.join(output)

    # AstClass.printCppFile {{{2
    ## Return the generated string that includes the constructors and the accept methods of all the AST Classes
    def printCppFile(self):
        output = []
        output.append(f'{self.name}::{self.name}({self.genConstructArgs()})')

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
                            f'    for (std::unique_ptr<AST> &ast : {field.name})\n'
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

        output.append(f'void {self.name}::accept(Visitor *v) {{ v->visit{self.name}(this); }}\n')
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
        '/// A base AST class\n'
        'class AST\n'
        '{\n'
        'public:\n'
        '    /// The virtual constructor\n'
        '    virtual ~AST() {}\n'
        '\n'
        '    /// A pure virtual accept method that each AST class is supposed to implement to call the right visitor method\n'
        '    virtual void accept(Visitor *v) = 0;\n'
        '};\n'
        )
    
    ## Return nothing because this class shouldn't appear in ast.cpp
    def printCppFile(self):
        return ''

# class: AstField {{{1
## A class representing a field in an instance of AstClass
class AstField:
    ## An "enum" constant meaning `Token`
    TTOKEN = 0
    ## An "enum" constant meaning `std::vector<std::unique_ptr<AST>>`
    TVECTOR = 1
    ## An "enum" constant meaning `std::unique_ptr<AST>`
    TUPTR = 2

    ## A dict containing each type constant what it should generate as
    TTOCTYPE = {
        TTOKEN: 'Token',
        TVECTOR: 'std::vector<std::unique_ptr<AST>>',
        TUPTR: 'std::unique_ptr<AST>',
    }

    ## @var name
    # The name of the field

    ## @var type_
    # The type of the field

    ## The constructor
    def __init__(self, name, type_, doc=''):
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
    cppfile.append('#include "ast.h"\n')
    for class_ in astClasses:
        cppfile.append(class_.printCppFile() + '\n')

    return ''.join(cppfile)

# generation functions: visitor files {{{1
# generate ast forward declarations for visitor.h {{{2
## Print the list of forward declarations that go in the top of visitor.h
def forwardDecl():
    return ''.join([f'class {c.name};\n' for c in astClasses])
# generate visitAST methods {{{2
## Print the visitSomethingAST methods to go in a class
# @param isBase If the generated class is the base AST class so that it can generate either `= 0;` or `override;`
def visitASTMethods(isBase):
    output = []
    for astClass in astClasses:
        if astClass.name == 'AST':
            continue

        if isBase:
            output.append('    virtual ')
        else:
            output.append('    ')

        output.append(f'void visit{astClass.name}(const {astClass.name} *ast)')

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
        output.append(f'void BlankVisitor::visit{astClass.name}(const {astClass.name} *ast) {{}}\n')
    return ''.join(output)
# lists: ast classes to generate {{{1
## The list of AST classes to generate
astClasses = [
    AstBaseClass(),
    AstClass('Binary'       , [AstField('op', AstField.TTOKEN, 'The operator'), AstField('last', AstField.TUPTR, 'The left operand'), AstField('rast', AstField.TUPTR, 'The right operator')], 'An AST for binary operators'),
    AstClass('TernaryOp'    , [AstField('conditional', AstField.TUPTR), AstField('trueast', AstField.TUPTR), AstField('falseast', AstField.TUPTR)], 'An AST for the ternary operator (?:)'),
    AstClass('Unary'        , [AstField('op', AstField.TTOKEN), AstField('ast', AstField.TUPTR)], 'An AST for unary operators'),
    AstClass('Primary'      , [AstField('value', AstField.TTOKEN)], 'An AST for primary tokens (literals etc.)'),
    AstClass('ExprStmt'     , [AstField('ast', AstField.TUPTR)], 'An AST for an expression statement'),
    AstClass('Program'      , [AstField('asts', AstField.TVECTOR)], 'An AST representing an entire program'),
    AstClass('Function'     , [AstField('type', AstField.TUPTR), AstField('name', AstField.TTOKEN), AstField('params', AstField.TUPTR), AstField('body', AstField.TUPTR)], 'An AST representing a function declaration or definition'),
    AstClass('Block'        , [AstField('stmts', AstField.TVECTOR)], 'An AST representing a code block'),
    AstClass('Type'         , [AstField('type', AstField.TTOKEN)], 'An AST for a type'),
    AstClass('Param'        , [AstField('type', AstField.TUPTR), AstField('paramname', AstField.TTOKEN)], 'An AST representing a parameter'),
    AstClass('Params'       , [AstField('params', AstField.TVECTOR)], 'An AST representing a parameter list'),
    AstClass('VarStmt'      , [AstField('type', AstField.TUPTR), AstField('name', AstField.TTOKEN), AstField('expression', AstField.TUPTR)], 'An AST representing a variable declaration statement'),
    AstClass('Assign'       , [AstField('lhs', AstField.TUPTR), AstField('rhs', AstField.TUPTR), AstField('equalSign', AstField.TTOKEN)], 'An AST representing an assignment expression'),
    AstClass('VariableRef'  , [AstField('var', AstField.TTOKEN)], 'An AST for a variable reference'),
    AstClass('ReturnStmt'   , [AstField('expr', AstField.TUPTR)], 'An AST representing a return statement'),
    AstClass('Arg'          , [AstField('expr', AstField.TUPTR)], 'An AST representing an arguemnt passed into a function call'),
    AstClass('Args'         , [AstField('args', AstField.TVECTOR)], 'An AST representing arguments passed into a function call'),
    AstClass('Call'         , [AstField('varrefast', AstField.TUPTR), AstField('arglistast', AstField.TUPTR), AstField('oparn', AstField.TTOKEN)], 'An AST representing a function call'),
]

