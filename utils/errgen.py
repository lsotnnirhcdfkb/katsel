import helpers

# Error class {{{1
class Error:
    def __init__(self, name, fields):
        self.name = name
        self.fields = helpers.Field.process(fields)
# errors {{{1
errors = [
    Error('BadChar', 'Span|tok'),
    Error('UntermCharlit', 'Span|tok'),
    Error('UntermStrlit', 'Span|tok'),
    Error('IndentBlockCbrace', 'Span|tok'),
    Error('InvalidNumlitBase', 'Span|tok'),
    Error('NondecimalFloatlit', 'Span|tok'),
    Error('InvalidCharForBase', 'Span|tok'),
    Error('IntlitNoDigits', 'Span|tok'),
    Error('MulticharCharlit', 'Span|tok'),
    Error('UntermMultilineComment', 'Span|tok'),
    Error('DedentNomatch', 'Span|tok'),
    Error('Expected', 'Span|expected ! std::string|name'),
    Error('LhsUnsupportedOp', 'Located<NNPtr<IR::Value>>|lhs ! Span|op'),
    Error('UnaryUnsupportedOp', 'Located<NNPtr<IR::Value>>|operand ! Located<ASTNS::UnaryOperator>|op'),
    Error('CallNoncallable', 'Located<NNPtr<IR::Value>>|func ! Span|oparn'),
    Error('IncorrectArg', 'Located<NNPtr<IR::Value>>|arg ! NNPtr<IR::Type>|expected'),
    Error('ConflTysIfexpr', 'Located<NNPtr<IR::Value>>|truev ! Located<NNPtr<IR::Value>>|falsev ! Span|iftok ! Span|elsetok'),
    Error('AssignConflictTys', 'Located<NNPtr<IR::Value>>|lhs ! Located<NNPtr<IR::Value>>|rhs ! Span|eq'),
    Error('ConflictRetTy', 'Located<NNPtr<IR::Value>>|val ! NNPtr<IR::Function>|f'),
    Error('NoDeref', 'Span|op ! Located<NNPtr<IR::Value>>|val'),
    Error('ConflictVarInitTy', 'Span|eq ! Span|name ! NNPtr<ASTNS::Type>|type_ast ! Located<NNPtr<IR::Value>>|init ! NNPtr<IR::Type>|expected_type'),
    Error('InvalidCast', 'NNPtr<ASTNS::AST>|ast ! Located<NNPtr<IR::Value>>|v ! NNPtr<IR::Type>|newty'),
    Error('ConflictTysBinaryOp', 'Located<NNPtr<IR::Value>>|lhs ! Located<NNPtr<IR::Value>>|rhs ! Span|op'),
    Error('CondNotBool', 'Located<NNPtr<IR::Value>>|v'),
    Error('PtrArithRhsNotNum', 'Located<NNPtr<IR::Value>>|lhs ! Located<ASTNS::BinaryOperator>|optok ! Located<NNPtr<IR::Value>>|rhs'),
    Error('NoElseNotVoid', 'Located<NNPtr<IR::Value>>|truev ! Span|iftok'),
    Error('TypelessThis', 'NNPtr<ASTNS::ThisParam>|p'),
    Error('WrongNumArgs', 'NNPtr<IR::Function>|func ! NNPtr<ASTNS::AST>|func_ref_ast ! Span|oparn ! std::vector<Located<NNPtr<IR::Value>>>|args'),
    Error('RedeclSym', 'Span|name ! NNPtr<IR::Value>|val'),
    Error('UndeclSymb', 'Span|path'),
    Error('RedeclParam', 'NNPtr<ASTNS::ParamB>|param ! NNPtr<IR::Register>|prev'),
    Error('RedeclVar', 'Span|name ! NNPtr<IR::Register>|prev'),
    Error('NotAType', 'Span|notty'),
    Error('NoMemberIn', 'NNPtr<IR::DeclSymbol>|prev ! Span|current'),
    Error('NoThis', 'Span|th'),
    Error('NoMethod', 'Located<NNPtr<IR::Value>>|op ! Span|name'),
    Error('NoField', 'Located<NNPtr<IR::Value>>|op ! Span|name'),
    Error('AddrofNotLvalue', 'Span|op ! Located<NNPtr<IR::Value>>|val'),
    Error('AssignInvalidLhs', 'Span|eq ! Located<NNPtr<IR::Value>>|lhs'),
    Error('AssignNotMut', 'Located<NNPtr<IR::Value>>|v ! Span|eq ! NNPtr<IR::Register>|reg'),
    Error('MutAddrofNonmutOp', 'Span|op ! NNPtr<IR::Register>|reg'),
    Error('NoSuppress', 'Span|dollar'),
    Error('ThisNotFirst', 'NNPtr<ASTNS::ThisParam>|ast'),
]
# generating {{{1
# decls {{{2
def gen_decls():
    output = []

    for err in errors:
        output.append(f'class {err.name} : public Error {{\n')
        output.append( 'public:\n')
        output.append(f'    {err.name}({helpers.Field.as_ref_params(err.fields)});\n')
        output.append( 'private:\n')
        output.append(helpers.Field.as_fields(err.fields, indent=4))
        output.append( 'protected:\n')
        output.append( '    SimpleError toSimpleError() const override;\n')
        output.append( '};\n')

    return ''.join(output)
# defs {{{2
def gen_defs():
    output = []

    for err in errors:
        output.append(f'using Errors::{err.name};\n')
        output.append(f'{err.name}::{err.name}({helpers.Field.as_ref_params(err.fields)}): ')
        output.append(helpers.Field.as_init_list(err.fields))
        output.append(' {}\n')


    return ''.join(output)
