import helpers

# Error class {{{1
class Error:
    def __init__(self, name, fields):
        self.name = name
        self.fields = helpers.Field.process(fields)
        self.num = None
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
    Error('Expected', 'Span|where ! std::string|what'),
    Error('LhsUnsupportedOp', 'Located<NNPtr<IR::Value>>|lhs ! Span|op'),
    Error('UnaryUnsupportedOp', 'Located<NNPtr<IR::Value>>|operand ! Located<ASTNS::UnaryOperator>|op'),
    Error('NoCall', 'Located<NNPtr<IR::Value>>|func ! Span|oparn'),
    Error('IncorrectArg', 'Located<NNPtr<IR::Value>>|arg ! NNPtr<IR::Type const>|expected'),
    Error('ConflTysIfexpr', 'Located<NNPtr<IR::Value>>|truev ! Located<NNPtr<IR::Value>>|falsev ! Span|iftok ! Span|elsetok'),
    Error('AssignConflictTys', 'Located<NNPtr<IR::Value>>|lhs ! Located<NNPtr<IR::Value>>|rhs ! Span|eq'),
    Error('ConflictRetTy', 'Located<NNPtr<IR::Value>>|val ! NNPtr<IR::Function const>|f'),
    Error('NoDeref', 'Span|op ! Located<NNPtr<IR::Value>>|val'),
    Error('ConflictVarInitTy', 'Span|eq ! Span|name ! NNPtr<ASTNS::Type>|type_ast ! Located<NNPtr<IR::Value>>|init ! NNPtr<IR::Type const>|expected_type'),
    Error('InvalidCast', 'NNPtr<ASTNS::AST const>|ast ! Located<NNPtr<IR::Value>>|v ! NNPtr<IR::Type const>|newty'),
    Error('ConflictTysBinaryOp', 'Located<NNPtr<IR::Value>>|lhs ! Located<NNPtr<IR::Value>>|rhs ! Span|op'),
    Error('CondNotBool', 'Located<NNPtr<IR::Value>>|v'),
    Error('PtrArithRhsNotNum', 'Located<NNPtr<IR::Value>>|lhs ! Located<ASTNS::BinaryOperator>|optok ! Located<NNPtr<IR::Value>>|rhs'),
    Error('NoElseNotVoid', 'Located<NNPtr<IR::Value>>|truev ! Span|iftok'),
    Error('TypelessThis', 'NNPtr<ASTNS::ThisParam>|p'),
    Error('WrongNumArgs', 'NNPtr<IR::Function const>|func ! Span|oparn ! std::vector<Located<NNPtr<IR::Value>>>|args'),
    Error('RedeclSym', 'Span|name ! NNPtr<IR::Value>|val'),
    Error('UndeclSymb', 'Span|path'),
    Error('RedeclParam', 'NNPtr<ASTNS::ParamB>|param ! NNPtr<IR::Register>|prev'),
    Error('RedeclVar', 'Span|name ! NNPtr<IR::Register>|prev'),
    Error('NotA_Type', 'Span|notty'),
    Error('NoMemberIn', 'NNPtr<IR::DeclSymbol>|prev ! Span|current'),
    Error('NoThis', 'Span|th'),
    Error('NoMethod', 'Located<NNPtr<IR::Value>>|op ! Span|name'),
    Error('NoField', 'Located<NNPtr<IR::Value>>|op ! Span|name'),
    Error('AddrofNotLvalue', 'Span|op ! Located<NNPtr<IR::Value>>|val'),
    Error('AssignInvalidLhs', 'Span|eq ! Located<NNPtr<IR::Value>>|lhs'),
    Error('AssignNotMut', 'Located<NNPtr<IR::Value>>|v ! Span|eq ! NNPtr<IR::Register>|reg'),
    Error('MutAddrofNonmutOp', 'Span|op ! NNPtr<IR::Register>|reg'),
    Error('ThisNotFirst', 'NNPtr<ASTNS::ThisParam>|ast'),
]

for err_i, err in enumerate(errors):
    err.num = err_i
# generating {{{1
# decls {{{2
def gen_decls():
    output = []

    for err in errors:
        output.append(f'class {err.name} : public Error {{\n')
        output.append( 'public:\n')
        output.append(f'    {err.name}({helpers.Field.as_ref_params(err.fields)});\n')
        output.append( 'private:\n')
        output.append(f'    static constexpr char const *CODE = "E{str(err.num).zfill(4)}";\n')
        output.append(f'    static constexpr char const *NAME = "{helpers.to_snake_case(err.name).replace("_", "-")}";\n')
        output.append(helpers.Field.as_fields(err.fields, indent=4))
        output.append( 'protected:\n')
        output.append( '    SimpleError to_simple_error() const override;\n')
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
