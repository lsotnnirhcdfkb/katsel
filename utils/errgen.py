#!/usr/bin/env python3

import textwrap

UNDER0 = '^'
UNDER1 = '='
UNDER2 = '~'
UNDER3 = '-'
PADAMT = 4

regions = {
    (0, 100): 'lexing errors',
}

errors = [
    {
        'code': 1,
        'name': 'unexpected-char',
        'desc': 'The lexer found an unexpected character that could not begin a token.',
        'inputs': 'Token const &tok',
        'location': 'tok',
        'highlights': [
            ('tok', UNDER0, [('error', '\"unexpected character\"')])
        ]
    },
    {
        'code': 2,
        'name': 'unterm-charlit',
        'desc': 'The lexer found an unterminated character literal. A common cause of this is character literals that are more than one character long.',
        'inputs': 'Token const &tok',
        'location': 'tok',
        'highlights': [
            ('tok', UNDER0, [('error', '\"unterminated character literal\"')])
        ]
    },
    {
        'code': 3,
        'name': 'unterm-strlit',
        'desc': 'The lexer found a newline in a string literal, thereby making it unterminated. Newlines that need to appear inside the string literal must be escaped by putting `\\n`.',
        'inputs': 'Token const &tok',
        'location': 'tok',
        'highlights': [
            ('tok', UNDER0, [('error', '\"unterminated string literal\"')])
        ]
    },
    {
        'code': 4,
        'name': 'invalid-intlit-base',
        'desc': 'The lexer found an integer literal that has an invalid base.',
        'inputs': 'Token const &tok',
        'location': 'tok',
        'highlights': [
            ('tok', UNDER0, [('error', '\"invalid integer literal base\"')])
        ]
    },
    {
        'code': 5,
        'name': 'nondecimal-floatlit',
        'desc': 'The lexer found a non-decimal floating point literal.',
        'inputs': 'Token const &tok',
        'location': 'tok',
        'highlights': [
            ('tok', UNDER0, [('error', '\"invalid integer literal base\"')])
        ]
    },
]

def genH():
    output = []

    for (regionst, regione), desc in regions.items():
        output.append(f'// Errors E{str(regionst).zfill(PADAMT)}-E{str(regione).zfill(PADAMT)}: {desc}\n')

    output.append(     '\n')

    for error in errors:
        code = str(error['code']).zfill(PADAMT)
        output.append(f'// E{code} - {error["name"]}\n')
        output.append(f'#define ERR_{error["name"].upper().replace("-", "_")} E{code}\n')
        output.append(f'void E{code}({error["inputs"]});\n')
        output.append( '\n')

    return ''.join(output)

def genCpp():
    output = []

    for error in errors:
        code = str(error['code']).zfill(PADAMT)
        output.append(        f'// E{code} - {error["name"]}\n')
        descWrapped = "".join("// | " + line + "\n" for line in textwrap.wrap(error['desc'], 60))
        output.append(        descWrapped)
        output.append(        f'void E{code}({error["inputs"]})\n')
        output.append(         '{\n')
        output.append(        f'    Error(Error::MsgType::ERROR, {error["location"]}, "{error["name"]}")\n')

        for loc, und, msgs in error['highlights']:
            output.append(    f'        .underline(Error::Underline({loc}, \'{und}\')\n')
            for ty, msg in msgs:
                output.append(f'            .{ty}({msg})\n')
            output.append(    '        )\n')

        output.append(        '    ;\n')

        output.append(         '}\n')

    return ''.join(output)
