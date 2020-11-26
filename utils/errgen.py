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
