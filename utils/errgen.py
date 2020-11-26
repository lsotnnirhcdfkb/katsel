#!/usr/bin/env python3

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
        'desc': 'The lexer found an unexpected character that could not begin a token',
        'inputs': 'Token const &tok',
        'highlights': [
            ('tok', UNDER0, ('error', 'unexpected character'))
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
        output.append(f'void E{code}({error["inputs"]});\n')
        output.append( '\n')

    for error in errors:
        output.append(f'#define ERR_{error["name"].upper().replace("-", "_")} E{str(error["code"]).zfill(PADAMT)}\n')

    return ''.join(output)

def genCpp():
    output = []

    return ''.join(output)
