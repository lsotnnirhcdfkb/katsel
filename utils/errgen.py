#!/usr/bin/env python3

import textwrap, yaml, os.path

try:
    loader = yaml.CLoader
except AttributeError:
    loader = yaml.Loader

PADAMT = 4

with open(os.path.join(os.path.dirname(__file__), 'errors.yml'), 'r') as f:
    errors = yaml.load(f.read(), Loader=loader)['errors']

def genH():
    output = []

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
        output.append(        f'    Error e = Error(Error::MsgType::ERROR, {error["location"]}, "E{code} ({error["name"]})");\n')

        for hii, hi in enumerate(error['highlights']):
            if len(hi) == 4:
                loc, und, msgs, cond = hi
            else:
                loc, und, msgs, cond = (*hi, None)
            if cond is not None:
                condty, rest = cond.split(' ', 1)
                if condty == 'dyncast':
                    cty, casttoname, castfrom = rest.split(',')
                    output.append(f'    {cty} *{casttoname};\n')
                    output.append(f'    if (({casttoname} = dynamic_cast<{cty}*>({castfrom})))\n')
                elif condty == 'if':
                    output.append(f'    if ({rest})\n')
                elif condty == 'isdeclared':
                    asdeclaredvar = rest.replace('.', '_').replace('-', '_').replace('>', '_') + '_asdeclared'
                    output.append(f'    IR::DeclaredValue *{asdeclaredvar};\n')
                    output.append(f'    if (({asdeclaredvar} = dynamic_cast<IR::DeclaredValue*>({rest})) && {asdeclaredvar}->defAST())\n')
                else:
                    raise Exception(f'invalid condition {condty}')

            output.append(    f'    e.underline(Error::Underline({loc}, \'{und}\')\n')
            for ty, msg in msgs:
                output.append(f'        .{ty}({msg})\n')
            output.append(    '    );\n')

        if 'extra' in error:
            output.append(error['extra'])

        output.append(         '    e.report();\n')

        output.append(         '}\n')

    return ''.join(output)

def genFriends():
    output = []
    for error in errors:
        n = 'E' + str(error['code']).zfill(PADAMT)
        output.append(f'    friend void {n}({error["inputs"]});\n')

    return ''.join(output)
