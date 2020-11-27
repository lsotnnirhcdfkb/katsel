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
        output.append(        f'    Error(Error::MsgType::ERROR, {error["location"]}, "E{code} ({error["name"]})")\n')

        for loc, und, msgs in error['highlights']:
            output.append(    f'        .underline(Error::Underline({loc}, \'{und}\')\n')
            for ty, msg in msgs:
                output.append(f'            .{ty}({msg})\n')
            output.append(    '        )\n')

        output.append(        '    ;\n')

        output.append(         '}\n')

    return ''.join(output)

def genFriends():
    output = []
    for error in errors:
        n = 'E' + str(error['code']).zfill(PADAMT)
        output.append(f'    friend void {n}({error["inputs"]});\n')

    return ''.join(output)
