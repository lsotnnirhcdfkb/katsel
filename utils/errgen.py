#!/usr/bin/env python3

import textwrap, os.path, yaml

try:
    Loader = yaml.CLoader
except AttributeError:
    Loader = yaml.Loader

PADAMT = 4

with open(os.path.join(os.path.dirname(__file__), 'errors.yml'), 'r') as f:
    thingy = yaml.load(f.read(), Loader=Loader)
    errors = thingy['errors']
    warnings = thingy['warnings']

def gen_h():
    output = []

    output.append(     '\n')

    for error in errors:
        code = str(error['code']).zfill(PADAMT)
        output.append(f'// E{code} - {error["name"]}\n')
        output.append(f'#define ERR_{error["name"].upper().replace("-", "_")} E{code}\n')
        output.append(f'void E{code}({error["inputs"]});\n')
        output.append( '\n')

    for warning in warnings:
        code = str(warning['code']).zfill(PADAMT)
        output.append(f'// W{code} - W{warning["name"]}\n')
        output.append(f'#define WARN_{warning["name"].upper().replace("-", "_")} W{code}\n')
        output.append(f'void W{code}({warning["inputs"]});\n')
        output.append( '\n')

    return ''.join(output)

def gen_cpp():
    def gen_message(code, name, msgtype, location, description, inputs, highlights, extra):
        output.append(        f'// {code} - {name}\n')
        desc_wrapped = "".join("// | " + line + "\n" for line in textwrap.wrap(description, 60))
        output.append(        desc_wrapped)
        output.append(        f'void {code}({inputs})\n')
        output.append(         '{\n')
        output.append(        f'    Error e = Error(Error::MsgType::{msgtype}, {location}, "{code} - {name}");\n')

        for hi in highlights:
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
                else:
                    raise Exception(f'invalid condition {condty}')

            output.append(    f'    e.underline(Error::Underline({loc}, \'{und}\')\n')
            for msgtype, msg in msgs:
                output.append(f'        .{msgtype}({msg})\n')
            output.append(    '    );\n')

        if extra is not None:
            output.append(extra)

        output.append(         '    e.report();\n')

        output.append(         '}\n\n')

    output = []

    for error in errors:
        code = 'E' + str(error['code']).zfill(PADAMT)
        name = error['name']
        msgtype = 'ERROR'
        location = error['location']
        description = error['desc']
        inputs = error['inputs']
        highlights = error['highlights']
        extra = error['extra'] if 'extra' in error else None
        gen_message(code, name, msgtype, location, description, inputs, highlights, extra)

    del error
    for warning in warnings:
        code = 'W' + str(warning['code']).zfill(PADAMT)
        name = 'W' + warning['name']
        msgtype = 'WARNING'
        location = warning['location']
        description = warning['desc']
        inputs = warning['inputs']
        highlights = warning['highlights']
        extra = warning['extra'] if 'extra' in warning else None
        gen_message(code, name, msgtype, location, description, inputs, highlights, extra)

    return ''.join(output)
