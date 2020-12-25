#!/usr/bin/env python3

import textwrap, os.path, yaml

try:
    Loader = yaml.CLoader
except AttributeError:
    Loader = yaml.Loader

PADAMT = 4

with open(os.path.join(os.path.dirname(__file__), 'errors.yml'), 'r') as f:
    errors = yaml.load(f.read(), Loader=Loader)['errors']

def gen_h():
    output = []

    output.append(     '\n')

    for error in errors:
        code = str(error['code']).zfill(PADAMT)
        output.append(f'// E{code} - {error["name"]}\n')
        output.append(f'#define ERR_{error["name"].upper().replace("-", "_")} E{code}\n')
        output.append(f'void E{code}({error["inputs"]});\n')
        output.append( '\n')

    return ''.join(output)

def gen_cpp():
    output = []

    for error in errors:
        code = str(error['code']).zfill(PADAMT)
        output.append(        f'// E{code} - {error["name"]}\n')
        desc_wrapped = "".join("// | " + line + "\n" for line in textwrap.wrap(error['desc'], 60))
        output.append(        desc_wrapped)
        output.append(        f'void E{code}({error["inputs"]})\n')
        output.append(         '{\n')
        output.append(        f'    Error e = Error(Error::MsgType::ERROR, {error["location"]}, "E{code} ({error["name"]})");\n')

        for hi in error['highlights']:
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

        if 'extra' in error:
            output.append(error['extra'])

        output.append(         '    e.report();\n')

        output.append(         '}\n\n')

    return ''.join(output)
