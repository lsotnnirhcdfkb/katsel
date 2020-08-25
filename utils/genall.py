#!/usr/bin/env python3
import subprocess
import io

jobs = [
    ('src/lexer.cpp', 'KWGEN BEGIN HERE', 'KWGEN END HERE', 'utils/kwgen.py' , '-c'),
    ('src/ast.cpp'  , None              , None            , 'utils/astgen.py', '-c'),
    ('include/ast.h', None              , None            , 'utils/astgen.py', '-e'),
]

for job in jobs:
    with io.open(job[0], 'r', encoding='utf-8') as f:
        flines = f.readlines()

    # delete old code
    genStartPattern = job[1]
    genEndPattern   = job[2]

    if genStartPattern is not None:
        genStart = list(filter(lambda n: genStartPattern in flines[n], range(len(flines))))
        if len(genStart) > 1:
            print(f'"{genStartPattern}" found {len(genStart)} times in {job[0]}. Skipping {job[0]}')
            continue
        genStart = genStart[0]
    else:
        genStart = -1

    if genEndPattern is not None:
        genEnd = list(filter(lambda n: genEndPattern in flines[n], range(len(flines))))
        if len(genEnd) > 1:
            print(f'"{genEndPattern}" found {len(genEnd)} times in {job[0]}. Skipping {job[0]}')
            continue
        genEnd = genEnd[0]
    else:
        genEnd = len(flines)

    del flines[genStart + 1:genEnd]

    # create command to generate new code
    # by combining script filename and flags
    command = f'{job[3]} {job[4]}'

    # run the created command
    process = subprocess.run(command, capture_output=True, shell=True)
    if process.returncode != 0:
        print(f'Command "{command}" failed with exit code {process.returncode}')
        continue

    # take the output of the command and put it back into the file
    flines.insert(genStart + 1, process.stdout.decode('utf-8'))

    with io.open(job[0], 'w', encoding='utf-8') as f:
        f.write(''.join(flines)) # write in all the code
