#!/usr/bin/env python3
import subprocess
import io

jobs = [
    ('src/lexer.cpp'           , 'KWGEN BEGIN HERE'                     , 'KWGEN END HERE'                         , 'utils/kwgen.py'               , '-c'),
    ('src/ast.cpp'             , None                                   , None                                     , 'utils/astgen.py'              , '--astsource'),
    ('include/ast.h'           , 'GENASTHEADER START'                   , 'GENASTHEADER END'                       , 'utils/astgen.py'              , '--astheader'),
    ('include/visitor.h'       , 'GENFORWARDDECL START'                 , 'GENFORWARDDECL END'                     , 'utils/astgen.py'              , '--forwarddecl'),
    ('src/visitor.cpp'         , 'BLANKGEN START'                       , 'BLANKGEN END'                           , 'utils/astgen.py'              , '--blankvisitor'),
    ('include/visitor.h'       , 'GENVISITORMETHODBASE START'           , 'GENVISITORMETHODBASE END'               , 'utils/astgen.py'              , '--visitorbasemethods'),
    ('include/visitor.h'       , 'GENVISITORMETHOD1 START'              , 'GENVISITORMETHOD1 END'                  , 'utils/astgen.py'              , '--visitormethods'),
    ('include/visitor.h'       , 'GENVISITORMETHOD2 START'              , 'GENVISITORMETHOD2 END'                  , 'utils/astgen.py'              , '--visitormethods'),
    ('include/llvmgenvisitor.h', 'GENVISITORMETHOD3 START'              , 'GENVISITORMETHOD3 END'                  , 'utils/astgen.py'              , '--visitormethods'),
]

for jobi, job in enumerate(jobs):
    print(f'Running job {jobi}/{len(jobs)}')
    with io.open(job[0], 'r', encoding='utf-8') as f:
        flines = f.readlines()
        print(f'Loaded file {job[0]}');

    # delete old code
    genStartPattern = job[1]
    genEndPattern   = job[2]

    if genStartPattern is not None:
        genStart = list(filter(lambda n: genStartPattern in flines[n], range(len(flines))))
        if len(genStart) != 1:
            print(f'"{genStartPattern}" found {len(genStart)} times in {job[0]}. Skipping {job[0]}')
            continue
        genStart = genStart[0]
        print(f'Found "{genStartPattern}" on line {genStart + 1}')
    else:
        genStart = -1

    if genEndPattern is not None:
        genEnd = list(filter(lambda n: genEndPattern in flines[n], range(len(flines))))
        if len(genEnd) != 1:
            print(f'"{genEndPattern}" found {len(genEnd)} times in {job[0]}. Skipping {job[0]}')
            continue
        genEnd = genEnd[0]
        print(f'Found "{genEndPattern}" on line {genEnd + 1}')
    else:
        genEnd = len(flines)

    print(f'Deleted {genEnd - (genStart + 1)} lines')
    del flines[genStart + 1:genEnd]

    # create command to generate new code
    # by combining script filename and flags
    command = f'{job[3]} {job[4]}'

    # run the created command
    process = subprocess.run(command, capture_output=True, shell=True)
    if process.returncode != 0:
        print(f'Command "{command}" failed with exit code {process.returncode}')
        continue

    print(f'Command "{command}" ran successfully')

    # take the output of the command and put it back into the file
    generated = process.stdout.decode('utf-8')
    flines.insert(genStart + 1, generated)

    print('Inserted ' + str(generated.count("\n")) + ' lines into source')

    with io.open(job[0], 'w', encoding='utf-8') as f:
        f.write(''.join(flines)) # write in all the code
        print(f'Wrote file {job[0]}');
    print()
