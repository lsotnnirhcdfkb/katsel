#!/usr/bin/env python3
import io
import astgen, kwgen

jobs = [
    ('src/lexer.cpp'           , 'KWGEN BEGIN HERE'                     , 'KWGEN END HERE'                         , kwgen.trie.generate),
    ('src/ast.cpp'             , None                                   , None                                     , astgen.astCppFile),
    ('include/ast.h'           , 'GENASTHEADER START'                   , 'GENASTHEADER END'                       , astgen.astHFile),
    ('include/visitor.h'       , 'GENFORWARDDECL START'                 , 'GENFORWARDDECL END'                     , astgen.forwardDecl),
    ('src/visitor.cpp'         , 'BLANKGEN START'                       , 'BLANKGEN END'                           , astgen.blankVisitorDefinitions),
    ('include/visitor.h'       , 'GENVISITORMETHODBASE START'           , 'GENVISITORMETHODBASE END'               , lambda: astgen.visitASTMethods(True)),
    ('include/visitor.h'       , 'GENVISITORMETHOD1 START'              , 'GENVISITORMETHOD1 END'                  , lambda: astgen.visitASTMethods(False)),
    ('include/visitor.h'       , 'GENVISITORMETHOD2 START'              , 'GENVISITORMETHOD2 END'                  , lambda: astgen.visitASTMethods(False)),
    ('include/llvmgenvisitor.h', 'GENVISITORMETHOD3 START'              , 'GENVISITORMETHOD3 END'                  , lambda: astgen.visitASTMethods(False)),
]

for jobi, job in enumerate(jobs):
    # print(f'Running job {jobi + 1}/{len(jobs)}: insert {job[3]} to {job[0]}')
    with io.open(job[0], 'r', encoding='utf-8') as f:
        flines = f.readlines()

    # delete old code
    genStartPattern = job[1]
    genEndPattern   = job[2]

    if genStartPattern is not None:
        genStart = list(filter(lambda n: genStartPattern in flines[n], range(len(flines))))
        if len(genStart) != 1:
            print(f'"{genStartPattern}" found {len(genStart)} times in {job[0]}. Skipping {job[0]}')
            continue
        genStart = genStart[0]
    else:
        genStart = -1

    if genEndPattern is not None:
        genEnd = list(filter(lambda n: genEndPattern in flines[n], range(len(flines))))
        if len(genEnd) != 1:
            print(f'"{genEndPattern}" found {len(genEnd)} times in {job[0]}. Skipping {job[0]}')
            continue
        genEnd = genEnd[0]
    else:
        genEnd = len(flines)

    del flines[genStart + 1:genEnd]

    # run the function to generate the code
    output = job[3]()

    # take the output of the function and put it back into the file
    flines.insert(genStart + 1, output)

    with io.open(job[0], 'w', encoding='utf-8') as f:
        f.write(''.join(flines)) # write in all the code
