#!/usr/bin/env python3
## @file genall.py
#  Generate all the code everywhere necessary in this project

import io
import astgen, kwgen

## A list of jobs to generate code for
jobs = [
    ('src/lexer.cpp'           , 'KWGEN START'               , 'KWGEN END'               , lambda: kwgen.trie.generate(doc='Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER')),
    ('src/ast.cpp'             , None                        , None                      , astgen.astCppFile),
    ('include/ast.h'           , 'GENASTHEADER START'        , 'GENASTHEADER END'        , astgen.astHFile),
    ('include/visitor.h'       , 'GENFORWARDDECL START'      , 'GENFORWARDDECL END'      , astgen.forwardDecl),
    ('src/visitor.cpp'         , 'BLANKGEN START'            , 'BLANKGEN END'            , astgen.blankVisitorDefinitions),
    ('include/visitor.h'       , 'GENVISITORMETHODBASE START', 'GENVISITORMETHODBASE END', lambda: astgen.visitASTMethods(True, doc='Visit a(n) {}')),
    ('include/visitor.h'       , 'GENPRINTVISITMETHOD START' , 'GENPRINTVISITMETHOD END' , lambda: astgen.visitASTMethods(False, doc='Visit the {} and print it in a readable form')),
    ('include/visitor.h'       , 'GENBLANKVISITMETHOD START' , 'GENBLANKVISITMETHOD END' , lambda: astgen.visitASTMethods(False, doc='Visit the {} and do nothing')),
    ('include/compiler.h'      , 'GENLLVMVISITMETHOD START'  , 'GENLLVMVISITMETHOD END'  , lambda: astgen.visitASTMethods(False, doc='Visit the {} and compile it to LLVM IR/object code', indent=8)),
]

for jobi, job in enumerate(jobs):
    # print(f'Running job {jobi + 1}/{len(jobs)}: insert {job[3]} to {job[0]}')
    with io.open(job[0], 'r', encoding='utf-8') as f:
        ## The lines of the file
        flines = f.readlines()

    # delete old code
    ## Pattern to tell where to start generated code
    genStartPattern = job[1]
    ## Pattern to tell where to end generated code
    genEndPattern   = job[2]

    if genStartPattern is not None:
        genStart = list(filter(lambda n: genStartPattern in flines[n], range(len(flines))))
        if len(genStart) != 1:
            print(f'"{genStartPattern}" found {len(genStart)} times in {job[0]}. Skipping {job[0]}')
            continue
        ## What line to start generated code
        genStart = genStart[0]
    else:
        genStart = -1

    if genEndPattern is not None:
        genEnd = list(filter(lambda n: genEndPattern in flines[n], range(len(flines))))
        if len(genEnd) != 1:
            print(f'"{genEndPattern}" found {len(genEnd)} times in {job[0]}. Skipping {job[0]}')
            continue
        ## What line to end generated code
        genEnd = genEnd[0]
    else:
        genEnd = len(flines)

    del flines[genStart + 1:genEnd]

    # run the function to generate the code
    ## Generated code to insert into the source file
    output = job[3]()

    # take the output of the function and put it back into the file
    flines.insert(genStart + 1, output)

    with io.open(job[0], 'w', encoding='utf-8') as f:
        f.write(''.join(flines)) # write in all the code
