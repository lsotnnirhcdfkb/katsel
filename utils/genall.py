#!/usr/bin/env python3
## @file genall.py
#  Generate all the code everywhere necessary in this project

import io
import astgen, kwgen, parsegen

## A list of jobs to generate code for
jobs = [
    ('src/lex/lexer.cpp'                   , 'KWGEN START'               , 'KWGEN END'               , lambda: kwgen.trie.generate(doc='Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER')),
    ('src/parse/ast.cpp'                   , 'ASTCPP START'              , 'ASTCPP END'              , astgen.genASTDefs),
    ('include/parse/ast.h'                 , 'ASTHEADER START'           , 'ASTHEADER END'           , astgen.genASTDecls),
    ('include/visit/visitor.h'             , 'ASTFORWDECL BEGIN'         , 'ASTFORWDECL END'         , astgen.genASTForwDecls),
    ('include/visit/visitor.h'             , 'VISITMETHODS START'        , 'VISITMETHODS END'        , lambda: astgen.genVisitorMethods(True)),
    ('src/visit/printvisitor.cpp'          , 'PRINTVISITOR START'        , 'PRINTVISITOR END'        , astgen.genPrintVisitorMethods),
    ('src/visit/dotvisitor.cpp'            , 'DOTVISITOR START'          , 'DOTVISITOR END'          , astgen.genDotVisitorMethods),
    ('src/parse/parser.cpp'                , 'PARSERLOOP START'          , 'PARSERLOOP END'          , parsegen.genLoop),
    ('src/parse/parser.cpp'                , 'GETGOTO START'             , 'GETGOTO END'             , parsegen.genGoto),
    ('include/visit/printvisitor.h'        , 'PRINTVISITOR METHODS START', 'PRINTVISITOR METHODS END', astgen.genVisitorMethods),
    ('include/visit/dotvisitor.h'          , 'DOTVISITOR METHODS START'  , 'DOTVISITOR METHODS END'  , astgen.genVisitorMethods),
    ('src/message/errors.cpp'              , 'LOCVISITOR METHODS START'  , 'LOCVISITOR METHODS END'  , astgen.genVisitorMethods),
    ('include/codegen/codegen.h'           , 'CODEGEN METHODS START'     , 'CODEGEN METHODS END'     , astgen.genVisitorMethods),
    ('include/replicate/replicatevisitor.h', 'REPLICATE METHODS START'   , 'REPLICATE METHODS END'   , astgen.genVisitorMethods),
    ('include/codegen/globalsassembler.h'  , 'GLOBALS METHODS START'     , 'GLOBALS METHODS END'     , astgen.genVisitorMethods),
    ('src/message/errors.cpp'              , 'LOCVISITOR IMPL START'     , 'LOCVISITOR IMPL END'     , astgen.genLocVisit),
]

for jobi, job in enumerate(jobs):
    # print(f'Running job {jobi + 1}/{len(jobs)}: insert {job[3]} to {job[0]}')
    with io.open(job[0], 'r', encoding='utf-8') as f:
        ## The lines of the file
        backup = f.read()
        f.seek(0)
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
    output = '\n// The following code was autogenerated - see the utils/ directory\n' + output + '// This code was autogenerated - see the utils/ directory\n\n'

    # take the output of the function and put it back into the file
    flines.insert(genStart + 1, output)
    finaloutput = ''.join(flines)

    if finaloutput == backup:
        print(f'Output is identical for {job[0]}. Skipping {job[0]}')
        continue

    try:
        with io.open(job[0], 'w', encoding='utf-8') as f:
            f.write(finaloutput) # write in all the code
    except Exception as e:
        with io.open(job[0], 'w', encoding='utf-8') as f:
            f.write(backup)

        print(f'In writing file {job[0]} with function {job[3]}')
        raise
