#!/usr/bin/env python3
## @file genall.py
#  Generate all the code everywhere necessary in this project

import io
import astgen, kwgen, parsegen, instrgen, errgen

## A list of jobs to generate code for
jobs = [
    ('src/lex/lexer.cpp'                   , 'KWGEN START'               , 'KWGEN END'               , lambda: kwgen.trie.generate(doc='Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER')),

    ('src/ast/ast.cpp'                     , 'ASTCPP START'              , 'ASTCPP END'              , astgen.genASTDefs),
    ('include/ast/ast.h'                   , 'ASTHEADER START'           , 'ASTHEADER END'           , astgen.genASTDecls),

    ('include/ast/visitor.h'               , 'ASTFORWDECL BEGIN'         , 'ASTFORWDECL END'         , astgen.genASTForwDecls),
    ('include/ast/visitor.h'               , 'VISITCLASSES START'        , 'VISITCLASSES END'        , astgen.genVisitorClasses),

    ('src/ast/printvisitor.cpp'            , 'PRINTVISITOR START'        , 'PRINTVISITOR END'        , astgen.genPrintVisitorMethods),
    ('src/ast/dotvisitor.cpp'              , 'DOTVISITOR START'          , 'DOTVISITOR END'          , astgen.genDotVisitorMethods),

    ('src/parse/parserloop.cpp'            , 'PARSERLOOP START'          , 'PARSERLOOP END'          , parsegen.genLoop),
    ('src/parse/parserloop.cpp'            , 'GETGOTO START'             , 'GETGOTO END'             , parsegen.genGoto),
    ('src/parse/error.cpp'                 , 'PANIC MODE START'          , 'PANIC MODE END'          , parsegen.genPanicMode),
    ('src/parse/error.cpp'                 , 'SINGLETOK START'           , 'SINGLETOK END'           , parsegen.genSingleTok),

    ('include/ast/printvisitor.h'          , 'PRINTVISIT METHODS START'  , 'PRINTVISIT METHODS END'  , lambda: astgen.genVisitorMethods('all')),
    ('include/ast/dotvisitor.h'            , 'DOTVISIT METHODS START'    , 'DOTVISIT METHODS END'    , lambda: astgen.genVisitorMethods('all')),

    ('src/message/errors.cpp'              , 'LOCVISITOR METHODS START'  , 'LOCVISITOR METHODS END'  , lambda: astgen.genVisitorMethods('all')),
    ('src/message/errors.cpp'              , 'LOCVISITOR IMPL START'     , 'LOCVISITOR IMPL END'     , astgen.genLocVisit),

    ('src/codegen/codegenlocal.h'          , 'FORWDECL METHODS START'    , 'FORWDECL METHODS END'    , lambda: astgen.genVisitorMethods('DeclB', 'CUB')),
    ('src/codegen/codegenlocal.h'          , 'TYPEVISITOR METHODS START' , 'TYPEVISITOR METHODS END' , lambda: astgen.genVisitorMethods('TypeB')),
    ('src/codegen/codegenlocal.h'          , 'STMTCG METHODS START'      , 'STMTCG METHODS END'      , lambda: astgen.genVisitorMethods('StmtB', 'VStmtIB')),
    ('src/codegen/codegenlocal.h'          , 'EXPRCG METHODS START'      , 'EXPRCG METHODS END'      , lambda: astgen.genVisitorMethods('ExprB')),
    ('src/codegen/codegenlocal.h'          , 'PARAMVISITOR METHODS START', 'PARAMVISITOR METHODS END', lambda: astgen.genVisitorMethods('PListB')),
    ('src/codegen/codegenlocal.h'          , 'ARGSVISITOR METHODS START' , 'ARGSVISITOR METHODS END' , lambda: astgen.genVisitorMethods('ArgB')),
    ('include/codegen/codegen.h'           , 'CG METHODS START'          , 'CG METHODS END'          , lambda: astgen.genVisitorMethods('DeclB', 'CUB')),

    ('include/ir/instruction.h'            , 'INSTR CLASSES START'       , 'INSTR CLASSES END'       , instrgen.genDecls),
    ('src/ir/instruction.cpp'              , 'INSTR CPP START'           , 'INSTR CPP END'           , instrgen.genDefs),
    ('src/ir/cfgdotter.cpp'                , 'CFGDOTTER START'           , 'CFGDOTTER END'           , instrgen.genCFGDotter),

    ('include/ir/visitor.h'                , 'PURE INSTR VISIT START'    , 'PURE INSTR VISIT END'    , lambda: instrgen.genPureVirtualMethodDecls('Instruction')),
    ('include/ir/visitor.h'                , 'PURE BRANCH VISIT START'   , 'PURE BRANCH VISIT END'   , lambda: instrgen.genPureVirtualMethodDecls('Br')),
    ('include/ir/printer.h'                , 'PRINT VISIT INSTR START'   , 'PRINT VISIT INSTR END'   , lambda: instrgen.genMethodDecls('Instruction')),
    ('include/ir/printer.h'                , 'PRINT VISIT BRANCH START'  , 'PRINT VISIT BRANCH END'  , lambda: instrgen.genMethodDecls('Br')),
    ('include/lower/lowerer.h'             , 'LOWER VISIT INSTR START'   , 'LOWER VISIT INSTR END'   , lambda: instrgen.genMethodDecls('Instruction')),
    ('include/lower/lowerer.h'             , 'LOWER VISIT BRANCH START'  , 'LOWER VISIT BRANCH END'  , lambda: instrgen.genMethodDecls('Br')),
    ('include/ir/cfgdotter.h'              , 'CFG DOT METHOD START'      , 'CFG DOT METHOD END'      , lambda: instrgen.genMethodDecls('Br')),

    ('include/message/errmsgs.h'           , 'ERRH START'                , 'ERRH END'                , errgen.genH),
    ('src/message/errmsgs.cpp'             , 'ERRCPP START'              , 'ERRCPP END'              , errgen.genCpp),
]

for jobi, job in enumerate(jobs):
    jobfile, jobstart, jobend, jobfunc = job
    # print(f'Running job {jobi + 1}/{len(jobs)}: insert {jobfunc} to {jobfile}')
    with io.open(jobfile, 'r', encoding='utf-8') as f:
        ## The lines of the file
        backup = f.read()
        f.seek(0)
        flines = f.readlines()

    # delete old code
    ## Pattern to tell where to start generated code
    genStartPattern = jobstart
    ## Pattern to tell where to end generated code
    genEndPattern   = jobend

    if genStartPattern is not None:
        genStart = list(filter(lambda n: genStartPattern in flines[n], range(len(flines))))
        if len(genStart) != 1:
            print(f'"{genStartPattern}" found {len(genStart)} times in {jobfile}. Skipping {jobfile}')
            continue
        ## What line to start generated code
        genStart = genStart[0]
    else:
        genStart = -1

    if genEndPattern is not None:
        genEnd = list(filter(lambda n: genEndPattern in flines[n], range(len(flines))))
        if len(genEnd) != 1:
            print(f'"{genEndPattern}" found {len(genEnd)} times in {jobfile}. Skipping {jobfile}')
            continue
        ## What line to end generated code
        genEnd = genEnd[0]
    else:
        genEnd = len(flines)

    del flines[genStart + 1:genEnd]

    # run the function to generate the code
    ## Generated code to insert into the source file
    output = jobfunc()
    output = '\n// The following code was autogenerated - see the utils/ directory\n' + output + '// This code was autogenerated - see the utils/ directory\n\n'

    # take the output of the function and put it back into the file
    flines.insert(genStart + 1, output)
    finaloutput = ''.join(flines)

    if finaloutput == backup:
        continue

    try:
        with io.open(jobfile, 'w', encoding='utf-8') as f:
            f.write(finaloutput) # write in all the code
    except Exception as e:
        with io.open(jobfile, 'w', encoding='utf-8') as f:
            f.write(backup)

        print(f'In writing file {jobfile} with function {jobfunction}')
        raise
