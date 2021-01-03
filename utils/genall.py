#!/usr/bin/env python3
## @file genall.py
#  Generate all the code everywhere necessary in this project

import io
import astgen, kwgen, parsegen, instrgen, errgen

## A list of jobs to generate code for
jobs = [
    ('src/lex/lexer.cpp'                   , 'KWGEN START'               , 'KWGEN END'               , lambda: kwgen.trie.generate(doc='Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER')),

    ('src/ast/ast.cpp'                     , 'ASTCPP START'              , 'ASTCPP END'              , astgen.gen_ast_defs),
    ('include/ast/ast.h'                   , 'ASTHEADER START'           , 'ASTHEADER END'           , astgen.gen_ast_decls),

    ('src/ast/printvisitor.cpp'            , 'PRINTVISITOR START'        , 'PRINTVISITOR END'        , astgen.gen_print_visitor_methods),

    ('src/parse/parsestack.h'              , 'NONTERM ENUM START'        , 'NONTERM ENUM END'        , parsegen.gen_non_term_enum),
    ('src/parse/parserloop.cpp'            , 'PARSERLOOP START'          , 'PARSERLOOP END'          , parsegen.gen_loop),
    ('src/parse/parserloop.cpp'            , 'GETGOTO START'             , 'GETGOTO END'             , parsegen.gen_goto),
    ('src/parse/error.cpp'                 , 'PANIC MODE START'          , 'PANIC MODE END'          , parsegen.gen_panic_mode),
    ('src/parse/error.cpp'                 , 'SINGLETOK START'           , 'SINGLETOK END'           , parsegen.gen_single_tok),

    ('include/ast/printvisitor.h'          , 'PRINTVISIT METHODS START'  , 'PRINTVISIT METHODS END'  , lambda: astgen.gen_visitor_methods('all')),
    ('include/ast/printvisitor.h'          , 'PRINTVISIT INHERIT START'  , 'PRINTVISIT INHERIT END'  , astgen.gen_visitor_inherit_all),

    ('src/codegen/codegenlocal.h'          , 'FORWDECL METHODS START'    , 'FORWDECL METHODS END'    , lambda: astgen.gen_visitor_methods('Decl', 'CUB')),
    ('src/codegen/codegenlocal.h'          , 'DECLARATOR METHODS START'  , 'DECLARATOR METHODS END'  , lambda: astgen.gen_visitor_methods('Decl', 'CUB')),
    ('src/codegen/codegenlocal.h'          , 'TYPEVISITOR METHODS START' , 'TYPEVISITOR METHODS END' , lambda: astgen.gen_visitor_methods('Type')),
    ('src/codegen/codegenlocal.h'          , 'STMTCG METHODS START'      , 'STMTCG METHODS END'      , lambda: astgen.gen_visitor_methods('Stmt', 'VStmtIB')),
    ('src/codegen/codegenlocal.h'          , 'EXPRCG METHODS START'      , 'EXPRCG METHODS END'      , lambda: astgen.gen_visitor_methods('Expr', 'ImplRetB')),
    ('src/codegen/codegenlocal.h'          , 'PARAMVISITOR METHODS START', 'PARAMVISITOR METHODS END', lambda: astgen.gen_visitor_methods('ParamB')),
    ('src/codegen/codegenlocal.h'          , 'ARGSVISITOR METHODS START' , 'ARGSVISITOR METHODS END' , lambda: astgen.gen_visitor_methods('ArgB')),
    ('src/codegen/codegenlocal.h'          , 'PATH VISITOR START'        , 'PATH VISITOR END'        , lambda: astgen.gen_visitor_methods('PathB')),
    ('include/codegen/codegen.h'           , 'CG METHODS START'          , 'CG METHODS END'          , lambda: astgen.gen_visitor_methods('Decl', 'CUB')),

    ('include/ir/instruction.h'            , 'INSTR CLASSES START'       , 'INSTR CLASSES END'       , instrgen.gen_decls),
    ('src/ir/instruction.cpp'              , 'INSTR CPP START'           , 'INSTR CPP END'           , instrgen.gen_defs),
    ('src/ir/cfgdotter.cpp'                , 'CFGDOTTER START'           , 'CFGDOTTER END'           , instrgen.gen_cfg_dotter),

    ('include/ir/visitor.h'                , 'PURE INSTR VISIT START'    , 'PURE INSTR VISIT END'    , lambda: instrgen.gen_pure_method_decls('Instruction')),
    ('include/ir/visitor.h'                , 'PURE BRANCH VISIT START'   , 'PURE BRANCH VISIT END'   , lambda: instrgen.gen_pure_method_decls('Br')),
    ('include/ir/printer.h'                , 'PRINT VISIT INSTR START'   , 'PRINT VISIT INSTR END'   , lambda: instrgen.gen_method_decls('Instruction')),
    ('include/ir/printer.h'                , 'PRINT VISIT BRANCH START'  , 'PRINT VISIT BRANCH END'  , lambda: instrgen.gen_method_decls('Br')),
    ('include/lower/lowerer.h'             , 'LOWER VISIT INSTR START'   , 'LOWER VISIT INSTR END'   , lambda: instrgen.gen_method_decls('Instruction')),
    ('include/lower/lowerer.h'             , 'LOWER VISIT BRANCH START'  , 'LOWER VISIT BRANCH END'  , lambda: instrgen.gen_method_decls('Br')),
    ('include/ir/cfgdotter.h'              , 'CFG DOT METHOD START'      , 'CFG DOT METHOD END'      , lambda: instrgen.gen_method_decls('Br')),

    ('include/message/errmsgs.h'           , 'ERRH START'                , 'ERRH END'                , errgen.gen_h),
    ('src/message/errmsgs.cpp'             , 'ERRCPP START'              , 'ERRCPP END'              , errgen.gen_cpp),
]

for jobi, job in enumerate(jobs):
    jobfile, jobstart, jobend, jobfunc = job
    # print(f'Running job {jobi + 1}/{len(jobs)}: insert {jobfunc} to {jobfile}')
    with io.open(jobfile, 'r', encoding='utf-8') as f:
        ## The lines of the file
        BACKUP = f.read()
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
        GEN_START_LINE = genStart[0]
    else:
        GEN_START_LINE = -1

    if genEndPattern is not None:
        genEnd = list(filter(lambda n: genEndPattern in flines[n], range(len(flines))))
        if len(genEnd) != 1:
            print(f'"{genEndPattern}" found {len(genEnd)} times in {jobfile}. Skipping {jobfile}')
            continue
        ## What line to end generated code
        GEN_END_LINE = genEnd[0]
    else:
        GEN_END_LINE = len(flines)

    del flines[GEN_START_LINE + 1:GEN_END_LINE]

    # run the function to generate the code
    ## Generated code to insert into the source file
    output = jobfunc()
    output = '// The following code was autogenerated - see the utils/ directory\n' + output + '// This code was autogenerated - see the utils/ directory\n'

    # take the output of the function and put it back into the file
    flines.insert(GEN_START_LINE + 1, output)
    FINAL_OUTPUT = ''.join(flines)

    if FINAL_OUTPUT == BACKUP:
        continue

    try:
        with io.open(jobfile, 'w', encoding='utf-8') as f:
            f.write(FINAL_OUTPUT) # write in all the code
    except Exception as e:
        with io.open(jobfile, 'w', encoding='utf-8') as f:
            f.write(BACKUP)

        print(f'In writing file {jobfile} with function {jobfunc}')
        raise
