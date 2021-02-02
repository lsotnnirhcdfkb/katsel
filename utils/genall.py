#!/usr/bin/env python3
#  Generate all the code everywhere necessary in this project

import io, re
import astgen, kwgen, parsegen, instrgen, errgen

jobs = [
    ('src/lex/lexer.cpp'           , 'KWMATCH'               , kwgen.trie.generate),

    ('src/ast/ast.cpp'             , 'ASTCPP'                , astgen.gen_ast_defs),
    ('include/ast/ast.h'           , 'ASTHEADER'             , astgen.gen_ast_decls),
    ('include/ast/astfwd.h'        , 'ASTFWD'                , astgen.gen_ast_fwd),
    ('include/ast/visitor.h'       , 'ASTVISITH'             , astgen.gen_visitor_decls),

    ('src/ast/printvisitor.cpp'    , 'PRINTVISITOR'          , astgen.gen_print_visitor_methods),

    ('src/parse/parserlocal.h'     , 'NONTERM ENUM'          , parsegen.gen_non_term_enum),
    ('src/parse/parserlocal.h'     , 'PARSESTACK ITEM TYPES' , parsegen.gen_non_term_types),
    ('src/parse/parserloop.cpp'    , 'PARSERLOOP'            , parsegen.gen_loop),
    ('src/parse/parserloop.cpp'    , 'GETGOTO'               , parsegen.gen_goto),

    ('include/ast/printvisitor.h'  , 'PRINTVISIT METHODS'    , lambda: astgen.gen_visitor_methods('all')),
    ('include/ast/printvisitor.h'  , 'PRINTVISIT INHERIT'    , astgen.gen_visitor_inherit_all),

    ('src/codegen/codegenlocal.h'  , 'TYPEVISITOR METHODS'   , lambda: astgen.gen_visitor_methods('Type')),
    ('src/codegen/codegenlocal.h'  , 'STMTCG METHODS'        , lambda: astgen.gen_visitor_methods('Stmt', 'VStmtIB')),
    ('src/codegen/codegenlocal.h'  , 'EXPRCG METHODS'        , lambda: astgen.gen_visitor_methods('Expr')),
    ('src/codegen/codegenlocal.h'  , 'PARAMVISITOR METHODS'  , lambda: astgen.gen_visitor_methods('ParamB')),
    ('src/codegen/codegenlocal.h'  , 'ARGSVISITOR METHODS'   , lambda: astgen.gen_visitor_methods('ArgB')),
    ('src/codegen/codegenlocal.h'  , 'PATH VISITOR'          , lambda: astgen.gen_visitor_methods('PathB')),
    ('src/codegen/codegenlocal.h'  , 'IMPL CG METHODS'       , lambda: astgen.gen_visitor_methods('ImplMember')),
    ('src/codegen/codegen.cpp'     , 'MAINCG METHODS'        , lambda: astgen.gen_visitor_methods('Decl', 'CUB')),

    ('include/ir/instruction.h'    , 'INSTR CLASSES'         , instrgen.gen_decls),
    ('include/ir/instructionfwd.h' , 'INSTR FWD'             , instrgen.gen_fwd),
    ('src/ir/instruction.cpp'      , 'INSTR CPP'             , instrgen.gen_defs),

    ('include/ir/visitor.h'        , 'PURE INSTR VISIT'      , lambda: instrgen.gen_pure_method_decls('Instruction')),
    ('include/ir/visitor.h'        , 'PURE BRANCH VISIT'     , lambda: instrgen.gen_pure_method_decls('Br')),
    ('include/lower/lowerer.h'     , 'LOWER VISIT INSTR'     , lambda: instrgen.gen_method_decls('Instruction')),
    ('include/lower/lowerer.h'     , 'LOWER VISIT BRANCH'    , lambda: instrgen.gen_method_decls('Br')),

    ('include/message/errmsgs.h'   , 'ERRH'                  , errgen.gen_h),
    ('src/message/errmsgs.cpp'     , 'ERRCPP'                , errgen.gen_cpp),
]

skipped = 0

LINE_START_PATTERN = re.compile(r'^(?!(\s*\n))', re.MULTILINE)

for job_i, job in enumerate(jobs):
    job_file, job_marker, job_func = job
    # print(f'Running job {job_i + 1}/{len(jobs)}: insert {job_func} to {job_file}')

    with io.open(job_file, 'r', encoding='utf-8') as f:
        job_file_lines = f.readlines()
        BACKUP = ''.join(job_file_lines)

    def find_marker(marker):
        reg = re.compile(fr'(\s*)// {marker}')
        matches = []
        for line_num, line in enumerate(job_file_lines):
            if match := reg.match(line):
                matches.append((line_num, match))

        if len(matches) != 1:
            print(f"error: marker '{marker}' matched {len(matches)} times")
            return (None, None)
        else:
            return matches[0]

    # delete old code
    gen_start_line, gen_start_match = find_marker(job_marker + ' START')
    gen_end_line  , gen_end_match   = find_marker(job_marker + ' END')

    if gen_start_line is None or gen_end_line is None:
        continue

    indent_str = gen_start_match.group(1)

    # run the function to generate the code
    output = LINE_START_PATTERN.sub(indent_str, job_func().rstrip('\n'))
    if not output.endswith('\n'): output += '\n'

    # take the output of the function and put it back into the file
    del job_file_lines[gen_start_line + 1 : gen_end_line]
    job_file_lines.insert(gen_start_line + 1, output)
    FINAL_OUTPUT = ''.join(job_file_lines)

    if FINAL_OUTPUT == BACKUP:
        skipped += 1
        continue

    try:
        with io.open(job_file, 'w', encoding='utf-8') as f:
            f.write(FINAL_OUTPUT) # write in all the code
    except Exception as e:
        with io.open(job_file, 'w', encoding='utf-8') as f:
            f.write(BACKUP)

        print(f'In writing file {job_file} with function {job_func}')
        raise

print(f'generation: {len(jobs)} jobs, {skipped} skipped')
