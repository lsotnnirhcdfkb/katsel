import os, sys, subprocess, glob, json, time, re, colorama

colorama.init()

if len(sys.argv) == 3:
    EXECLOC = os.path.abspath(sys.argv[1])
    CPPCOMP = os.path.abspath(sys.argv[2])
else:
    print(f'Usage: {sys.argv[0]} <katselc path> <c++ compiler>')
    sys.exit(1)

def fail(testfile, msg):
    global nfailed
    nfailed += 1
    print( '\033[0;1;31mfailed\033[0m')
    print(f'\t- {msg}')

    tmplog = f'/tmp/log_{os.path.basename(testfile)}.txt'

    print(f'\t- log written to \033[36m{tmplog}\033[0m')

    with open(tmplog, 'w') as f:
        f.write(f'failed with {msg}\n')
        f.write(str(outputs))

def mark_failed(msg):
    global failed, failmsg
    if not failed:
        failed = True
        failmsg = msg

def pass_test(_):
    global npassed
    npassed += 1
    print('\033[0;1;32mpassed\033[0m', end='')

def set_outputs(outputs, category, process):
    outputs[category]           = {}
    outputs[category]['stdout'] = process.stdout.decode('utf-8')
    outputs[category]['stderr'] = process.stderr.decode('utf-8')

TESTDIR = os.path.abspath(os.path.dirname(__file__))

TESTS = glob.glob(f'{TESTDIR}/**/*.ksl', recursive=True)
NTESTS = len(TESTS)
NTESTWIDTH = len(str(NTESTS))
LONGESTNAME = max(map(len, TESTS))

PRINTDEFF = 'printdef.cpp'

with open(PRINTDEFF, 'w') as f:
    f.write(r'''
#ifdef __cplusplus
#include <cstdio>
#include <cstdint>
#else
#include <stdio.h>
#include <stdint.h>
#endif

#ifdef __cplusplus
extern "C"
#endif
void printunsigned(uint32_t i) {
    printf("%u\n", i);
}

#ifdef __cplusplus
extern "C"
#endif
void printsigned(int32_t i) {
    printf("%d\n", i);
}

#ifdef __cplusplus
extern "C"
#endif
void printchar(uint32_t i) {
    printf("%c\n", i);
}

#ifdef __cplusplus
extern "C"
#endif
void printfloat(float i) {
    printf("%.2f\n", i);
}

#ifdef __cplusplus
extern "C"
#endif
void printdouble(double i) {
    printf("%.2f\n", i);
}
''')

EXPECT_COMP_ERR_REGEX = re.compile(r'expect compile error \(([\w-]+)\)')
EXPECT_COMP_WARN_REGEX = re.compile(r'expect compile warning \(([\w-]+)\)')
EXPECT_RUN_ERR_REGEX = re.compile(r'expect runtime error')
EXPECT_PRINT_REGEX = re.compile(r'expect output (.+)')

npassed = 0
nfailed = 0

for testi, testfile in enumerate(TESTS):
    teststart = time.perf_counter()
    print(f'[{str(testi + 1).rjust(NTESTWIDTH)}/{NTESTS}] \033[36m{testfile.ljust(LONGESTNAME)}\033[0m (', end='')
    sys.stdout.flush()

    with open(testfile, 'r') as f:
        contents = f.read()

    compiledfile = os.path.join(TESTDIR, os.path.splitext(testfile)[0] + '.o')
    linkedfile = os.path.join(TESTDIR, 'testout')

    outputs = {
        'compile': None,
        'linking': None,
        'running': None
    }

    compiled = False
    linked = False
    ran = False

    compile_command = [EXECLOC, '-e', 'json', testfile]
    link_command = [CPPCOMP, compiledfile, PRINTDEFF, '-o', linkedfile]
    run_command = [linkedfile]

    compilation = subprocess.run(compile_command, capture_output=True)
    compiled = compilation.returncode == 0
    set_outputs(outputs, 'compile', compilation)
    print('c' if compiled else '-', end='')
    sys.stdout.flush()

    if compiled:
        linking = subprocess.run(link_command, capture_output=True)
        linked = linking.returncode == 0
        set_outputs(outputs, 'linking', linking)
    print('l' if linked else '-', end='')
    sys.stdout.flush()

    if linked:
        running = subprocess.run(run_command, capture_output=True)
        ran = True
        set_outputs(outputs, 'running', running)
    print('r' if ran else '-', end='')
    sys.stdout.flush()

    if compiled:
        os.remove(compiledfile)
    if linked:
        os.remove(linkedfile)

    print(') ', end='')
    sys.stdout.flush()

    comp_err_expectations  = EXPECT_COMP_ERR_REGEX  .finditer(contents)
    comp_warn_expectations = EXPECT_COMP_WARN_REGEX .finditer(contents)
    run_err_expectations   = EXPECT_RUN_ERR_REGEX   .finditer(contents)
    print_expectations     = EXPECT_PRINT_REGEX     .finditer(contents)

    failed = False
    failmsg = ''

    try:
        compile_messages = [json.loads(e) for e in outputs['compile']['stderr'].split('\n') if len(e)]
    except json.decoder.JSONDecodeError:
        mark_failed('got internal error')

    for expect in comp_err_expectations:
        expect_nr = contents[:expect.start(0)].count('\n') + 1
        matchede = [e for e in compile_messages if e['type'] == 'error' and e['location']['line'] == expect_nr and os.path.abspath(e['location']['file']) == os.path.abspath(testfile) and e['message'].split(' ')[1] == f'({expect.group(1)})']
        if len(matchede) != 1:
            mark_failed(f'expected {expect.group(1)} on testfile line {expect_nr}, but got {len(matchede)} matched errors')
        else:
            compile_messages.remove(matchede[0])

    for expect in comp_warn_expectations:
        expect_nr = contents[:expect.start(0)].count('\n') + 1
        matchede = [e for e in compile_messages if e['type'] == 'warning' and e['location']['line'] == expect_nr and os.path.abspath(e['location']['file']) == os.path.abspath(testfile) and e['message'].split(' ')[1] == f'({expect.group(1)})']
        if len(matchede) != 1:
            mark_failed(f'expected {expect.group(1)} on testfile line {expect_nr}, but got {len(matchede)} matched warnings')
        else:
            compile_messages.remove(matchede[0])

    if len(compile_messages):
        mark_failed(f'got {len(compile_messages)} extra compile messages')

    if ran:
        output_lines = outputs['running']['stdout'].split('\n')
        if len(output_lines[-1]) == 0:
            del output_lines[-1]

        for expect in print_expectations:
            if len(output_lines) == 0:
                mark_failed('have more print expects, but ran out of lines to check against')
                break

            if expect.group(1) != (l := output_lines.pop(0)):
                mark_failed(f'printed wrong thing (expected {expect.group(1)} but got {l})')

        if len(output_lines) > 0:
            mark_failed('got extra print lines')
    else:
        if len([0 for _ in print_expectations]) > 0:
            mark_failed('expected printing but did not run')

    if failed:
        fail(testfile, failmsg)
    else:
        pass_test(testfile)
        print(' in', round(time.perf_counter() - teststart, 3), 'seconds')

    sys.stdout.flush()

os.remove(PRINTDEFF)

if nfailed:
    sys.exit(1)
