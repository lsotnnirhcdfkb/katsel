import os, sys, subprocess, glob, colorama, re, json

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
    print(f'\033[0;1;31mfailed\033[0m')
    print(f'\t- {msg}')

def passTest(testfile):
    global npassed
    npassed += 1
    print(f'\033[0;1;32mpassed\033[0m')

def setOutputs(outputs, category, process):
    outputs[category]           = {}
    outputs[category]['stdout'] = process.stdout.decode('utf-8')
    outputs[category]['stderr'] = process.stderr.decode('utf-8')

TESTDIR = os.path.abspath(os.path.dirname(__file__))

TESTS = glob.glob(f'{TESTDIR}/**/*.ksl', recursive=True)
NTESTS = len(TESTS)
NTESTWIDTH = len(str(NTESTS))

PRINTDEFF = 'printdef.c'

with open(PRINTDEFF, 'w') as f:
    f.write(r'''
#include <stdio.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
#endif
void printd(uint32_t i)
{
    printf("%d\n", i);
}

#ifdef __cplusplus
extern "C"
#endif
void printc(uint32_t i)
{
    printf("%c", i);
}
''')

EXPECT_COMP_ERR_REGEX = re.compile(r'// expect compile error \(([\w-]+)\)$', re.MULTILINE)
EXPECT_COMP_WARN_REGEX = re.compile(r'// expect compile warning \(([\w-]+)\)$', re.MULTILINE)
EXPECT_RUN_ERR_REGEX = re.compile(r'// expect runtime error$', re.MULTILINE)
EXPECT_PRINT_REGEX = re.compile(r'// expect output (.+)$', re.MULTILINE)

npassed = 0
nfailed = 0

for testi, testfile in enumerate(TESTS):
    print(f'[{str(testi + 1).rjust(NTESTWIDTH)}/{NTESTS}] - \033[36m{testfile}\033[0m (', end='')
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

    compileCommand = [EXECLOC, '-e', 'json', testfile]
    linkCommand = [CPPCOMP, compiledfile, PRINTDEFF, '-o', linkedfile]
    runCommand = [linkedfile]

    compilation = subprocess.run(compileCommand, capture_output=True)
    compiled = compilation.returncode == 0
    setOutputs(outputs, 'compile', compilation)
    print('c' if compiled else '-', end='')
    sys.stdout.flush()

    if compiled:
        linking = subprocess.run(linkCommand, capture_output=True)
        linked = linking.returncode == 0
        setOutputs(outputs, 'linking', linking)
    print('l' if linked else '-', end='')
    sys.stdout.flush()

    if linked:
        running = subprocess.run(runCommand, capture_output=True)
        ran = True
        setOutputs(outputs, 'running', running)
    print('r' if ran else '-', end='')
    sys.stdout.flush()

    if compiled:
        os.remove(compiledfile)
    if linked:
        os.remove(linkedfile)

    print('): ', end='')
    sys.stdout.flush()

    compErrExpectations  = EXPECT_COMP_ERR_REGEX  .finditer(contents)
    compWarnExpectations = EXPECT_COMP_WARN_REGEX .finditer(contents)
    runErrExpectations   = EXPECT_RUN_ERR_REGEX   .finditer(contents)
    printExpectations    = EXPECT_PRINT_REGEX     .finditer(contents)

    compileMessages = [json.loads(e) for e in outputs['compile']['stderr'].split('\n') if len(e)]

    failed = False
    failmsg = ''

    for expect in compErrExpectations:
        expectNr = contents[:expect.start(0)].count('\n') + 1
        matchede = [e for e in compileMessages if e['type'] == 'error' and e['location']['line'] == expectNr and os.path.abspath(e['location']['file']) == os.path.abspath(testfile) and e['message'].split(' ')[1] == f'({expect.group(1)})']
        if len(matchede) != 1:
            failed = True
            failmsg = f'expected {expect.group(1)} on testfile line {expectNr}, but got {len(matchede)} matched errors'
        else:
            compileMessages.remove(matchede[0])

    for expect in compWarnExpectations:
        expectNr = contents[:expect.start(0)].count('\n') + 1
        matchede = [e for e in compileMessages if e['type'] == 'warning' and e['location']['line'] == expectNr and os.path.abspath(e['location']['file']) == os.path.abspath(testfile) and e['message'].split(' ')[1] == f'({expect.group(1)})']
        if len(matchede) != 1:
            failed = True
            failmsg = f'expected {expect.group(1)} on testfile line {expectNr}, but got {len(matchede)} matched warnings'
        else:
            compileMessages.remove(matchede[0])

    if len(compileMessages):
        failed = True
        failmsg = f'got {len(compileMessages)} extra compile messages'

    if ran:
        ol = outputs['running']['stdout'].split('\n')
        if not len(ol[-1]):
            del ol[-1]

        for expect in printExpectations:
            if not len(ol):
                failed = True
                failmsg = 'have more print expects, but ran out of lines to check against'
                break

            if expect.group(1) != ol.pop(0):
                failed = True

        if len(ol):
            failed = True
            failmsg = 'got extra print lines'
    else:
        if len([0 for _ in printExpectations]):
            failed = True
            failmsg = 'expected printing but did not run'

    if failed:
        fail(testfile, failmsg)
    else:
        passTest(testfile)
    sys.stdout.flush()

os.remove(PRINTDEFF)

if nfailed:
    sys.exit(1)
