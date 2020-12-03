import os, sys, subprocess, glob, colorama, re

colorama.init()

if len(sys.argv) == 3:
    EXECLOC = os.path.abspath(sys.argv[1])
    LINKER = os.path.abspath(sys.argv[2])
else:
    print(f'Usage: {sys.argv[0]} <katselc path> <linker>')
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

EXPECT_COMP_ERR_REGEX = re.compile(r'// expect compile error \(([\w-]+)\)$', re.MULTILINE)
EXPECT_COMP_WARN_REGEX = re.compile(r'// expect compile warning \(([\w-]+)\)$', re.MULTILINE)
EXPECT_RUN_ERR_REGEX = re.compile(r'// expect runtime error$', re.MULTILINE)
EXPECT_PRINT_REGEX = re.compile(r'// expect output (.+)$', re.MULTILINE)

npassed = 0
nfailed = 0

for testi, testfile in enumerate(TESTS):
    print(f'[{str(testi + 1).rjust(NTESTWIDTH)}/{NTESTS}] - \033[36m{testfile}\033[0m (', end='')

    with open(testfile, 'r') as f:
        contents = f.read()

    compErrExpectations  = EXPECT_COMP_ERR_REGEX  .findall(contents)
    compWarnExpectations = EXPECT_COMP_WARN_REGEX .findall(contents)
    runErrExpectations   = EXPECT_RUN_ERR_REGEX   .findall(contents)
    printExpectations    = EXPECT_PRINT_REGEX     .findall(contents)

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

    compileCommand = [EXECLOC, testfile]
    linkCommand = [LINKER, compiledfile, '-o', linkedfile]
    runCommand = [linkedfile]

    compilation = subprocess.run(compileCommand, capture_output=True)
    compiled = compilation.returncode == 0
    setOutputs(outputs, 'compile', compilation)

    if compiled:
        linking = subprocess.run(linkCommand, capture_output=True)
        linked = linking.returncode == 0
        setOutputs(outputs, 'linking', linking)

    if linked:
        running = subprocess.run(runCommand, capture_output=True)
        ran = True
        setOutputs(outputs, 'running', running)

    if compiled:
        os.remove(compiledfile)
    if linked:
        os.remove(linkedfile)

    print('c' if compiled else '-', end='')
    print('l' if linked   else '-', end='')
    print('r' if ran      else '-', end='')

    print('): ', end='')

    passTest(testfile)

if nfailed:
    sys.exit(1)
