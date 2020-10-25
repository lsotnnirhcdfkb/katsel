import os, sys, subprocess, glob, shlex, yaml, traceback

functions = r'''
def expectToken(tokType):
    global outl
    i = outl
    while stdout[i] != '\n':
        i += 1

    line = stdout[outl:i]
    ttype = line[line.index('(') + 1:line.index(')')]
    # tok = line[line.index('"') + 1: line.rindex('"')]
    assert ttype == tokType, f'Got unexpected token {ttype}, expected {tokType}'
    outl = i + 1

'''

if len(sys.argv) == 2:
    EXECLOC = os.path.abspath(sys.argv[1])
else:
    print(f'Usage: {sys.argv[0]} <coxianc path>')
    sys.exit(1)

orig = os.getcwd()
dirname = os.path.abspath(os.path.dirname(__file__))

os.chdir(dirname)

files = glob.glob('./**/*.oxian', recursive=True)
nfiles = len(files)

anyFailed = False

for i, testfile in enumerate(files):
    print(f'[{i + 1}/{nfiles}] Testing \033[36m{testfile}\033[0m: ', end='')
    with open(testfile, 'r') as f:
        contents = f.read()

    opts = yaml.load(contents[contents.index('/*') + 2 : contents.index('*/')], Loader=yaml.CLoader)
    command = shlex.split(opts['command'].replace('<coxianc>', EXECLOC).replace('<file>', testfile))
    expectretc = opts['returncode']
    checkc = functions + opts['test']

    proc = subprocess.run(command, capture_output=True)
    stdout = proc.stdout.decode('utf-8')
    stderr = proc.stderr.decode('utf-8')

    failed = False
    log = ''

    if proc.returncode != expectretc:
        failed = True
        log += f'Expected return code {expectretc}, got {proc.returncode}\n'

    try:
        exec(checkc, {
            'testfile': testfile, 
            'outl': 0,
            'errl': 0,
            'stdout': stdout,
            'stderr': stderr,
        })
    except:
        failed = True
        log += 'Failed with test code error:\n'
        log += traceback.format_exc()

    if failed:
        print(f'\033[0;1;31mfailed\033[0m')
        faillog = os.path.splitext(testfile)[0] + '.log'
        print(f'\t- For more information, check log written to \033[36m{faillog}\033[0m')
        with open(faillog, 'w') as f:
            f.write(f'Error log for test file {testfile}\nCommand: {command}\nReturn code: {proc.returncode}\nstdout: \'{stdout}\'\nstderr: \'{stderr}\'\ncode: \'{checkc}\'\n\n')
            f.write(log)

        anyFailed = True
    else:
        print(f'\033[0;1;32mpassed\033[0m')

os.chdir(orig)

if anyFailed:
    sys.exit(1)
