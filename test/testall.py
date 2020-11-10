import os, sys, subprocess, glob, shlex, yaml, traceback

functions = r'''
class lex:
    @staticmethod
    def expect(type):
        lines = [l for l in stdout.split('\n') if l.startswith(f'{testfile}:{line}')]
        assert len(lines) > 0, f'no matching line for type {type}'

        found = False
        for l in lines:
            if l[l.index('(') + 1:l.index(')')] == type:
                found = True

        if not found:
            raise Exception(f'Token {type} not found on line {line}')
'''

def getLine(i, con):
    count = 0
    while i > 0:
        if con[i] == '\n':
            count += 1
        i -= 1
    return count + 1

def failTest(log, logname, fname):
    global anyFailed
    print(f'\033[0;1;31mfailed\033[0m')
    print(f'\t- For more information, check log written to \033[36m{faillog}\033[0m')
    with open(logname, 'w') as f:
        f.write(f'Error log for test file {fname}\n\n')
        f.write(log)

    anyFailed = True

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

    failed = False
    log = ''
    faillog = os.path.splitext(testfile)[0] + '.log'

    if os.path.exists(faillog):
        os.remove(faillog)

    with open(testfile, 'r') as f:
        contents = f.read()

    try:
        opts = yaml.load(contents[contents.index('/*') + 2 : contents.index('*/')], Loader=yaml.CLoader)
    except:
        failed = True
        log += 'Failed with yaml error:\n'
        log += traceback.format_exc()
        log += '\n'

        failTest(log, faillog, testfile)
        continue

    if 'command' not in opts:
        log += 'Failed: command field not found in yaml\n\n'
        failTest(log, faillog, testfile)
        continue
    if 'returncode' not in opts:
        log += 'Failed: returncode field not found in yaml\n\n'
        failTest(log, faillog, testfile)
        continue

    command = shlex.split(opts['command'].replace('<coxianc>', EXECLOC).replace('<file>', testfile))
    expectretc = opts['returncode']

    proc = subprocess.run(command, capture_output=True)
    stdout = proc.stdout.decode('utf-8')
    stderr = proc.stderr.decode('utf-8')

    tests = []
    i = 0
    while i <= len(contents):
        try:
            i = contents.index('--->', i) + 4
            iend = contents.index('<', i)
            tests.append({
                'text': contents[i:iend].strip(),
                'start': i,
                'end': iend,
                'line': getLine(i, contents)
            })
        except ValueError:
            break

    if proc.returncode != expectretc:
        failed = True
        log += f'Expected return code {expectretc}, got {proc.returncode}\n'
        log += '\n'

    for test in tests:
        try:
            ns = test
            ns['contents'] = contents
            ns['testfile'] = testfile
            ns['stdout'] = stdout
            ns['stderr'] = stderr

            exec(functions + test['text'], ns)
        except Exception as e:
            failed = True
            log += 'Failed with test error:\n'
            log += traceback.format_exc()
            log += '\n'

    if failed:
        failTest(log, faillog, testfile)
        anyFailed = True
    else:
        print(f'\033[0;1;32mpassed\033[0m')

os.chdir(orig)

if anyFailed:
    sys.exit(1)
