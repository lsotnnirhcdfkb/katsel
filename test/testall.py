import os
import sys
import subprocess
from yaml import CLoader as Loader, load

if len(sys.argv) == 2:
    EXECLOC = os.path.abspath(sys.argv[1])
else:
    print(f'Usage: {sys.argv[0]} <coxianc path>')
    sys.exit(1)

orig = os.path.abspath(os.getcwd())
dirname = os.path.abspath(os.path.dirname(__file__))

os.chdir(dirname)
print(f'Change to {dirname}')
print()
for testfile in os.listdir():
    if testfile.endswith('.oxian'):
        print(f'Testing file {testfile}')
        with open(testfile, 'r') as f:
            contents = f.read()

        yamlstart = contents.index('/*')
        yamlend = contents.index('*/')

        thingy = load(contents[yamlstart+2:yamlend], Loader=Loader)

        command = [EXECLOC, testfile, *thingy['args']]
        print(f'Run command {command}')

        proc = subprocess.run(command, capture_output=True, check=True)
        stdout = proc.stdout.decode('utf-8')

        output = thingy['output']
        if stdout == output:
            print(f'Test for {testfile} passed.')
        else:
            print(f'Test for {testfile} failed!')

            expectedfile = f'{testfile}.expected.txt'
            gottenfile = f'{testfile}.gotten.txt'
            with open(expectedfile, 'w') as f:
                print(f'Expected output written to \'{expectedfile}\'')
                f.write(output)

            with open(gottenfile, 'w') as f:
                print(f'Gotten output written to \'{gottenfile}\'')
                f.write(stdout)
        print()

os.chdir(orig)
print(f'Change to {orig}')
