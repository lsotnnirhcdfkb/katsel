import os, sys, glob
import subprocess
import json, re
import colorama
import itertools

class Process:
    def __init__(self, command):
        self.command = command
        self.process = subprocess.run(command, capture_output=True)
        self.stdout = self.process.stdout.decode('utf-8')
        self.stderr = self.process.stderr.decode('utf-8')
        self.success = self.process.returncode == 0
class SkippedProcess:
    def __init__(self, command):
        self.command = command
        self.process = None
        self.stdout = ''
        self.stderr = ''
        self.success = False

class Test:
    def __init__(self, num, test_path):
        self.num = num
        self.test_path = test_path
        self.failures = []

    def run(self):
        write(f'[{self.num}/{NUM_TESTS}] \033[36m{self.test_path}\033[0m (   )\033[4D')
        expect_errors, expect_warnings, expect_prints = self.read_file()

        object_path = os.path.join(TEST_DIR, os.path.splitext(self.test_path)[0] + '.o')
        linked_path = os.path.join(TEST_DIR, 'testout')

        compile_command = [EXECLOC, '-e', 'json', self.test_path]
        link_command = [CPPCOMP, object_path, PRINT_DEF_FILE, '-o', linked_path]
        run_command = [linked_path]

        compile_process = Process(compile_command)

        if compile_process.success:
            write('c')
            link_process = Process(link_command)
        else:
            write('-')
            link_process = SkippedProcess(link_command)

        if link_process.success:
            write('l')
            run_process = Process(run_command)
        else:
            write('-')
            run_process = SkippedProcess(run_command)

        if run_process.success:
            write('r')
        else:
            write('-')

        write(')')

        self.verify_compilation(expect_errors, expect_warnings, compile_process)
        self.verify_running(expect_prints, run_process)

        try:
            os.remove(object_path)
        except FileNotFoundError as e:
            pass
        try:
            os.remove(linked_path)
        except FileNotFoundError as e:
            pass

        return len(self.failures) == 0

    def verify_compilation(self, expect_errors, expect_warnings, process):
        total_expects = itertools.chain(expect_errors, expect_warnings)

        stderr_lines = filter(len, process.stderr.split('\n')) # filter only non-empty lines
        messages = []

        for line in stderr_lines:
            if '!!!' in line:
                self.failures.append(f'Compiler internal error: {line}')
            else:
                try:
                    messages.append(json.loads(line))
                except json.decoder.JSONDecodeError as jexc:
                    self.failures.append((f'JSON Decode Error: {jexc}\n'
                                          f'on line: "{line}"'))

        for expect in total_expects:
            expect_line = match_line(self.test_contents, expect)
            expect_message = expect.group(1)

            found_matching_message = False
            for message in messages:
                if message['start']['line'] == expect_line and message['name'] == expect_message:
                    messages.remove(message)
                    found_matching_message = True
                    break

            if not found_matching_message:
                self.failures.append(f'No match for expected message: {expect_message} on line {expect_line}')

        for message in messages:
            message_line = message['start']['line']
            message_name = message['name']
            self.failures.append(f'Unexpected message {message_name} on line {message_line}')

    def verify_running(self, expect_prints, process):
        if process.stdout == '':
            process_output = []
        else:
            process_output = process.stdout.split('\n')

        expect_prints = [expect.group(1) for expect in expect_prints]

        if len(expect_prints) != len(process_output):
            self.failures.append(f'Expected {len(expect_prints)} lines of output, but got {len(process_output)}')

        for expect_line, output_line in zip(expect_prints, process_output):
            if expect_line != output_line:
                self.failures.append(f'Printed wrong thing: expected {expect_line}, but got {output_line}')

    def read_file(self):
        with open(self.test_path, 'r') as f:
            self.test_contents = f.read()

        expect_errors     = EXPECT_ERROR_REGEX.finditer(self.test_contents)
        expect_warnings   = EXPECT_WARN_REGEX .finditer(self.test_contents)
        expect_prints     = EXPECT_PRINT_REGEX.finditer(self.test_contents)

        return expect_errors, expect_warnings, expect_prints

def match_line(contents, match):
    return contents[:match.start(0)].count('\n') + 1

def write(thing):
    sys.stdout.write(thing)
    sys.stdout.flush()

colorama.init()

if len(sys.argv) == 3:
    EXECLOC = os.path.abspath(sys.argv[1])
    CPPCOMP = os.path.abspath(sys.argv[2])
else:
    print(f'Usage: {sys.argv[0]} <katselc path> <c++ compiler>')
    sys.exit(1)

EXPECT_ERROR_REGEX = re.compile(r'expect compile error \(([\w-]+)\)')
EXPECT_WARN_REGEX  = re.compile(r'expect compile warning \(([\w-]+)\)')
EXPECT_PRINT_REGEX = re.compile(r'expect output (.+)')

PRINT_DEF_FILE = 'printdef.cpp'

TEST_DIR = os.path.abspath(os.path.dirname(__file__))

TESTS = glob.glob(f'{TEST_DIR}/**/*.ksl', recursive=True)
NUM_TESTS = len(TESTS)

with open(PRINT_DEF_FILE, 'w') as f:
    f.write(r'''
        #ifdef __cplusplus
        #include <cstdio>
        #include <cstdint>
        #else
        #include <stdio.h>
        #include <stdint.h>
        #endif

        #ifdef __cplusplus
        extern "C" {
        #endif

        void printunsigned(uint32_t i) { printf("%u\n", i); }
        void printsigned  (int32_t i)  { printf("%d\n", i); }
        void printchar    (uint32_t i) { printf("%c\n", i); }
        void printfloat   (float i)    { printf("%.2f\n", i); }
        void printdouble  (double i)   { printf("%.2f\n", i); }

        #ifdef __cplusplus
        }
        #endif
    ''')

num_passed = 0
num_failed = 0

try:
    for test_num, test_path in enumerate(TESTS):
        write('\n')
        write(f'\033[32m{num_passed}\033[0m passed, \033[31m{num_failed}\033[0m failed')
        write('\033[F\033[2K')

        test = Test(test_num, test_path)
        test_success = test.run()

        write('\033[2K\033[1000D')
        if test_success:
            num_passed += 1
        else:
            num_failed += 1

            write(f'\033[31mFAIL\033[0m: \033[36m{test_path}\033[0m\n')

            for failure in test.failures:
                write(f' \033[31;1m*\033[0m {failure}\n')

            write('\n')
except KeyboardInterrupt:
    write('\033[0m\n\033[2K')
else:
    if num_failed == 0:
        write('\033[32;1mALL TESTS PASSED!\033[0m\n')
    else:
        write(f'\033[31;1m{num_failed} tests failed\033[0m, \033[32;1m{num_passed} tests passed\033[0m\n')
        sys.exit(1)
finally:
    os.remove(PRINT_DEF_FILE)
