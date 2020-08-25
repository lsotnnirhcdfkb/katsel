import subprocess

files = [
    ('src/lexer.cpp', 'KWGEN BEGIN HERE', 'KWGEN END HERE', 'utils/kwgen.py', '-c')
    ('src/ast.cpp', 0, 0, 'utils/kwgen.py', '-s')
    ('include/ast.h', 0, 0, 'utils/kwgen.py', '-e')
]

subprocess
