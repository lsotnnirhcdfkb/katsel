import re

LINE_START_PATTERN = re.compile(r'^(?!(\s*\n))', re.MULTILINE)

def to_snake_case(iden):
    s = []
    prev = ''
    for ch in iden:
        if ch.isupper() and len(prev) > 0 and prev.islower():
            s.append('_')

        s.append(ch.lower())
        prev = ch

    return ''.join(s)

def indent(text, amount):
    return LINE_START_PATTERN.sub(' ' * amount, text)
