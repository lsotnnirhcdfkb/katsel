def to_snake_case(iden):
    s = []
    prev = ''
    for ch in iden:
        if ch.isupper() and len(prev) > 0 and prev.islower():
            s.append('_')

        s.append(ch.lower())
        prev = ch

    return ''.join(s)

def visit_method_name(name):
    return to_snake_case(f'visit{name}')
