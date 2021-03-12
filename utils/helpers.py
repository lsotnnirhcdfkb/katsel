def to_snake_case(iden):
    s = []
    prev = ''
    for ch in iden:
        if ch.isupper() and len(prev) > 0 and prev.islower():
            s.append('_')

        s.append(ch.lower())
        prev = ch

    return ''.join(s)

class Field:
    def __init__(self, type_, name):
        self.type = type_
        self.name = name

    def __eq__(self, other):
        return self.type == other.type and self.name == other.name

    @staticmethod
    def process(fields):
        process_field = lambda field: (item.strip() for item in field.split('|'))
        return [Field(*process_field(f)) for f in fields.split('!')]

    @staticmethod
    def as_params(fields):
        return ', '.join(f'{field.type} {field.name}' for field in fields)

    @staticmethod
    def as_ref_params(fields):
        return ', '.join(f'{field.type} const &{field.name}' for field in fields)

    @staticmethod
    def as_fields(fields, indent=0):
        indent_str = ' ' * indent
        return ''.join(f'{indent_str}{field.type} {field.name};\n' for field in fields)

    @staticmethod
    def as_init_list(fields):
        return ', '.join(f'{field.name}({field.name})' for field in fields)

    @staticmethod
    def as_move_init_list(fields):
        return ', '.join(f'{field.name}(std::move({field.name}))' for field in fields)
