#!/usr/bin/env python3

# TrieNode class {{{1
class TrieNode:
    def __init__(self, value, length):
        self.value = value
        self.length = length
        self.nodes = []
        self.tokentype = None
        self.initializer = None

    def add_node(self, node):
        self.nodes.append(node)

    def has_node(self, nodevalue):
        return nodevalue in [node.value for node in self.nodes]

    def get_node(self, nodevalue):
        # every i in the self.nodes where self.nodes[i].value == nodevalue
        i = [i for i in range(len(self.nodes)) if self.nodes[i].value == nodevalue][0]
        return self.nodes[i]

    def show(self, uni=True):
        if uni:
            chars = ['\u2502', '\u2514', '\u2500']
        else:
            chars = ['|', '+', '-']

        return self.__show([], False, chars)

    def __show(self, indent, is_last, chars):
        if is_last:
            indent = list(indent)
            indent[-1] = chars[1]

        output = []

        output.append(f'{"".join(indent)}{chars[2]}\'{self.value}\'')
        if self.tokentype is not None:
            output.append(f' - {self.tokentype}')
        output.append('\n')

        if is_last:
            indent[-1] = ' '

        for i, node in enumerate(self.nodes):
            output.append(node.__show(indent + ['  ', chars[0]], i == len(self.nodes) - 1, chars))

        return ''.join(output)

    def generate(self):
        output = []
        output.append('TokenData Lexer::get_identifier_type() {\n')
        output.append(self.__generate(1))
        output.append('\n')
        output.append(f'{TrieNode.get_indent(1)}return Tokens::Identifier {{ std::string(start, end) }};\n')
        output.append('}\n')
        return ''.join(output)

    def __generate(self, indent):
        output = []

        indent_str = self.get_indent(indent)
        body_indent_str = self.get_indent(indent + (1 if len(self.nodes) > 0 else 0))
        break_indent_str = self.get_indent(indent + 2)

        if self.tokentype is not None:
            output.append(f'{body_indent_str}if (start + {self.length} == end) return Tokens::{self.tokentype} {{ {self.initializer} }};\n')

        if len(self.nodes) == 0:
            return ''.join(output)

        if self.canusestrcmp():
            letters = []
            cur = self
            while len(cur.nodes) > 0:
                cur = cur.nodes[0]
                letters.append((cur.length - 1, cur.value))

            output.append(f'{indent_str}if (std::distance(start, end) == {self.length + len(letters)}')
            for ind, val in letters:
                output.append(f' && *(start + {ind}) == \'{val}\'')

            output.append(f') return Tokens::{cur.tokentype} {{ {cur.initializer} }};\n')

            return ''.join(output)

        output.append(f'{indent_str}switch (*(start + {self.length})) {{\n')
        for node in self.nodes:
            output.append(f'{body_indent_str}case \'{node.value}\':\n')
            output.append(node.__generate(indent + 2))
            output.append(f'{break_indent_str}break;\n')
        output.append(indent_str + '}\n')

        return ''.join(output)

    @staticmethod
    def get_indent(indent, tab=False):
        return ('\t' if tab else '    ') * indent

    def canusestrcmp(self):
        if len(self.nodes) == 0:
            return True
        elif len(self.nodes) == 1:
            return self.nodes[0].canusestrcmp()
        else:
            return False

# keywords {{{1
keywords = [
    ('data', 'Data', None),
    ('impl', 'Impl', None),
    ('fun', 'Fun', None),
    ('var', 'Var', None),
    ('mut', 'Mut', None),
    ('let', 'Let', None),

    ('this', 'This', None),

    ('return', 'Return', None),

    ('while', 'While', None),
    ('for', 'For', None),
    ('if', 'If', None),
    ('else', 'Else', None),
    ('case', 'Case', None),

    ('break', 'Break', None),
    ('continue', 'Continue', None),

    ('true', 'BoolLit', 'true'),
    ('false', 'BoolLit', 'false'),

    ('boom', 'Boom', None),
]
# }}}
trie = TrieNode(None, 0)
# make the trie {{{1
for keyword, tokentype, initializer in keywords:
    lastnode = trie
    for letter in keyword:
        if lastnode.has_node(letter):
            lastnode = lastnode.get_node(letter)
            continue

        newnode = TrieNode(letter, lastnode.length + 1)
        lastnode.add_node(newnode)
        lastnode = newnode

    lastnode.tokentype = tokentype
    lastnode.initializer = '' if initializer is None else initializer
