#!/usr/bin/env python3
## @file kwgen.py
#  Generate keyword matching code to go in Lexer::getIdentifierType()

# TrieNode class {{{1
## A node in a Trie
class TrieNode:

    ## @var value
    # The value of this trie node

    ## @var length
    # The current depth of this trie node in the trie

    ## @var nodes
    # The children nodes of this node

    ## @var tokentype
    # The tokentype that this trienode could represent

    ## The constructor
    def __init__(self, value, length, tokentype=None):
        self.value = value
        self.length = length
        self.nodes = []
        self.tokentype = tokentype

    ## Add a child node this node
    def add_node(self, node):
        self.nodes.append(node)

    ## Check if this node has a child with a certain value
    def has_node(self, nodevalue):
        return nodevalue in [node.value for node in self.nodes]

    ## Get a child node with a certain value
    def get_node(self, nodevalue):
        #   every i in the self.nodes where self.nodes[i].value == nodevalue
        i = [i for i in range(len(self.nodes)) if self.nodes[i].value == nodevalue][0]
        return self.nodes[i]

    ## Wrapper method to show this node in a human readable format
    def show(self, uni=True):
        if uni:
            chars = ['\u2502', '\u2514', '\u2500']
        else:
            chars = ['|', '+', '-']

        return self.__show([], False, chars)

    ## Recursive method to show this node
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

    ## Wrapper method to generate code to match this node
    def generate(self, doc=''):
        output = []
        output.append(f'/// {doc}\n')
        output.append('TokenType Lexer::getIdentifierType() {\n')
        output.append(self.__generate(1))
        output.append('\n')
        output.append(f'{TrieNode.get_indent(1)}return TokenType::IDENTIFIER;\n')
        output.append('}\n')
        return ''.join(output)

    ## Recursive method to generate code to match this node
    def __generate(self, indent):
        output = []

        indent_str = self.get_indent(indent)
        body_indent_str = self.get_indent(indent + (1 if len(self.nodes) > 0 else 0))
        break_indent_str = self.get_indent(indent + 2)

        if self.tokentype is not None:
            output.append(f'{body_indent_str}if (start + {self.length} == end) return TokenType::{self.tokentype};\n')

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

            output.append(f') return TokenType::{cur.tokentype};\n')


            return ''.join(output)

        output.append(f'{indent_str}switch (*(start + {self.length})) {{\n')
        for node in self.nodes:
            output.append(f'{body_indent_str}case \'{node.value}\':\n')
            output.append(node.__generate(indent + 2))
            output.append(f'{break_indent_str}break;\n')
        output.append(indent_str + '}\n')

        return ''.join(output)

    ## Get the string to pad with for a certain indent level
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
## Keywords to generate matching code for
keywords = [
    ('void', 'VOID'),
    ('float', 'FLOAT'),
    ('bool', 'BOOL'),
    ('double', 'DOUBLE'),
    ('char', 'CHAR'),
    ('uint8', 'UINT8'),
    ('uint16', 'UINT16'),
    ('uint32', 'UINT32'),
    ('uint64', 'UINT64'),
    ('sint8', 'SINT8'),
    ('sint16', 'SINT16'),
    ('sint32', 'SINT32'),
    ('sint64', 'SINT64'),

    ('class', 'CLASS'),
    ('data', 'DATA'),
    ('impl', 'IMPL'),
    ('fun', 'FUN'),
    ('var', 'VAR'),
    ('let', 'LET'),

    ('return', 'RETURN'),

    ('while', 'WHILE'),
    ('for', 'FOR'),
    ('if', 'IF'),
    ('else', 'ELSE'),
    ('match', 'MATCH'),

    ('break', 'BREAK'),
    ('continue', 'CONTINUE'),

    ('true', 'TRUELIT'),
    ('false', 'FALSELIT'),
    ('nullptr', 'NULLPTRLIT'),

    ('assert', 'ASSERT'),

    ('boom', 'BOOM'),
]
# }}}

## The trie that represents all the different keywords
trie = TrieNode(None, 0)

# generating {{{1
for keyword, tokentype in keywords:
    lastnode = trie
    for letter in keyword:
        if lastnode.has_node(letter):
            lastnode = lastnode.get_node(letter)
            continue

        newnode = TrieNode(letter, lastnode.length + 1)
        lastnode.add_node(newnode)
        lastnode = newnode

    lastnode.tokentype = tokentype
