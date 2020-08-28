#!/usr/bin/env python3
# {{{ trienode class
## A node in a Trie
class TrieNode:
    def __init__(self, value, length, tokentype=None):
        self.value = value
        self.length = length
        self.nodes = []
        self.tokentype = tokentype

    def addNode(self, node):
        self.nodes.append(node)

    def hasNode(self, nodevalue):
        return nodevalue in [node.value for node in self.nodes]

    def getNode(self, nodevalue):
        #   every i in the self.nodes where self.nodes[i].value == nodevalue
        i = [i for i in range(len(self.nodes)) if self.nodes[i].value == nodevalue][0]
        return self.nodes[i]

    def show(self, uni=True):
        if uni:
            return self.__show([], False, ['\u2502', '\u2514', '\u2500'])
        else:
            return self.__show([], False, ['|', '+', '-'])

    def __show(self, indent, isLast, chars):
        if isLast:
            indent = list(indent)
            indent[-1] = chars[1]

        output = []

        output.append(f'{"".join(indent)}{chars[2]}\'{self.value}\'')
        if self.tokentype != None:
            output.append(f' - {self.tokentype}')
        output.append('\n')

        if isLast:
            indent[-1] = ' '

        for i, node in enumerate(self.nodes):
            output.append(node.__show(indent + ['  ', chars[0]], i == len(self.nodes) - 1, chars))

        return ''.join(output)

    def generate(self, doc=''):
        output = []
        output.append(f'/// {doc}\n')
        output.append('TokenType Lexer::getIdentifierType()\n{\n')
        output.append(self.__generate(True, 1))
        output.append('\n')
        output.append(f'{self.__getIndent(1)}return TokenType::IDENTIFIER;\n')
        output.append('}\n')
        return ''.join(output)

    def __generate(self, root, indent):
        output = []

        indentStr = self.__getIndent(indent)
        bodyIndentStr = self.__getIndent(indent + (1 if len(self.nodes) else 0))
        breakIndentStr = self.__getIndent(indent + 2)

        if self.tokentype != None:
            output.append(f'{bodyIndentStr}if (start + {self.length} == end) return TokenType::{self.tokentype};\n')

        if len(self.nodes) == 0:
            return ''.join(output)

        output.append(f'{indentStr}switch (*(start + {self.length}))\n{indentStr}{{\n')
        for node in self.nodes:
            output.append(f'{bodyIndentStr}case \'{node.value}\':\n')
            output.append(node.__generate(False, indent + 2))
            output.append(f'{breakIndentStr}break;\n')
        output.append(f'{indentStr}' + '}\n')

        return ''.join(output)

    def __getIndent(self, indent, tab=False):
        return ('\t' if tab else '    ') * indent
# }}}

# {{{ keywords
keywords = [
    ('void', 'VOID'),
    ('namespace', 'NAMESPACE'),
    ('class', 'CLASS'),
    ('enum', 'ENUM'),
    ('return', 'RETURN'),
    ('this', 'THIS'),
    ('while', 'WHILE'),
    ('for', 'FOR'),
    ('if', 'IF'),
    ('else', 'ELSE'),
    ('switch', 'SWITCH'),
    ('case', 'CASE'),
    ('default', 'DEFAULT'),
    ('break', 'BREAK'),
    ('breakall', 'BREAKALL'),
    ('breakto', 'BREAKTO'),
    ('const', 'CONST'),
    ('continue', 'CONTINUE'),
    ('inline', 'INLINE'),
    ('volatile', 'VOLATILE'),
    ('float', 'FLOAT'),
    ('bool', 'BOOL'),
    ('double', 'DOUBLE'),
    ('true', 'TRUELIT'),
    ('false', 'FALSELIT'),
    ('null', 'NULLLIT'),
    ('assert', 'ASSERT'),
    ('char', 'CHAR'),
    ('uint8', 'UINT8'),
    ('uint16', 'UINT16'),
    ('uint32', 'UINT32'),
    ('uint64', 'UINT64'),
    ('sint8', 'SINT8'),
    ('sint16', 'SINT16'),
    ('sint32', 'SINT32'),
    ('sint64', 'SINT64'),
    ('var', 'VAR')
]
# }}}

trie = TrieNode(None, 0)

# {{{ generating
for keyword, tokentype in keywords:
    lastnode = trie
    for letter in keyword:
        if lastnode.hasNode(letter):
            lastnode = lastnode.getNode(letter)
            continue

        newnode = TrieNode(letter, lastnode.length + 1)
        lastnode.addNode(newnode)
        lastnode = newnode

    lastnode.tokentype = tokentype
# }}}

