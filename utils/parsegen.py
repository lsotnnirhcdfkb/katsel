#!/usr/bin/env python3

# classes {{{1
# symbols {{{2
class NonTerminal:
    def __init__(self, symbol):
        self.symbol = symbol
        self.first = []
        self.follow = []

    def __repr__(self):
        return str(self)
    def __str__(self):
        return self.symbol

    def __eq__(self, other):
        return type(self) == type(other) and (self.symbol == other.symbol)

class Terminal:
    def __init__(self, symbol):
        self.symbol = symbol

    def __repr__(self):
        return str(self)
    def __str__(self):
        return self.symbol

    def __eq__(self, other):
        return type(self) == type(other) and (self.symbol == other.symbol)
# rule {{{2
class Rule:
    __num = 0
    def __init__(self, symbol, expansion):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1

    def __repr__(self):
        return str(self)
    def __str__(self):
        return f'{str(self.symbol)} -> {" ".join(map(str, self.expansion))}'

    def __eq__(self, other):
        return type(self) == type(other) and self.symbol == other.symbol and self.expansion == other.expansion
# item stuff {{{2
class Item: # an LR1 item
    def __init__(self, rule, index, lookahead):
        self.rule = rule
        self.index = index
        self.lookahead = lookahead

    def __repr__(self):
        return str(self)
    def __str__(self):
        expansionstr = list(map(str, self.rule.expansion))
        expansionstr.insert(self.index, '.')
        return str(self.rule.symbol) + ' -> ' + ' '.join(expansionstr) + ', ' + str(self.lookahead)

    def __eq__(self, other):
        return type(self) == type(other) and self.rule == other.rule and self.index == other.index and self.lookahead == other.lookahead

class ItemSet: # an LR1 item set
    __n = 0
    def __init__(self, kernel):
        self.kernel = kernel
        self.extras = []
        self.n = ItemSet.__n
        ItemSet.__n += 1

    def __repr__(self):
        return str(self)
    def __str__(self):
        return f'Itemset {self.n}\n' + \
            ''.join(map(lambda x: 'K: ' + str(x) + '\n', self.kernel)) + \
            ''.join(map(lambda x: 'E: ' + str(x) + '\n', self.extras))
    def __eq__(self, other):
        return self.items == other.items
# state stuff {{{2
class State:
    def __init__(self, set_):
        self.set_ = set_
        self.actions = {}
        self.goto = {}
# actions {{{2
class ShiftAction:
    def __init__(self, newstate):
        self.newstate = newstate
    def __str__(self):
        return f's{self.newstate}'
class ReduceAction:
    def __init__(self, rule):
        self.rule = rule
    def __str__(self):
        return f'r{self.rule}'
class AcceptAction:
    def __str__(self):
        return 'acc'
# rules {{{1
def r(s, e):
    return Rule(nt(s), e)
def nt(s):
    return NonTerminal(s)
def t(s):
    return Terminal(f'TokenType::{s}')

_grammar = '''
statement -> $expression
expression -> $addition

addition -> $addition PLUS $multiplication
addition -> $addition MINUS $multiplication
addition -> $multiplication

multiplication -> $multiplication STAR $unary
multiplication -> $multiplication SLASH $unary
multiplication -> $unary

unary -> MINUS $unary
unary -> TILDE $unary
unary -> $primary

primary -> DECINTLIT
primary -> OPARN $expression CPARN
'''

grammar = []
for rule in _grammar.split('\n'):
    if len(rule) == 0:
        continue
    symbol, expansion = rule.split('->', 1)
    symbol = symbol.strip().rstrip()
    expansion = list(filter(len, expansion.split(' ')))
    for i, s in enumerate(expansion):
        if s.startswith('$'):
            expansion[i] = NonTerminal(s[1:])
        else:
            expansion[i] = Terminal(f'TokenType::{s}')

    grammar.append(Rule(NonTerminal(symbol), tuple(expansion)))
    print(grammar[-1])

augmentSymbol = NonTerminal('augment')
augmentRule = Rule(augmentSymbol, (grammar[0].symbol, ))
grammar.append(augmentRule)
