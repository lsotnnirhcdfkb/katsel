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

    def updateFirsts(self):
        self.first = getFirsts(self)
    def updateFollows(self):
        self.follow = getFollows(self)

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
# helpers {{{1
def makeUnique(already, new):
    return [x for x in new if x not in already]
# first and follows functions {{{1
def getFirsts(sym):
    if type(sym) == Terminal:
        return [sym]

    firsts = []
    for rule in grammar:
        if rule.symbol == sym:
            if len(rule.expansion) == 0:
                if eofSym not in firsts:
                    firsts.append(eofSym)
            elif rule.expansion[0] != sym:
                if type(rule.expansion[0]) == NonTerminal:
                    firsts.extend(makeUnique(firsts, getFirsts(rule.expansion[0])))
                else:
                    if rule.expansion[0] not in firsts:
                        firsts.append(rule.expansion[0])

    return firsts
def getFollows(sym):
    if sym == augmentSymbol:
        return [eofSym]

    follows = []
    for rule in grammar:
        for i, gsym in enumerate(rule.expansion):
            if gsym != sym:
                continue

            if i + 1 >= len(rule.expansion):
                if rule.symbol != sym:
                    follows.extend(makeUnique(follows, getFollows(rule.symbol)))
            else:
                follows.extend(makeUnique(follows, [x for x in getFirsts(rule.expansion[i + 1]) if x != Terminal('eof')]))

    return follows
# rules {{{1
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

augmentSymbol = NonTerminal('augment')
augmentRule = Rule(augmentSymbol, (grammar[0].symbol, ))
grammar.append(augmentRule)

eofSym = Terminal('TokenType::EOF_')

for rule in grammar:
    for sym in [rule.symbol, *rule.expansion]:
        if type(sym) == NonTerminal:
            sym.updateFirsts()
            sym.updateFollows()
