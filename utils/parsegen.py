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

    def __hash__(self):
        return hash(self.symbol)

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

    def __hash__(self):
        return hash(self.symbol)

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

    def getAfterDot(self):
        if self.index < len(self.rule.expansion):
            return self.rule.expansion[self.index]
        else:
            return None

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

    def items(self):
        return self.kernel + self.extras

    def __repr__(self):
        return str(self)
    def __str__(self):
        return f'Itemset {self.n}\n' + \
            ''.join(map(lambda x: 'K: ' + str(x) + '\n', self.kernel)) + \
            ''.join(map(lambda x: 'E: ' + str(x) + '\n', self.extras))
    def __eq__(self, other):
        return self.kernel == other.kernel and self.extras == other.extras

    def __del__(self):
        ItemSet.__n -= 1
# state stuff {{{2
class State:
    def __init__(self, set_):
        self.set_ = set_
        self.actions = {}
        self.goto = {}

    def setAction(self, sym, action):
        if sym in self.actions.keys():
            raise Exception(f'action table conflict: {type(action)}/{type(self.actions[sym])}')

        self.actions[sym] = action

    def setGoto(self, sym, newstate):
        if sym in self.goto.keys():
            raise Exception('goto conflict')

        self.goto[sym] = newstate
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
        return f'r{self.rule.num}'
class AcceptAction:
    def __str__(self):
        return 'acc'
# helpers {{{1
def makeUnique(already, new):
    return [x for x in new if x not in already]
# first and follows functions {{{2
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
# closure {{{2
def getClosurelr0(lr0set):
    kernel = lr0set
    extras = []
    stack = list(lr0set)
    while len(stack):
        cur = stack.pop(0)

        if cur[1] < len(cur[0].expansion):
            after = cur[0].expansion[cur[1]]
            if type(after) == NonTerminal:
                for rule in grammar:
                    if rule.symbol == after:
                        newitem = (rule, 0)
                        if newitem not in extras and newitem not in kernel:
                            extras.append(newitem)

                        if rule.symbol != cur[0].symbol:
                            stack.append(newitem)

    return lr0tolr1(kernel, extras)
# lr0tolr1 {{{2
def lr0tolr1(kernel, extras):
    lr1set = ItemSet([])
    for kerneli in kernel:
        for follow in kerneli[0].symbol.follow:
            lr1set.kernel.append(Item(kerneli[0], kerneli[1], follow))

    for extrai in extras:
        for follow in extrai[0].symbol.follow:
            lr1set.extras.append(Item(extrai[0], extrai[1], follow))

    return lr1set
# make parser table {{{1
# find item sets {{{2
def getItemSets():
    initial = getClosurelr0([(augmentRule, 0)])

    isets = [initial]
    transitions = []

    stack = [initial]
    while len(stack):
        origset = stack.pop(0)

        afters = []
        for item in origset.items():
            after = item.getAfterDot()
            if after is not None and after not in afters:
                afters.append(after)

        for after in afters:
            newsetlr0 = []
            for item in origset.items():
                newlr0item = (item.rule, item.index + 1)
                if item.getAfterDot() == after and newlr0item not in newsetlr0:
                    newsetlr0.append(newlr0item)

            newsetlr1 = getClosurelr0(newsetlr0)
            toseti = newsetlr1.n
            if newsetlr1 not in isets:
                isets.append(newsetlr1)
                stack.append(newsetlr1)
            else:
                toseti = isets[isets.index(newsetlr1)].n
                del newsetlr1

            transitions.append((origset.n, after, toseti))

    return isets, transitions
# fill parsing table {{{2
def fillParseTable(isets, transitions):
    table = {}
    for iset in isets:
        state = State(iset.n)
        table[iset.n] = state

    for fromseti, symbol, toseti in transitions:
        state = table[fromseti]
        if type(symbol) == Terminal:
            state.setAction(symbol, ShiftAction(toseti))
        else:
            state.setGoto(symbol, toseti)

    for iset in isets:
        for item in iset.items():
            if item.getAfterDot() is None:
                state = table[iset.n]
                if item.rule.symbol != augmentSymbol:
                    state.setAction(item.lookahead, ReduceAction(item.rule))
                else:
                    state.setAction(item.lookahead, AcceptAction())

    return table
# entry function {{{2
def makeParseTable():
    return fillParseTable(*getItemSets())
# rules {{{1
_grammar = [
    {
        'symbol': 'stmt',
        'expansion': '$expr:expr',
        'name': 'statemesymbol'
    },
    {
        'symbol': 'expr',
        'expansion': '$add:expr',
        'name': 'expression'
    },

    {
        'symbol': 'add',
        'expansion': '$add:lhs PLUS:op $mult:rhs',
        'name': 'addition expression'
    },
    {
        'symbol': 'add',
        'expansion': '$add:lhs MINUS:op $mult:rhs',
        'name': 'addition expression'
    },
    {
        'symbol': 'add',
        'expansion': '$mult:_',
        'skip': True, # don't reduce this rule, only change state according to goto
        'name': 'addition expression'
    },

    {
        'symbol': 'mult',
        'expansion': '$mult:lhs STAR:op $unary:rhs',
        'name': 'multiplication expression'
    },
    {
        'symbol': 'mult',
        'expansion': '$mult:lhs SLASH:op $unary:rhs',
        'name': 'multiplication expression'
    },
    {
        'symbol': 'mult',
        'expansion': '$unary:_',
        'skip': True,
        'name': 'multiplication expression'
    },

    {
        'symbol': 'unary',
        'expansion': 'MINUS:op $unary:operand',
        'name': 'unary expression'
    },
    {
        'symbol': 'unary',
        'expansion': 'TILDE:op $unary:operand',
        'name': 'unary expression'
    },
    {
        'symbol': 'unary',
        'expansion': '$primary:_',
        'skip': True,
        'name': 'unary expression'
    },

    {
        'symbol': 'primary',
        'expansion': 'DECINTLIT:value',
        'name': 'primary expression'
    },
    {
        'symbol': 'primary',
        'expansion': 'OPARN:oparn $expr:expr CPARN:cparn',
        'name': 'primary expression'
    }
]

grammar = []
for rule in _grammar:
    symbol = rule['symbol']
    expansion = list(filter(len, rule['expansion'].split(' ')))
    for i, s in enumerate(expansion):
        sname, _ = s.split(':')
        if sname.startswith('$'):
            expansion[i] = NonTerminal(sname[1:])
        else:
            expansion[i] = Terminal(f'TokenType::{sname}')

    grammar.append(Rule(NonTerminal(symbol), tuple(expansion)))

augmentSymbol = NonTerminal('augment')
augmentRule = Rule(augmentSymbol, (grammar[0].symbol, ))
grammar.append(augmentRule)

eofSym = Terminal('TokenType::EOF_')

symbols = [eofSym]
for rule in grammar:
    for sym in [rule.symbol, *rule.expansion]:
        if type(sym) == NonTerminal:
            sym.updateFirsts()
            sym.updateFollows()

        if sym not in symbols:
            symbols.append(sym)

# make the parse table {{{1
table = makeParseTable()
# generating stuff {{{1
# print parse table {{{2
def printParseTable():
    cw = 4
    print(' ' * cw, end='')
    for sym in symbols:
        print(str(sym).rjust(cw), end='')
    print()

    for staten, state in table.items():
        print(str(staten).rjust(cw), end='')

        for sym in symbols:
            if sym in state.actions.keys():
                print(str(state.actions[sym]).rjust(cw), end='')
            elif sym in state.goto.keys():
                print(str(state.goto[sym]).rjust(cw), end='')
            else:
                print(' ' * cw, end='')

        print()
# generate getAction code {{{2
def genLoop():
    output = []

    output.append(                     '    bool done = false;\n')
    output.append(                     '    Token lookahead (consume());\n')
    output.append(                     '    std::stack<std::unique_ptr<stackitem>> stack;\n')
    output.append(                     '    stack.push(std::make_unique<stackitem>(0));\n')

    output.append(                     '    while (true)\n')
    output.append(                     '    {\n')
    output.append(                     '        switch(stack.top()->state)\n')
    output.append(                     '        {\n')

    for staten, state in table.items():
        output.append(                f'            case {staten}:\n')
        output.append(                 '               switch (lookahead.type)\n')
        output.append(                 '               {\n')

        for nt, ac in state.actions.items():
            output.append(            f'                    case {str(nt)}:\n')
            output.append(             '                        {\n')

            if type(ac) == ShiftAction:
                output.append(         '                            Token last (lookahead);\n')
                output.append(         '                            stack.push(std:make_unique<tokstackitem>(last))\n')
                output.append(         '                            lookahead = consume();\n')
            elif type(ac) == ReduceAction:
                for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                    output.append(    f'                            std::unique_ptr<stackitem> _a{i} = stack.pop();\n')
                    if type(sym) == Terminal:
                        output.append(f'                            tokstackitem *tsi{i} = dynamic_cast<tokstackitem*>(_a{i}.get());\n')
                        output.append(f'                            Token a{i} (tsi{i}->tok);\n') # TODO: add parser method to say internal error: invalid pop expected tokstackitem/aststackitem but got ...
                    elif type(sym) == NonTerminal:
                        output.append(f'                            aststackitem *asi{i} = dynamic_cast<aststackitem*>(_a{i}.get());\n')
                        output.append(f'                            std::unique_ptr<ASTNS::AST> a{i} (asi{i}->ast);\n') # same TODO as above

                output.append(        f'                            std::unique_ptr<ASTNS::AST> push = std::make_unique<ASTNS::{str(ac.rule.symbol)}>({", ".join([f"a{i}" for i in range(len(ac.rule.expansion))])});\n')
            elif type(ac) == AcceptAction:
                output.append(         '                            done = true;\n')
            else:
                raise Exception('invalid action type')

            output.append(             '                        }\n')
            output.append(             '                        break;\n')

        output.append(                 '                    default:\n')
        output.append(                ('                        Error(Error::MsgType::ERROR, lookahead, "Invalid syntax")\n'
                                       '                            .primary(Error::Primary(lookahead)\n'
                                       '                                 .error("Invalid syntax"))\n'
                                       '                             .report();\n'))
        output.append(                 '                        break;\n')
        output.append(                 '                }\n')
        output.append(                 '                break;\n')

    output.append(                     '            default:\n')
    output.append(                    ('                Error(Error::MsgType::INTERR, lookahead, "Parser reached invalid state")\n'
                                       '                    .primary(Error::Primary(lookahead)\n'
                                       '                        .error(static_cast<std::stringstream&>(std::stringstream() << "Parser reached invalid state: " << state).str()))\n'
                                       '                    .reportAbort();\n'))

    output.append(                     '        }\n')
    output.append(                     '    }\n')

    return ''.join(output)
