#!/usr/bin/env python3

import os, re, colorama, itertools

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
        self.follow, self.ntfollow = getFollows(self)

class Terminal:
    def __init__(self, symbol):
        self.symbol = symbol

    def __repr__(self):
        return str(self)
    def __str__(self):
        return self.symbol[len('TokenType::'):]

    def astt(self):
        return self.symbol

    def __hash__(self):
        return hash(self.symbol)

    def __eq__(self, other):
        return type(self) == type(other) and (self.symbol == other.symbol)
# rule {{{2
class Rule:
    __num = 0
    def __init__(self, symbol, expansion, skip, name):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1
        self.skip = skip
        self.name = name

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
        self.seti = self.set_.n
        self.actions = {}
        self.goto = {}

        self.makeDescription()

    def setAction(self, sym, action):
        if sym in self.actions.keys():
            print(f'\033[1maction table conflict: {type(self.actions[sym])}/{type(action)}')
            print(f'    : in state {self.seti}')
            print(f'    : with set {self.set_}')
            print(f'    : keys {self.actions.keys()}')
            print(f'    : adding {action} for {sym}')
            print(f'    : already have {self.actions[sym]} for {sym}\033[0m')
            raise Exception(f'action table conflict')

        self.actions[sym] = action

    def setGoto(self, sym, newstate):
        if sym in self.goto.keys():
            raise Exception('goto conflict')

        self.goto[sym] = newstate

    def makeDescription(self):
        justparsed = []
        expected = []
        whileparsing = []

        for item in self.set_.kernel:
            if item.index > 0:
                s = item.rule.expansion[item.index - 1]
                if type(s) == NonTerminal:
                    justparsed.extend([r.name for r in grammar if r.symbol == s])
                else:
                    justparsed.append(str(s))
            else:
                justparsed.append('beginning')

            after = item.getAfterDot()
            if after is not None:
                if type(after) == NonTerminal:
                    expected.extend([r.name for r in grammar if r.symbol == after])
                else:
                    expected.append(str(after))
            else:
                expected.extend(itertools.chain.from_iterable(map(lambda nt: [r.name for r in grammar if r.symbol == nt], item.rule.symbol.ntfollow)))

            whileparsing.append(item.rule.name)

        if len(set(justparsed)) > 1:
            raise Exception(self.set_)

        self.justparsed = justparsed[0]
        self.expected = list(set(expected))
        self.expected.sort()
        if len(set(whileparsing)) == 1:
            self.whileparsing = whileparsing[0]
        else:
            self.whileparsing = None

# actions {{{2
class ShiftAction:
    def __init__(self, newstate):
        self.newstate = newstate
    def __str__(self):
        return f's{self.newstate}'
    def __eq__(self, other):
        return type(self) == type(other) and self.newstate == other.newstate
class ReduceAction:
    def __init__(self, rule):
        self.rule = rule
    def __str__(self):
        return f'r{self.rule.num}'
    def __eq__(self, other):
        return type(self) == type(other) and self.rule == other.rule
class AcceptAction:
    def __str__(self):
        return 'acc'
    def __eq__(self, other):
        return True
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
                    if len(rule.expansion[0].first):
                        firsts.extend(makeUnique(firsts, rule.expansion[0].first))
                    else:
                        firsts.extend(makeUnique(firsts, getFirsts(rule.expansion[0])))
                else:
                    if rule.expansion[0] not in firsts:
                        firsts.append(rule.expansion[0])

    return firsts
def getFollows(sym):
    if sym == augmentSymbol:
        return [eofSym], []

    follows = []
    ntfollows = []
    for rule in grammar:
        for i, gsym in enumerate(rule.expansion):
            if gsym != sym:
                continue

            if i + 1 >= len(rule.expansion):
                if rule.symbol != sym:
                    if len(rule.symbol.follow):
                        follows.extend(makeUnique(follows, rule.symbol.follow))
                        ntfollows.extend(makeUnique(ntfollows, rule.symbol.ntfollow))
                    else:
                        newfollows, newntfollows = getFollows(rule.symbol)
                        follows.extend(makeUnique(follows, newfollows))
                        ntfollows.extend(makeUnique(follows, newntfollows))
            else:
                if type(rule.expansion[i + 1]) == NonTerminal:
                    if len(rule.expansion[i + 1].first):
                        follows.extend(makeUnique(follows, [x for x in rule.expansion[i + 1].first if x != Terminal('eof')]))
                    else:
                        follows.extend(makeUnique(follows, [x for x in getFirsts(rule.expansion[i + 1]) if x != Terminal('eof')]))
                    ntfollows.append(rule.expansion[i + 1])
                else:
                    if rule.expansion[i + 1] not in follows:
                        follows.append(rule.expansion[i + 1])

    return follows, ntfollows
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
        state = State(iset)
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
    isets = getItemSets()
    print('-- get item sets')
    table = fillParseTable(*isets)
    print('-- get table')
    return table
# rules {{{1
with open(os.path.join(os.path.dirname(os.path.realpath(__file__)), 'grammar'), 'r') as f:
    grammarstr = f.read()

colorama.init()

_grammar = []
rulere = re.compile(r'\s*(\w+)\s*->\s*([^;]+);\s*"([^"]+)"\s*')
for rule in filter(len, grammarstr.split('\n')):
    if (match := rulere.fullmatch(rule)) is None:
        raise Exception(f'rule "{rule}" does not match rulere')

    sym = match.group(1)
    expansion = match.group(2).strip().rstrip()
    name = match.group(3).strip().rstrip()

    r = {}
    r['symbol'] = sym
    r['expansion'] = expansion
    r['name'] = name

    _grammar.append(r)

grammar = []
found = set()
missing = set()
for rule in _grammar:
    symbol = rule['symbol']
    if symbol in missing:
        missing.remove(symbol)
        found.add(symbol)

    expansion = list(filter(len, rule['expansion'].split(' ')))
    for i, s in enumerate(expansion):
        try:
            sname, _ = s.split(':')
        except:
            print(f'\033[1mrule: {rule["symbol"]} -> {rule["expansion"]}\033[0m')
            raise

        if sname.startswith('$'):
            expansion[i] = NonTerminal(sname[1:])
            if sname[1:] not in found and sname[1:] not in missing:
                missing.add(sname[1:])
        else:
            expansion[i] = Terminal(f'TokenType::{sname}')
            if sname.lower() == sname:
                print(f'\033[35;1mwarning\033[0m: terminal {sname} in rule \033[1m{symbol} -> {expansion}\033[0m')

    if len(expansion) == 1 and type(expansion[0]) == NonTerminal:
        skip = True
        print('\033[34mrule is skip\033[0m', rule['symbol'], rule['expansion'])
    else:
        skip = False

    grammar.append(Rule(NonTerminal(symbol), tuple(expansion), skip, rule['name']))

for missingi in missing:
    print(f'\033[35;1mwarning\033[0m: undefined terminal \033[1m{missingi}\033[0m')
print('-- parsed grammar')

augmentSymbol = NonTerminal('augment')
augmentRule = Rule(augmentSymbol, (grammar[0].symbol, ), False, 'compilation unit')
grammar.append(augmentRule)

print('-- augment grammar')

eofSym = Terminal('TokenType::EOF_')

symbols = [eofSym]
for rule in grammar:
    for sym in [rule.symbol, *rule.expansion]:
        if type(sym) == NonTerminal:
            sym.updateFirsts()
            sym.updateFollows()

        if sym not in symbols:
            symbols.append(sym)

print('-- got first and follows')

# make the parse table {{{1
table = makeParseTable()
# generating stuff {{{1
# print parse table {{{2
def printParseTable(pad=True):
    cw = 4
    if pad:
        padf = lambda s: s.rjust(cw)
    else:
        padf = lambda s: s + ' '
    print(padf('_'), end='')
    for sym in symbols:
        print(padf(str(sym)), end='')
    print()

    for staten, state in table.items():
        print(padf(str(staten)), end='')

        for sym in symbols:
            if sym in state.actions.keys():
                print(padf(str(state.actions[sym])), end='')
            elif sym in state.goto.keys():
                print(padf(str(state.goto[sym])), end='')
            else:
                print(padf('_'), end='')

        print()
# generate parser loop code {{{2
# helper {{{
def formatList(l):
    if len(l) == 1:
        return l[0]
    elif len(l) == 2:
        return f'either {l[0]} or {l[1]}'
    elif len(l) == 0:
        return 'nothing'
    else:
        return ', '.join(l[:-1]) + ', or ' + l[-1]
# }}}
def genLoop():
    output = []

    output.append(                    ('#define SHIFT(newstate) \\\n'
                                       '    lasttok = lookahead;\\\n'
                                       '    stack.push(std::make_unique<tokstackitem>(newstate, lasttok));\\\n'
                                       '    lookahead = consume();\n'
                                       '#define REDUCET(n) \\\n'
                                       '    std::unique_ptr<stackitem> _a ## n = std::move(stack.top()); stack.pop();\\\n'
                                       '    tokstackitem *si ## n = dynamic_cast<tokstackitem*>(_a ## n .get());\\\n'
                                       '    Token a ## n (si ## n ->tok);\n' # TODO: add parser method to say internal error: invalid pop expected tokstackitem/aststackitem but got ...
                                       '#define REDUCEA(n) \\\n'
                                       '    std::unique_ptr<stackitem> _a ## n = std::move(stack.top()); stack.pop();\\\n'
                                       '    aststackitem *si ## n = dynamic_cast<aststackitem*>(_a ## n .get());\\\n'
                                       '    std::unique_ptr<ASTNS::AST> a ## n (std::move(si ## n ->ast));\n' # same TODO as above
                                       '#define SHIFTON(ty, n) \\\n'
                                       '    case ty: \\\n'
                                       '        {SHIFT(n)} break;\n'
                                       '#define DEFAULTINVALID2(justparsed, expected) \\\n'
                                       '    default: \\\n'
                                       '        invalidSyntax(justparsed, expected, lookahead, lasttok);\\\n'
                                       '        done = true;\\\n'
                                       '        break;\n'
                                       '#define DEFAULTINVALID3(justparsed, expected, whileparsing) \\\n'
                                       '    default: \\\n'
                                       '        invalidSyntax(justparsed, expected, whileparsing, lookahead, lasttok);\\\n'
                                       '        done = true;\\\n'
                                       '        break;\n'
                                       '#define REDUCESKIP(cl) \\\n'
                                       '    {\\\n'
                                       '        std::unique_ptr<stackitem> popped (std::move(stack.top())); stack.pop();\\\n'
                                       '        aststackitem *asi = dynamic_cast<aststackitem*>(popped.get());\\\n'
                                       '        size_t newstate = getGoto<ASTNS::cl>(stack.top()->state);\\\n'
                                       '        stack.push(std::make_unique<aststackitem>(newstate, std::move(asi->ast)));\\\n'
                                       '    }\n'))

    output.append(                     '    bool done = false;\n')
    output.append(                     '    Token lookahead (consume());\n')
    output.append(                     '    Token lasttok = lookahead;\n')
    output.append(                     '    std::stack<std::unique_ptr<stackitem>> stack;\n')
    output.append(                     '    stack.push(std::make_unique<stackitem>(0));\n')

    output.append(                     '    while (!done)\n')
    output.append(                     '    {\n')
    output.append(                     '        switch (stack.top()->state)\n')
    output.append(                     '        {\n')

    for staten, state in sorted(table.items(), key=lambda x:x[0]):
        output.append(                f'            case {staten}:\n')
        output.append(                 '               switch (lookahead.type)\n')
        output.append(                 '               {\n')

        stateactions = []
        for term, ac in sorted(state.actions.items(), key=lambda x:str(x[0])):
            found = False
            for i, (ac2, _) in enumerate(stateactions):
                if ac == ac2:
                    stateactions[i][1].append(term)
                    found = True
                    break

            if not found:
                stateactions.append((ac, [term]))


        for ac, nts in stateactions:
            if type(ac) == ShiftAction:
                for term in nts:
                    output.append(    f'                    SHIFTON({term.astt()}, {ac.newstate})\n')
                continue

            for term in nts:
                output.append(        f'                    case {term.astt()}:\n')

            if type(ac) == ReduceAction:
                if not ac.rule.skip:
                    output.append(         '                        {\n')

                    for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                        if type(sym) == Terminal:
                            output.append(f'                            REDUCET({i})\n')
                        elif type(sym) == NonTerminal:
                            output.append(f'                            REDUCEA({i})\n')

                    output.append(        f'                            std::unique_ptr<ASTNS::AST> push = std::make_unique<ASTNS::{str(ac.rule.symbol).capitalize()}>({", ".join([f"std::move(a{i})" for i in range(len(ac.rule.expansion))])});\n')
                    output.append(        f'                            size_t newstate = getGoto<ASTNS::{str(ac.rule.symbol).capitalize()}>(stack.top()->state);\n')
                    output.append(        f'                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));\n')
                    output.append(         '                        }\n')
                else:
                    output.append(        f'                        REDUCESKIP({str(ac.rule.symbol).capitalize()});\n')


            elif type(ac) == AcceptAction:
                output.append(         '                            done = true;\n')
            else:
                raise Exception('invalid action type')

            output.append(             '                        break;\n')

        if state.whileparsing is not None:
            output.append(            f'                    DEFAULTINVALID3("{state.justparsed}", "{formatList(state.expected)}", "{state.whileparsing}")\n')
        else:
            output.append(            f'                    DEFAULTINVALID2("{state.justparsed}", "{formatList(state.expected)}")\n')
        output.append(                 '                }\n')
        output.append(                 '                break;\n')

    output.append(                     '            default:\n')
    output.append(                    ('                Error(Error::MsgType::INTERR, lookahead, "Parser reached invalid state")\n'
                                       '                    .underline(Error::Underline(lookahead, \'!\')\n'
                                       '                        .error(concatMsg("Parser reached invalid state: ", stack.top()->state)))\n'
                                       '                    .reportAbort();\n'))

    output.append(                     '        }\n')
    output.append(                     '    }\n')
    output.append(                    ('#undef SHIFT\n'
                                       '#undef REDUCET\n'
                                       '#undef REDUCEA\n'
                                       '#undef REDUCESKIP\n'
                                       '#undef SHIFTON\n'
                                       '#undef DEFAULTINVALID2\n'
                                       '#undef DEFAULTINVALID3\n'))

    return ''.join(output)
# generate goto code {{{2
def genGoto():
    output = []

    for nonterm in symbols:
        if type(nonterm) == Terminal:
            continue

        if nonterm == augmentSymbol:
            continue

        output.append(        f'template <> size_t Parser::getGoto<ASTNS::{str(nonterm).capitalize()}>(size_t state)\n')
        output.append(         '{\n')
        output.append(         '    switch (state)\n')
        output.append(         '    {\n')

        returns = {}
        for staten, state in table.items():
            if nonterm in state.goto:
                if state.goto[nonterm] in returns: # there is already a state in which this goto is returned (squishing rows together)
                    returns[state.goto[nonterm]].append(staten)
                else:
                    returns[state.goto[nonterm]] = [staten]

        for retval, states in returns.items():
            for state in states:
                output.append(f'        case {state}:\n')
            output.append(    f'            return {retval};\n')

        output.append(         '        default:\n')
        output.append(        f'            reportAbortNoh("retrieve goto of nonterminal {str(nonterm)} in invalid state");\n')
        output.append(         '    }\n')
        output.append(         '}\n')

    return ''.join(output)

# entry {{{1
if __name__ == '__main__':
    printParseTable(False)
