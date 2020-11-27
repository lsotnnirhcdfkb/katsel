#!/usr/bin/env python3

import os, re, colorama, itertools

# classes {{{1
# symbols {{{2
class NonTerminal:
    def __init__(self, symbol, name=None):
        self.symbol = symbol
        self.name = name

    def __repr__(self):
        return str(self)
    def __str__(self):
        return self.symbol

    def __eq__(self, other):
        return type(self) == type(other) and (self.symbol == other.symbol)

    def __hash__(self):
        return hash(self.symbol)

    def getName(self):
        if self.name is None:
            for sym, rule in _grammar.items():
                if sym == self.symbol:
                    self.name = rule['name']
                    break

        return self.name

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
    def __init__(self, symbol, expansion, skip, vnames, base, exhistart, exhiend):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1
        self.skip = skip
        self.vnames = vnames
        self.base = base
        self.exhistart, self.exhiend = exhistart, exhiend

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
            print(f'    : already have {self.actions[sym]} for {sym}')
            print(f'    : summary: {type(self.actions[sym])}/{type(action)} conflict: while parsing {self.whileparsing}, found {sym}, {self.actions[sym].explain()} or {action.explain()}?\033[0m')
            self.actions[sym] = None
            return False

        self.actions[sym] = action
        return True

    def setGoto(self, sym, newstate):
        if sym in self.goto.keys():
            self.goto[sym] = None
            print('goto conflict')
            return False

        self.goto[sym] = newstate
        return True

    def makeDescription(self):
        justparsed = []
        expected = []
        whileparsing = []

        for item in self.set_.kernel:
            if item.index > 0:
                s = item.rule.expansion[item.index - 1]
                if type(s) == NonTerminal:
                    justparsed.append(f'"{s.getName()}"')
                else:
                    justparsed.append(f'stringifyTokenType({s.astt()})')
            else:
                justparsed.append('"beginning"')

            after = item.getAfterDot()
            if after is not None:
                if type(after) == NonTerminal:
                    expected.extend([f'"{r.symbol.getName()}"' for r in grammar if r.symbol == after])
                else:
                    expected.append(f'stringifyTokenType({after.astt()})')
            else:
                if len(follows[item.rule.symbol]) > 4:
                    expected.extend(map(lambda nt: f'"{_grammar[str(nt)]["name"]}"', ntfollows[item.rule.symbol]))
                else:
                    expected.extend(map(lambda x: f'stringifyTokenType({x.astt()})', follows[item.rule.symbol]))

            whileparsing.append(f'"{item.rule.symbol.name}"')

        justparsed = list(set(justparsed))
        if len(set(justparsed)) > 1:
            raise Exception('More than one justparsed: ' + str(justparsed) + ' from\n'+ str(self.set_))

        assert len(justparsed), 'No justparsed'

        self.justparsed = justparsed[0]

        self.expected = list(set(expected))
        self.expected.sort()

        self.whileparsing = list(set(whileparsing))
        self.whileparsing.sort()

# actions {{{2
class ShiftAction:
    def __init__(self, newstate):
        self.newstate = newstate
    def __str__(self):
        return f's{self.newstate}'
    def __repr__(self):
        return str(self)
    def explain(self):
        return f'shift and goto state {self.newstate}'
    def __eq__(self, other):
        return type(self) == type(other) and self.newstate == other.newstate
class ReduceAction:
    def __init__(self, rule):
        self.rule = rule
    def __str__(self):
        return f'r{self.rule.num}'
    def __repr__(self):
        return str(self)
    def explain(self):
        return f'reduce rule {self.rule}'
    def __eq__(self, other):
        return type(self) == type(other) and self.rule == other.rule
class AcceptAction:
    def __str__(self):
        return 'acc'
    def __repr__(self):
        return str(self)
    def explain(self):
        return 'accept'
    def __eq__(self, other):
        return True
# helpers {{{1
def makeUnique(already, new):
    return [x for x in new if x not in already]
# first and follows functions {{{2
def findFirsts():
    global firsts
    firsts = {}

    for rule in grammar:
        firsts[rule.symbol] = []

    changed = True
    while changed:
        changed = False
        for rule in grammar:
            first = firsts[rule.symbol]
            if len(rule.expansion) == 0:
                if eofSym not in firsts:
                    first.append(eofSym)
                    changed = True
            elif rule.expansion[0] != rule.symbol:
                if type(rule.expansion[0]) == NonTerminal:
                    extension = makeUnique(first, firsts[rule.expansion[0]])
                    if len(extension):
                        first.extend(extension)
                        changed = True
                else:
                    if rule.expansion[0] not in first:
                        first.append(rule.expansion[0])
                        changed = True

def findFollows():
    global follows, ntfollows
    follows = {}
    ntfollows = {}

    for rule in grammar:
        for sym in filter(lambda x: type(x) == NonTerminal, [rule.symbol, *rule.expansion]):
            follows[sym] = []
            ntfollows[sym] = []

    follows[augmentSymbol] = [eofSym]

    changed = True
    while changed:
        changed = False
        for rule in grammar:
            for i, sym in enumerate(rule.expansion):
                if type(sym) == NonTerminal:
                    follow = follows[sym]
                    ntfollow = ntfollows[sym]

                    if i + 1 >= len(rule.expansion):
                        followextens = makeUnique(follow, follows[rule.symbol])
                        ntextens = makeUnique(ntfollow, ntfollows[rule.symbol])

                        if len(followextens):
                            follow.extend(followextens)
                            changed = True
                        if len(ntextens):
                            ntfollow.extend(ntextens)
                            changed = True

                    else:
                        if type(rule.expansion[i + 1]) == NonTerminal:
                            followextens = makeUnique(follow, [x for x in firsts[rule.expansion[i + 1]] if x != Terminal('eof')])
                            if len(followextens):
                                follow.extend(followextens)
                                changed = True

                            if rule.expansion[i + 1] not in ntfollow:
                                ntfollow.append(rule.expansion[i + 1])
                                changed = True
                        else:
                            if rule.expansion[i + 1] not in follow:
                                follow.append(rule.expansion[i + 1])
                                changed = True

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
        for follow in follows[kerneli[0].symbol]:
            lr1set.kernel.append(Item(kerneli[0], kerneli[1], follow))

    for extrai in extras:
        for follow in follows[extrai[0].symbol]:
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

    conflicts = 0

    for fromseti, symbol, toseti in transitions:
        state = table[fromseti]
        if type(symbol) == Terminal:
            if not state.setAction(symbol, ShiftAction(toseti)):
                conflicts += 1
        else:
            if not state.setGoto(symbol, toseti):
                conflicts += 1

    for iset in isets:
        for item in iset.items():
            if item.getAfterDot() is None:
                state = table[iset.n]
                if item.rule.symbol != augmentSymbol:
                    if not state.setAction(item.lookahead, ReduceAction(item.rule)):
                        conflicts += 1
                else:
                    if not state.setAction(item.lookahead, AcceptAction()):
                        conflicts += 1

    if conflicts:
        raise Exception('conflicts')
    return table
# entry function {{{2
def makeParseTable():
    print('-- finding item sets')
    isets = getItemSets()
    print('-- getting table')
    table = fillParseTable(*isets)
    return table
# rules {{{1
colorama.init()

def nt(sym, name, base):
    assert sym not in _grammar, f'redefinition of symbol {sym} in grammar'
    _grammar[sym] = {
        'name': name,
        'base': base,
        'expansions': []
    }

def rule(sym, expansion, histart='BEGIN', hiend='END'):
    assert sym in _grammar, f'defining an expansion for a symbol {sym} that is not in the grammar (yet)'
    _grammar[sym]['expansions'].append((expansion, histart, hiend))

def listRule(sym, name, base, delimit=None):
    symlist = sym + 'List'
    nt(symlist, name + ' list', base)
    rule(symlist, f'${sym}List:{sym.lower()}list {f"{delimit}:{delimit.lower()}" if delimit is not None else ""} ${sym}:{sym.lower()}')
    rule(symlist, f'${sym}:{sym.lower()}')

_grammar = {}

listRule('Decl', 'declaration', 'DeclB')
nt('Decl', 'declaration', 'DeclB')
rule('Decl', '$Function:_')

nt('Function', 'function declaration', 'DeclB')
rule('Function', 'FUN:fun $TypeV:retty IDENTIFIER:name OPARN:oparn                      CPARN:cparn $Block:body', 'fun', 'cparn')
rule('Function', 'FUN:fun $TypeV:retty IDENTIFIER:name OPARN:oparn $ParamList:paramlist CPARN:cparn $Block:body', 'fun', 'cparn')

listRule('Stmt', 'statement', 'StmtB')
nt('Stmt', 'statement', 'StmtB')
rule('Stmt', '$EmptyStmt:_')
rule('Stmt', '$VarStmt:_')
rule('Stmt', '$ExprStmt:_')
rule('Stmt', '$RetStmt:_')
rule('Stmt', '$Block:_')

nt('VarStmt', 'variable statement', 'StmtB')
rule('VarStmt', 'VAR:var $TypeNV:type $VarStmtItemList:assignments SEMICOLON:semi')

nt('ExprStmt', 'expression statement', 'StmtB')
rule('ExprStmt', '$Expr:expr SEMICOLON:semi')

nt('RetStmt', 'return statement', 'StmtB')
rule('RetStmt' , 'RETURN:ret $Expr:expr SEMICOLON:semi')
rule('RetStmt', 'RETURN:ret SEMICOLON:semi')

nt('EmptyStmt', 'empty statement', 'StmtB')
rule('EmptyStmt', 'SEMICOLON:semi')

listRule('VarStmtItem', 'variable statement initialization', 'VStmtIB', 'COMMA')
nt('VarStmtItem', 'variable statement initialization', 'VStmtIB')
rule('VarStmtItem', 'IDENTIFIER:name EQUAL:equal $Expr:expr')
rule('VarStmtItem', 'IDENTIFIER:name')

nt('Block', 'code block', 'StmtB')
rule('Block', 'OCURB:ocurb $StmtList:stmts CCURB:ccurb')
rule('Block', 'OCURB:ocurb CCURB:ccurb')

nt('TypeNV', 'non-void type specifier', 'TypeB')
rule('TypeNV', '$BuiltinTypeNoVoid:_')

nt('TypeV', 'void-inclusive type specifier', 'TypeB')
rule('TypeV', '$BuiltinTypeVoid:_')

nt('BuiltinTypeVoid', 'void-inclusive builtin type specifier', 'TypeB')
rule('BuiltinTypeVoid', '$BuiltinTypeNoVoid:_')
rule('BuiltinTypeVoid', 'VOID:type')

nt('BuiltinTypeNoVoid', 'non-void builtin type specifier', 'TypeB')
rule('BuiltinTypeNoVoid', 'UINT8:type')
rule('BuiltinTypeNoVoid', 'UINT16:type')
rule('BuiltinTypeNoVoid', 'UINT32:type')
rule('BuiltinTypeNoVoid', 'UINT64:type')
rule('BuiltinTypeNoVoid', 'SINT8:type')
rule('BuiltinTypeNoVoid', 'SINT16:type')
rule('BuiltinTypeNoVoid', 'SINT32:type')
rule('BuiltinTypeNoVoid', 'SINT64:type')
rule('BuiltinTypeNoVoid', 'FLOAT:type')
rule('BuiltinTypeNoVoid', 'BOOL:type')
rule('BuiltinTypeNoVoid', 'DOUBLE:type')
rule('BuiltinTypeNoVoid', 'CHAR:type')

listRule('Arg', 'argument', 'ArgB', 'COMMA')
nt('Arg', 'argument', 'ArgB')
rule('Arg', '$Expr:expr')

listRule('Param', 'parameter', 'PListB', 'COMMA')
nt('Param', 'parameter', 'PListB')
rule('Param', '$TypeNV:type IDENTIFIER:name')

nt('Expr', 'expression', 'ExprB')
rule('Expr', '$AssignmentExpr:_')
nt('AssignmentExpr', 'assignment expression', 'ExprB')
rule('AssignmentExpr', '$TernaryExpr:target EQUAL:equal $AssignmentExpr:value')
rule('AssignmentExpr', '$TernaryExpr:_')
nt('TernaryExpr', 'ternary expression', 'ExprB')
rule('TernaryExpr', '$BinorExpr:_')
rule('TernaryExpr', '$BinorExpr:cond QUESTION:quest $Expr:trues COLON:colon $TernaryExpr:falses')
nt('BinorExpr', 'binary or expression', 'ExprB')
rule('BinorExpr', '$BinorExpr:lhs DOUBLEPIPE:op $BinandExpr:rhs')
rule('BinorExpr', '$BinandExpr:_')
nt('BinandExpr', 'binary and expression', 'ExprB')
rule('BinandExpr', '$BinandExpr:lhs DOUBLEAMPER:op $CompeqExpr:rhs')
rule('BinandExpr', '$CompeqExpr:_')
nt('CompeqExpr', 'equality expression', 'ExprB')
rule('CompeqExpr', '$CompeqExpr:lhs BANGEQUAL:op $ComplgtExpr:rhs')
rule('CompeqExpr', '$CompeqExpr:lhs DOUBLEEQUAL:op $ComplgtExpr:rhs')
rule('CompeqExpr', '$ComplgtExpr:_')
nt('ComplgtExpr', 'comparison expression', 'ExprB')
rule('ComplgtExpr', '$ComplgtExpr:lhs LESS:op $BitxorExpr:rhs')
rule('ComplgtExpr', '$ComplgtExpr:lhs GREATER:op $BitxorExpr:rhs')
rule('ComplgtExpr', '$ComplgtExpr:lhs LESSEQUAL:op $BitxorExpr:rhs')
rule('ComplgtExpr', '$ComplgtExpr:lhs GREATEREQUAL:op $BitxorExpr:rhs')
rule('ComplgtExpr', '$BitxorExpr:_')
nt('BitxorExpr', 'bitwise xor expression', 'ExprB')
rule('BitxorExpr', '$BitxorExpr:lhs CARET:op $BitorExpr:rhs')
rule('BitxorExpr', '$BitorExpr:_')
nt('BitorExpr', 'bitwise or expression', 'ExprB')
rule('BitorExpr', '$BitorExpr:lhs PIPE:op $BitandExpr:rhs')
rule('BitorExpr', '$BitandExpr:_')
nt('BitandExpr', 'bitwise and expression', 'ExprB')
rule('BitandExpr', '$BitandExpr:lhs AMPER:op $BitshiftExpr:rhs')
rule('BitandExpr', '$BitshiftExpr:_')
nt('BitshiftExpr', 'bit shift expression', 'ExprB')
rule('BitshiftExpr', '$BitshiftExpr:lhs DOUBLEGREATER:op $AdditionExpr:rhs')
rule('BitshiftExpr', '$BitshiftExpr:lhs DOUBLELESS:op $AdditionExpr:rhs')
rule('BitshiftExpr', '$AdditionExpr:_')
nt('AdditionExpr', 'addition expression', 'ExprB')
rule('AdditionExpr', '$AdditionExpr:lhs PLUS:op $MultExpr:rhs')
rule('AdditionExpr', '$AdditionExpr:lhs MINUS:op $MultExpr:rhs')
rule('AdditionExpr', '$MultExpr:_')
nt('MultExpr', 'multiplication expression', 'ExprB')
rule('MultExpr', '$MultExpr:lhs STAR:op $UnaryExpr:rhs')
rule('MultExpr', '$MultExpr:lhs SLASH:op $UnaryExpr:rhs')
rule('MultExpr', '$MultExpr:lhs PERCENT:op $UnaryExpr:rhs')
rule('MultExpr', '$UnaryExpr:_')
nt('UnaryExpr', 'unary expression', 'ExprB')
rule('UnaryExpr', 'TILDE:op $UnaryExpr:operand')
rule('UnaryExpr', 'MINUS:op $UnaryExpr:operand')
rule('UnaryExpr', 'BANG:op $UnaryExpr:operand')
rule('UnaryExpr', '$CallExpr:_')
nt('CallExpr', 'function call expression', 'ExprB')
rule('CallExpr', '$PrimaryExpr:callee OPARN:oparn $ArgList:args CPARN:cparn')
rule('CallExpr', '$PrimaryExpr:callee OPARN:oparn CPARN:cparn')
rule('CallExpr', '$PrimaryExpr:_')
nt('PrimaryExpr', 'primary expression', 'ExprB')
rule('PrimaryExpr', 'TRUELIT:value')
rule('PrimaryExpr', 'FALSELIT:value')
rule('PrimaryExpr', 'FLOATLIT:value')
rule('PrimaryExpr', 'NULLPTRLIT:value')
rule('PrimaryExpr', 'DECINTLIT:value')
rule('PrimaryExpr', 'OCTINTLIT:value')
rule('PrimaryExpr', 'BININTLIT:value')
rule('PrimaryExpr', 'HEXINTLIT:value')
rule('PrimaryExpr', 'CHARLIT:value')
rule('PrimaryExpr', 'STRINGLIT:value')
rule('PrimaryExpr', 'IDENTIFIER:value')
rule('PrimaryExpr', 'OPARN:oparn $Expr:expr CPARN:cparn')

# convert grammar {{{1
grammar = []
found = set()
missing = set()

for sym, rule in _grammar.items():
    if sym in missing:
        missing.remove(sym)

    found.add(sym)

    expansions = rule['expansions']

    base = rule['base']
    for expansion, exhistart, exhiend in expansions:
        vnames = []
        expansion = expansion.split(' ')

        convertedexpansion = []

        for i, s in enumerate(expansion):
            if len(s) == 0:
                continue

            try:
                sname, vname = s.split(':')
                vnames.append(vname)
            except:
                print(f'\033[1mrule: {sym} -> {rule["expansions"]}\033[0m')
                print(f'\033[1m{repr(s)}\033[0m')
                raise

            if sname.startswith('$'):
                convertedexpansion.append(NonTerminal(sname[1:]))
                if sname[1:] not in found:
                    missing.add(sname[1:])
            else:
                convertedexpansion.append(Terminal(f'TokenType::{sname}'))
                if sname != sname.upper():
                    print(f'\033[35;1mwarning\033[0m: terminal {sname} in rule \033[1m{sym} -> {expansion}\033[0m')

        if len(convertedexpansion) == 1 and type(convertedexpansion[0]) == NonTerminal and _grammar[str(convertedexpansion[0])]['base'] == base:
            skip = True
        else:
            skip = False

        grammar.append(Rule(NonTerminal(sym, rule['name']), tuple(convertedexpansion), skip, vnames, base, exhistart, exhiend))

for missingi in missing:
    print(f'\033[35;1mwarning\033[0m: undefined nonterminal \033[1m{missingi}\033[0m')

augmentSymbol = NonTerminal('augment', 'compilation unit')
augmentRule = Rule(augmentSymbol, (grammar[0].symbol, ), True, '_', '', 'START', 'END')
grammar.append(augmentRule)

eofSym = Terminal('TokenType::EOF_')

symbols = [eofSym]
for rule in grammar:
    for sym in [rule.symbol, *rule.expansion]:
        if sym not in symbols:
            symbols.append(sym)

print('-- finding first and follows')
findFirsts()
findFollows()

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
        return f'{l[0]}'
    elif len(l) == 2:
        return f'concatMsg("either ", {l[0]}, " or ", {l[1]})'
    elif len(l) == 0:
        return '"nothing"'
    else:
        return 'concatMsg(' + ', '.join(l[:-1]) + ', " or ", ' + l[-1] + ')'
# }}}
def genLoop():
    output = []

    output.append(                    ('#define ERRORSTART() \\\n'
                                       '    {\\\n'
                                       '        if (istrial) return false;\n'
                                       '#define ERROREND() \\\n'
                                       '            done = true;\\\n'
                                       '        errored = true;\\\n'
                                       '    }\\\n'
                                       '    break;\n'
                                       '#define DEFAULTINVALIDWHILE(justparsed, expected, whileparsing) \\\n'
                                       '    ERRORSTART()\\\n'
                                       '        if (!errorRecovery(errorstate(p, stack, lookahead, lasttok, justparsed, expected, whileparsing)))\\\n'
                                       '    ERROREND()\n'
                                       '#define DEFAULTINVALIDNOWHILE(justparsed, expected) \\\n'
                                       '    ERRORSTART()\\\n'
                                       '        if (!errorRecovery(errorstate(p, stack, lookahead, lasttok, justparsed, expected, \"\")))\\\n'
                                       '    ERROREND()\n'))

    output.append(                     '    bool done = false;\n')
    output.append(                     '    bool errored = false;\n')
    output.append(                     '    int steps = 0;\n')
    output.append(                     '    Token lookahead (_lookahead); // for when you need to inject a new token\n')
    output.append(                     '    Token lasttok = lookahead;\n')

    output.append(                     '    while (!done)\n')
    output.append(                     '    {\n')
    output.append(                     '        if (istrial && steps > 5)\n')
    output.append(                     '            return true;\n')
    output.append(                     '        switch (stack.back().state)\n')
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

        reduceOnly = len([ac for ac in stateactions if type(ac[0]) == ReduceAction]) == 1
        for ac, nts in stateactions:
            if type(ac) == ShiftAction:
                for term in nts:
                    output.append(    f'                    case {term.astt()}:\n')
                output.append(        f'                        shift(p, lasttok, lookahead, stack, steps, {ac.newstate}); break;\n')
                continue

            if reduceOnly:
                output.append(        f'                    default:\n')
                # do not check for lookahead, just reduce to have better performance
                # if reduceOnly, then all the reduce actions of this state reduce the same rule
                # and according to Wikipedia, just reducing regardless of the lookahead in
                # these states will cause a few "harmless reductions," and errors will just be
                # reported after a few reduces
                # this actually helps with error reporting because if you have "return 2",
                # it will reduce 2 up the chain of expression precedence before reporting the error
                # so the error message is "expected ';' after expression of return statement"
                # wheras if it didnt reduce, you would get "invalid token to follow 2 of primary expression"
                # which used to be the erorr format if there was an invalid lookahead token for a state that didn't have any shift actions
            else:
                for term in nts:
                    output.append(    f'                    case {term.astt()}:\n')

            if type(ac) == ReduceAction:
                if not ac.rule.skip:
                    output.append(         '                        {\n')

                    for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                        if type(sym) == Terminal:
                            output.append(f'                            auto a{i} (popT(stack));\n')
                        elif type(sym) == NonTerminal:
                            output.append(f'                            auto a{i} (popA<ASTNS::{str(sym)}>(stack));\n')

                    if not len(ac.rule.expansion):
                        output.append(        f'                            std::unique_ptr<ASTNS::AST> push (nullptr);\n')
                    else:
                        output.append(        f'                            std::unique_ptr<ASTNS::AST> push (std::make_unique<ASTNS::{str(ac.rule.symbol)}>({", ".join([f"std::move(a{i})" for i in range(len(ac.rule.expansion))])}));\n')

                    output.append(        f'                            size_t newstate = getGoto<ASTNS::{str(ac.rule.symbol)}>(stack.back().state);\n')
                    output.append(        f'                            stack.emplace_back(newstate, std::move(push));\n')
                    output.append(         '                        }\n')
                else:
                    output.append(        f'                        reduceSkip<ASTNS::{str(ac.rule.symbol)}>(stack);\n')


            elif type(ac) == AcceptAction:
                output.append(         '                            done = true;\n')
            else:
                raise Exception('invalid action type')

            output.append(             '                        break;\n')

        if not reduceOnly:
            output.append(            f'                    default:\n')
            if len(state.expected):
                if len(state.whileparsing) == 1:
                    output.append(    f'                        DEFAULTINVALIDWHILE({state.justparsed}, {formatList(state.expected)}, {formatList(state.whileparsing)})\n')
                else:
                    output.append(    f'                        DEFAULTINVALIDNOWHILE({state.justparsed}, {formatList(state.expected)})\n')
            else:
                print(state.actions)
                raise Exception('no expect for non-reduceOnly state')
        output.append(                 '                }\n')
        output.append(                 '                break;\n')

    output.append(                     '            default:\n')
    output.append(                     '                reportAbortNoh(concatMsg("Parser reached invalid state: ", stack.back().state));\n')

    output.append(                     '        }\n')
    output.append(                     '    }\n')
    output.append(                    ('#undef ERRORSTART\n'
                                       '#undef ERROREND\n'))

    return ''.join(output)
# generate goto code {{{2
def genGoto():
    output = []

    for nonterm in symbols:
        if type(nonterm) == Terminal:
            continue

        if nonterm == augmentSymbol:
            continue

        output.append(        f'template <> size_t getGoto<ASTNS::{str(nonterm)}>(size_t state)\n')
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

# generate panic mode error recovery code {{{2
def genPanicMode():
    output = []
    output.append(        ('#define CHECKASI(ty)\\\n'
                           '    ASTNS::ty *ast##ty (dynamic_cast<ASTNS::ty*>(ast));\\\n'
                           '    if (ast##ty)\\\n'
                           '    {\\\n'
                           '        switch (e.lookahead.type)\\\n'
                           '        {\n'
                           '#define FINISHCHECKASI()\\\n'
                           '        }\\\n'
                           '    }\n'
                           '#define RECOVERANDDEFBREAK()\\\n'
                           '        valid = true;\\\n'
                           '        delto = i;\\\n'
                           '        break;\\\n'
                           '    default:\\\n'
                           '        break;\n'
                           '    bool valid = false;\n'
                           '    std::vector<stackitem>::reverse_iterator delto;\n'
                           '    while (!valid)\n'
                           '    {\n'
                           '        for (auto i = e.stack.rbegin(); i != e.stack.rend() && !valid; ++i)\n'
                           '        {\n'
                           '            if (!i->istok && !i->isinitial)\n'
                           '            {\n'
                           '                ASTNS::AST *ast = i->ast.get();\n'))

    for nonterm in symbols:
        if type(nonterm) == Terminal:
            continue
        if nonterm == augmentSymbol:
            continue

        output.append(    f'                CHECKASI({str(nonterm)})\n')
        output.append(     '                       ')
        for follow in follows[nonterm]:
            output.append(f' case {follow.astt()}:')
        output.append(     '\n')
        output.append(     '                            RECOVERANDDEFBREAK()\n')
        output.append(     '                FINISHCHECKASI()\n')

    output.append(        ('            }\n'
                           '        }\n'
                           '        if (!valid)\n'
                           '            e.lookahead = e.p.consume();\n'
                           '        if (e.lookahead.type == TokenType::EOF_)\n'
                           '            return false;\n'
                           '    }\n'
                           '    e.stack.erase(delto.base(), e.stack.end());\n'
                           '#undef CHECKASI\n'
                           '#undef FINISHCHECKASI\n'
                           '#undef RECOVERANDDEFBREAK\n'
                           '    if (e.w)\n'
                           '        ERR_PANICKING_INVALID_SYNTAX_WHILE(e.justparsed, e.expected, e.whileparsing, e.lasttok, e.lookahead);\n'
                           '    else\n'
                           '        ERR_PANICKING_INVALID_SYNTAX(e.justparsed, e.expected, e.lasttok, e.lookahead);\n'
                           '    return true;\n'))

    return ''.join(output)
# generate single token insertion/deletion/substitution error recovery code {{{2
def genSingleTok():
    output = []
    output.append(              '#define TRYINSERT(ty) if (tryInsert(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::INSERT, ty}; if (score(f) > score(bestfix)) bestfix = f;}\n')
    output.append(              '#define TRYSUB(ty) if (trySub(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::SUBSTITUTE, ty}; if (score(f) > score(bestfix)) bestfix = f;}\n')
    output.append(              '#define TRYTOKTY(ty) TRYINSERT(ty); TRYSUB(ty);\n')

    for terminal in symbols:
        if type(terminal) == Terminal:
            output.append(     f'    TRYTOKTY({terminal.astt()})\n');

    output.append(              '    if (tryDel(e.p, e.stack)) {fix f = fix {fix::fixtype::REMOVE}; if (score(f) > score(bestfix)) bestfix = f;};\n')
    return ''.join(output)
# entry {{{1
if __name__ == '__main__':
    printParseTable(False)
