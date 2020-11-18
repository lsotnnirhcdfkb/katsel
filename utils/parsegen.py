#!/usr/bin/env python3

import os, re, colorama, itertools

# classes {{{1
# symbols {{{2
class NonTerminal:
    def __init__(self, symbol, name):
        self.symbol = symbol
        self.first = []
        self.follow = []
        self.name = name

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
    def __init__(self, symbol, expansion, skip, vnames, base):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1
        self.skip = skip
        self.vnames = vnames
        self.base = base

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
                    justparsed.append(f'"{_grammar[str(s)]["name"]}"')
                else:
                    justparsed.append(f'stringifyTokenType({s.astt()})')
            else:
                justparsed.append('"beginning"')

            after = item.getAfterDot()
            if after is not None:
                if type(after) == NonTerminal:
                    expected.extend([f'"{r.symbol.name}"' for r in grammar if r.symbol == after])
                else:
                    expected.append(f'stringifyTokenType({after.astt()})')
            else:
                if len(item.rule.symbol.follow) > 4:
                    expected.extend(map(lambda nt: f'"{_grammar[str(nt)]["name"]}"', item.rule.symbol.ntfollow))
                else:
                    expected.extend(map(lambda x: f'stringifyTokenType({x.astt()})', item.rule.symbol.follow))

            whileparsing.append(f'"{item.rule.symbol.name}"')

        if len(set(justparsed)) > 1:
            raise Exception(self.set_)

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
colorama.init()

def rule(sym, name, base, *expansions):
    if sym in _grammar:
        raise Exception(f'duplicate rule symbol {sym}')

    _grammar[sym] = {
        'name': name,
        'expansions': expansions,
        'base': base
    }

_grammar = {}
rule('Decls', 'declaration list', 'DeclB', '$Decls:decls $Decl:decl', '$Decl:_')
rule('Decl', 'declaration', 'DeclB', '$Function:_')

rule('Function', 'function declaration', 'DeclB',
    'FUN:fun $Type:retty IDENTIFIER:name OPARN:oparn                      CPARN:cparn $Block:body',
    'FUN:fun $Type:retty IDENTIFIER:name OPARN:oparn $ParamList:paramlist CPARN:cparn $Block:body')

rule('Stmts', 'statement list', 'StmtB', '$Stmts:stmts $Stmt:stmt',  '$Stmt:_')
rule('Stmt', 'statement', 'StmtB', '$EmptyStmt:_', '$VarStmt:_', '$ExprStmt:_', '$RetStmt:_', '$Block:_')

rule('VarStmt', 'variable statement', 'StmtB', 'VAR:var $Type:type $VarStmtItems:assignments SEMICOLON:semi')
rule('ExprStmt', 'expression statement', 'StmtB', '$Expr:expr SEMICOLON:semi')
rule('RetStmt', 'return statement', 'StmtB', 'RETURN:ret $Expr:expr SEMICOLON:semi')
rule('EmptyStmt', 'empty statement', 'StmtB', 'SEMICOLON:semi')

rule('VarStmtItems', 'variable statement assignment list', 'VStmtIB', '$VarStmtItems:items COMMA:comma $VarStmtItem:item', '$VarStmtItem:_')
rule('VarStmtItem', 'variable statement assignment', 'VStmtIB', 'IDENTIFIER:name EQUAL:equal $Expr:expr', 'IDENTIFIER:name')

rule('Block', 'code block', 'StmtB', 'OCURB:ocurb $Stmts:stmts CCURB:ccurb', 'OCURB:ocurb CCURB:ccurb')

rule('Type', 'type specifier', 'TypeB', '$BuiltinType:_')
rule('BuiltinType', 'builtin type specifier', 'TypeB', 'UINT8:type', 'UINT16:type', 'UINT32:type', 'UINT64:type', 'SINT8:type', 'SINT16:type', 'SINT32:type', 'SINT64:type', 'FLOAT:type', 'BOOL:type', 'DOUBLE:type', 'VOID:type', 'CHAR:type')

rule('Args', 'argument list', 'ArgsB', '$Args:args COMMA:comma $Expr:expr', '$Expr:expr')

rule('ParamList', 'parameter list', 'PListB', '$ParamList:plist COMMA:comma $Type:type IDENTIFIER:name', '$Type:type IDENTIFIER:name')

rule('Expr', 'expression', 'ExprB', '$AssignmentExpr:_')
rule('AssignmentExpr', 'assignment expression', 'ExprB', '$TernaryExpr:target EQUAL:equal $AssignmentExpr:value', '$TernaryExpr:_')
rule('TernaryExpr', 'ternary expression', 'ExprB', '$BinorExpr:_', '$BinorExpr:cond QUESTION:quest $Expr:trues COLON:colon $TernaryExpr:falses')
rule('BinorExpr', 'binary or expression', 'ExprB', '$BinorExpr:lhs DOUBLEPIPE:op $BinandExpr:rhs', '$BinandExpr:_')
rule('BinandExpr', 'binary and expression', 'ExprB', '$BinandExpr:lhs DOUBLEAMPER:op $BinnotExpr:rhs', '$BinnotExpr:_')
rule('BinnotExpr', 'binary not expression', 'ExprB', 'BANG:op $BinnotExpr:operand', '$CompeqExpr:_')
rule('CompeqExpr', 'equality expression', 'ExprB', '$CompeqExpr:lhs BANGEQUAL:op $ComplgtExpr:rhs', '$CompeqExpr:lhs DOUBLEEQUAL:op $ComplgtExpr:rhs', '$ComplgtExpr:_')
rule('ComplgtExpr', 'comparison expression', 'ExprB', '$ComplgtExpr:lhs LESS:op $BitxorExpr:rhs', '$ComplgtExpr:lhs GREATER:op $BitxorExpr:rhs', '$ComplgtExpr:lhs LESSEQUAL:op $BitxorExpr:rhs', '$ComplgtExpr:lhs GREATEREQUAL:op $BitxorExpr:rhs', '$BitxorExpr:_')
rule('BitxorExpr', 'bitwise xor expression', 'ExprB', '$BitxorExpr:lhs CARET:op $BitorExpr:rhs', '$BitorExpr:_')
rule('BitorExpr', 'bitwise or expression', 'ExprB', '$BitorExpr:lhs PIPE:op $BitandExpr:rhs', '$BitandExpr:_')
rule('BitandExpr', 'bitwise and expression', 'ExprB', '$BitandExpr:lhs AMPER:op $BitshiftExpr:rhs', '$BitshiftExpr:_')
rule('BitshiftExpr', 'bit shift expression', 'ExprB', '$BitshiftExpr:lhs DOUBLEGREATER:op $AdditionExpr:rhs', '$BitshiftExpr:lhs DOUBLELESS:op $AdditionExpr:rhs', '$AdditionExpr:_')
rule('AdditionExpr', 'addition expression', 'ExprB', '$AdditionExpr:lhs PLUS:op $MultExpr:rhs', '$AdditionExpr:lhs MINUS:op $MultExpr:rhs', '$MultExpr:_')
rule('MultExpr', 'multiplication expression', 'ExprB', '$MultExpr:lhs STAR:op $UnaryExpr:rhs', '$MultExpr:lhs SLASH:op $UnaryExpr:rhs', '$MultExpr:lhs PERCENT:op $UnaryExpr:rhs', '$UnaryExpr:_')
rule('UnaryExpr', 'unary expression', 'ExprB', 'TILDE:op $UnaryExpr:operand', 'MINUS:op $UnaryExpr:operand', '$CallExpr:_')
rule('CallExpr', 'function call expression', 'ExprB', '$PrimaryExpr:callee OPARN:oparn $Args:args CPARN:cparn', '$PrimaryExpr:callee OPARN:oparn CPARN:cparn', '$PrimaryExpr:_')
rule('PrimaryExpr', 'primary expression', 'ExprB', 'TRUELIT:value', 'FALSELIT:value', 'FLOATLIT:value', 'NULLPTRLIT:value', 'DECINTLIT:value', 'OCTINTLIT:value', 'BININTLIT:value', 'HEXINTLIT:value', 'CHARLIT:value', 'STRINGLIT:value', 'IDENTIFIER:value', 'OPARN:oparn $Expr:expr CPARN:cparn')

grammar = []
found = set()
missing = set()
for sym, rule in _grammar.items():
    if sym in missing:
        missing.remove(sym)

    found.add(sym)

    expansions = rule['expansions']

    base = rule['base']
    for expansion in expansions:
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
                convertedexpansion.append(NonTerminal(sname[1:], rule['name']))
                if sname[1:] not in found:
                    missing.add(sname[1:])
            else:
                convertedexpansion.append(Terminal(f'TokenType::{sname}'))
                if sname != sname.upper():
                    print(f'\033[35;1mwarning\033[0m: terminal {sname} in rule \033[1m{symbol} -> {expansion}\033[0m')

        if len(convertedexpansion) == 1 and type(convertedexpansion[0]) == NonTerminal and _grammar[str(convertedexpansion[0])]['base'] == base:
            skip = True
            print('\033[34mrule is skip\033[0m', sym, expansion)
        else:
            skip = False

        grammar.append(Rule(NonTerminal(sym, rule['name']), tuple(convertedexpansion), skip, vnames, base))

for missingi in missing:
    print(f'\033[35;1mwarning\033[0m: undefined terminal \033[1m{missingi}\033[0m')
print('-- parsed grammar')

augmentSymbol = NonTerminal('augment', 'compilation unit')
augmentRule = Rule(augmentSymbol, (grammar[0].symbol, ), True, '_', '')
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
                                       '        if (!errorRecovery(p, stack, lookahead, e))\\\n'
                                       '            done = true;\\\n'
                                       '        e.report();\\\n'
                                       '        errored = true;\\\n'
                                       '    }\\\n'
                                       '    break;\n'
                                       '#define DEFAULTINVALIDWHILE(justparsed, expected, whileparsing) \\\n'
                                       '    ERRORSTART()\\\n'
                                       '            Error e = p.invalidSyntaxWhile(justparsed, expected, whileparsing, lookahead, lasttok);\\\n'
                                       '    ERROREND()\n'
                                       '#define DEFAULTINVALIDNOEXPECT(justparsed, whileparsing) \\\n'
                                       '    ERRORSTART()\\\n'
                                       '            Error e = p.invalidSyntaxNoExpect(justparsed, whileparsing, lookahead, lasttok);\\\n'
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


        for ac, nts in stateactions:
            if type(ac) == ShiftAction:
                for term in nts:
                    output.append(    f'                    case {term.astt()}:\n')
                output.append(        f'                        shift(p, lasttok, lookahead, stack, steps, {ac.newstate}); break;\n')
                continue

            for term in nts:
                output.append(        f'                    case {term.astt()}:\n')

            if type(ac) == ReduceAction:
                if not ac.rule.skip:
                    output.append(         '                        {\n')

                    for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                        if type(sym) == Terminal:
                            output.append(f'                            auto a{i} (popT(stack));\n')
                        elif type(sym) == NonTerminal:
                            output.append(f'                            auto a{i} (popA<ASTNS::{str(sym)}>(stack));\n')

                    output.append(        f'                            std::unique_ptr<ASTNS::AST> push = std::make_unique<ASTNS::{str(ac.rule.symbol)}>({", ".join([f"std::move(a{i})" for i in range(len(ac.rule.expansion))])});\n')
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

        output.append(                f'                    default:\n')
        if len(state.expected):
            output.append(        f'                        DEFAULTINVALIDWHILE({state.justparsed}, {formatList(state.expected)}, {formatList(state.whileparsing)})\n')
        else:
            output.append(        f'                        DEFAULTINVALIDNOEXPECT({state.justparsed}, {formatList(state.whileparsing)})\n')
        output.append(                 '                }\n')
        output.append(                 '                break;\n')

    output.append(                     '            default:\n')
    output.append(                    ('                Error(Error::MsgType::INTERR, lookahead, "Parser reached invalid state")\n'
                                       '                    .underline(Error::Underline(lookahead, \'!\')\n'
                                       '                        .error(concatMsg("Parser reached invalid state: ", stack.back().state)))\n'
                                       '                    .reportAbort();\n'))

    output.append(                     '        }\n')
    output.append(                     '    }\n')
    output.append(                    ('#undef DEFAULTINVALIDNOEXPECT\n'
                                       '#undef DEFAULTINVALIDWHILE\n'
                                       '#undef ERRORSTART\n'
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
                           '        switch (lookahead.type)\\\n'
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
                           '#define SITOLOC(v)\\\n'
                           '    Location v##l ({}, {}, nullptr);\\\n'
                           '    if (v->istok)\\\n'
                           '        v##l = v->tok;\\\n'
                           '    else\\\n'
                           '        v##l = v->ast.get();\n'
                           '    bool valid = false;\n'
                           '    std::vector<stackitem>::reverse_iterator delto;\n'
                           '    while (!valid)\n'
                           '    {\n'
                           '        for (auto i = stack.rbegin(); i != stack.rend() && !valid; ++i)\n'
                           '        {\n'
                           '            if (!i->istok)\n'
                           '            {\n'
                           '                ASTNS::AST *ast = i->ast.get();\n'))

    for nonterm in symbols:
        if type(nonterm) == Terminal:
            continue
        if nonterm == augmentSymbol:
            continue

        output.append(    f'                CHECKASI({str(nonterm)})\n')
        output.append(     '                       ')
        for follow in nonterm.follow:
            output.append(f' case {follow.astt()}:')
        output.append(     '\n')
        output.append(     '                            RECOVERANDDEFBREAK()\n')
        output.append(     '                FINISHCHECKASI()\n')

    output.append(        ('            }\n'
                           '        }\n'
                           '        if (!valid)\n'
                           '            lookahead = p.consume();\n'
                           '        if (lookahead.type == TokenType::EOF_)\n'
                           '            return false;\n'
                           '    }\n'
                           '    stackitem *startabandon = &*delto.base();\n'
                           '    stackitem *endabandon = &*stack.end() - 1;\n'
                           '    SITOLOC(startabandon)\n'
                           '    SITOLOC(endabandon)\n'
                           '    e.underline(Error::Underline(Location(startabandonl.start, endabandonl.end, startabandonl.file), \'~\').note("erroneous syntax: abandoned this construct"));\n'
                           '    e.underline(Error::Underline(lookahead, \'-\').note("parser panicked until here"));\n'
                           '    stack.erase(delto.base(), stack.end());\n'
                           '#undef CHECKASI\n'
                           '#undef FINISHCHECKASI\n'
                           '#undef RECOVERANDDEFBREAK\n'
                           '    return true;\n'))

    return ''.join(output)
# generate single token insertion/deletion/substitution error recovery code {{{2
def genSingleTok():
    output = []
    output.append(              '#define TRYINSERT(ty) if (tryInsert(ty, p, lookahead, stack)) fixes.push_back(fix {fix::fixtype::INSERT, ty});;\n')
    output.append(              '#define TRYSUB(ty) if (trySub(ty, p, lookahead, stack)) fixes.push_back(fix {fix::fixtype::SUBSTITUTE, ty});;\n')
    output.append(              '#define TRYTOKTY(ty) TRYINSERT(ty); TRYSUB(ty);\n')

    for terminal in symbols:
        if type(terminal) == Terminal:
            output.append(     f'    TRYTOKTY({terminal.astt()})\n');

    output.append(              '    if (tryDel(p, stack)) fixes.push_back(fix {fix::fixtype::REMOVE});\n')
    return ''.join(output)
# entry {{{1
if __name__ == '__main__':
    printParseTable(False)
