#!/usr/bin/env python3

# classes {{{1
# symbols {{{2
class NonTerminal:
    nonterminals = []
    def __init__(self, symbol, panickable, name, base):
        self.symbol = symbol
        self.name = name
        self.panickable = panickable
        self.base = base
        assert symbol not in NonTerminal.nonterminals, f'duplicate nonterminal {symbol}'
        NonTerminal.nonterminals.append(symbol)

    def __repr__(self):
        return str(self)
    def __str__(self):
        return self.symbol

    def __hash__(self):
        return hash(self.symbol)

class Terminal:
    terminals = []
    def __init__(self, symbol):
        self.symbol = symbol
        assert symbol not in Terminal.terminals, f'duplicate terminal {symbol}'
        Terminal.terminals.append(symbol)

    def __repr__(self):
        return str(self)
    def __str__(self):
        return self.symbol

    def astt(self):
        return 'TokenType::' + self.symbol

    def __hash__(self):
        return hash(self.symbol)
# rule {{{2
class Rule:
    __num = 0
    def __init__(self, symbol, expansion, reduceAction, vnames, exhistart, exhiend, special):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1
        self.reduceAction = reduceAction
        self.vnames = vnames
        self.exhistart, self.exhiend = exhistart, exhiend
        self.special = special

    def __repr__(self):
        return str(self)
    def __str__(self):
        return f'{str(self.symbol)} -> {" ".join(map(str, self.expansion))}'

    def __hash__(self):
        return self.num # should be unique

# item stuff {{{2
class Item: # an LR1 item
    items = {}
    def __init__(self, rule, index, lookahead):
        self.rule = rule
        self.index = index
        self.lookahead = lookahead

    @staticmethod
    def get(rule, index, lookahead):
        itemtuple = (rule, index, lookahead)
        if itemtuple in Item.items:
            return Item.items[itemtuple]

        i = Item(*itemtuple)
        Item.items[itemtuple] = i
        return i

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

class ItemSet: # an LR1 item set
    sets = []
    __n = 0
    def __init__(self, kernel, extras):
        self.kernel = kernel
        self.extras = extras
        self.n = ItemSet.__n
        ItemSet.__n += 1

    @staticmethod
    def get(kernel, extras):
        for itemset in ItemSet.sets:
            if itemset.kernel == kernel and itemset.extras == extras:
                return itemset

        iset = ItemSet(kernel, extras)
        ItemSet.sets.append(iset)
        return iset

    def items(self):
        return self.kernel + self.extras

    def __repr__(self):
        return str(self)
    def __str__(self):
        return f'Itemset {self.n}\n' + \
            ''.join(map(lambda x: 'K: ' + str(x) + '\n', self.kernel)) + \
            ''.join(map(lambda x: 'E: ' + str(x) + '\n', self.extras))
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
            print(f'    : summary: {type(self.actions[sym])}/{type(action)} conflict: found {sym}, {self.actions[sym].explain()} or {action.explain()}?\033[0m')
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
        self.futures = {}
        self.terminates = {}

        for item in self.set_.kernel:
            if item.getAfterDot() is not None:
                if item.rule.symbol not in self.futures:
                    self.futures[item.rule.symbol] = []
                if item.getAfterDot() not in self.futures[item.rule.symbol]:
                    self.futures[item.rule.symbol].append(item.getAfterDot())
            else:
                if item.rule.symbol not in self.terminates:
                    self.terminates[item.rule.symbol] = []
                if item.lookahead not in self.terminates[item.rule.symbol]:
                    self.terminates[item.rule.symbol].append(item.lookahead)

# state table actions {{{2
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
# reduce actions {{{2
class SimpleReduceAction:
    def __init__(self, classname, args):
        self.classname = classname
        self.args = args
    def generate(self):
        return f'''
        std::unique_ptr<ASTNS::{self.classname}> push (std::make_unique<ASTNS::{self.classname}>({", ".join(f"a{i}" for i in self.args)}));
        stack.emplace_back(gotoState, std::move(push));
        '''
class SkipReduceAction:
    def __init__(self, ind=0):
        self.ind = ind
    def generate(self):
        return f'stack.emplace_back(gotoState, a{self.ind});\n'
class NullptrReduceAction:
    def generate(self):
        return 'stack.emplace_back(gotoState, nullptr);\n'
class VectorPushReduceAction:
    def __init__(self, vectorName, itemToPush, pushBackToStack):
        self.vectorName = vectorName
        self.itemToPush = itemToPush
        self.pushBackToStack = pushBackToStack
    def generate(self):
        return f'''
        {self.vectorName}.push_back({self.itemToPush});
        stack.emplace_back(gotoState, {self.pushBackToStack});
        '''
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
                pass
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

                    def addFirstsOf(i):
                        nonlocal changed
                        if i >= len(rule.expansion):
                            followextens = makeUnique(follow, follows[rule.symbol])
                            ntextens = makeUnique(ntfollow, ntfollows[rule.symbol])

                            if len(followextens):
                                follow.extend(followextens)
                                changed = True
                            if len(ntextens):
                                ntfollow.extend(ntextens)
                                changed = True
                        else:
                            if type(rule.expansion[i]) == NonTerminal:
                                followextens = makeUnique(follow, [x for x in firsts[rule.expansion[i]] if x != eofSym])
                                if len(followextens):
                                    follow.extend(followextens)
                                    changed = True

                                if rule.expansion[i] not in ntfollow:
                                    ntfollow.append(rule.expansion[i])
                                    changed = True
                            else:
                                if rule.expansion[i] not in follow:
                                    follow.append(rule.expansion[i])
                                    changed = True

                    addamt = 1
                    while addamt == 1 or any([len(r.expansion) == 0 for r in grammar if i + addamt - 1 < len(rule.expansion) and r.symbol == rule.expansion[i + addamt - 1]]):
                        addFirstsOf(i + addamt)
                        addamt += 1

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
    lr1kernel = []
    lr1extras = []

    for kerneli in kernel:
        for follow in follows[kerneli[0].symbol]:
            lr1kernel.append(Item.get(kerneli[0], kerneli[1], follow))

    for extrai in extras:
        for follow in follows[extrai[0].symbol]:
            lr1extras.append(Item.get(extrai[0], extrai[1], follow))

    return ItemSet.get(lr1kernel, lr1extras)
# make parser table {{{1
# find item sets {{{2
def getItemSets():
    initial = getClosurelr0([(augmentRule, 0)])

    isets = [initial]
    transitions = []

    stack = [initial]
    while len(stack):
        origset = stack.pop(0)

        afters = {}
        for item in origset.items():
            after = item.getAfterDot()
            if after is not None:
                if after not in afters:
                    afters[after] = []

                newitem = (item.rule, item.index + 1)
                if newitem not in afters[after]:
                    afters[after].append(newitem)

        for after, afternewset in afters.items():
            newsetlr1 = getClosurelr0(afternewset)
            toseti = newsetlr1.n
            if newsetlr1 not in isets:
                isets.append(newsetlr1)
                stack.append(newsetlr1)
            else:
                toseti = isets[isets.index(newsetlr1)].n

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
# rule shorthands {{{1
def nt(sym, name, base, panickable=False):
    return NonTerminal(sym, panickable, name, base)

def rule(sym, expansion, reduceAction, histart='START', hiend='END', special=''):
    vnames = []
    syms = []

    for exsym, vname in expansion:
        syms.append(exsym)
        vnames.append(vname)

    rsp = {}
    rsp['defaultreduce'] = True

    for sp in special.split(' '):
        if not len(sp):
            continue

        v = not sp.startswith('no')
        rest = sp[2:] if not v else sp
        if rest in rsp:
            rsp[rest] = v
        else:
            raise Exception(f'invalid special {rest}')

    rule = Rule(sym, syms, reduceAction, vnames, histart, hiend, rsp)
    grammar.append(rule)
    return rule

def listRule(sym, base, makeListAction, appendListAction, delimit=None):
    anothersym = nt('Another' + sym.symbol, 'another ' + sym.name, base, panickable=True) # useless rule to take advantage of "expected another x"
    rule(anothersym, ((sym, sym.symbol.lower()),), SkipReduceAction())

    symlist = nt(sym.symbol + 'List', sym.name + ' list', base, panickable=True)

    if delimit is not None:
        symsegment = nt(sym.symbol + 'Segment', sym.name + ' list', base, panickable=True)
        rule(symsegment, ((symsegment, symsegment.symbol.lower()), (delimit, str(delimit).lower()), (anothersym, anothersym.symbol.lower())), appendListAction)
        rule(symsegment, ((sym, sym.symbol.lower()),), makeListAction)

        rule(symlist, ((symsegment, symsegment.symbol.lower()),), SkipReduceAction(), special='nodefaultreduce')
        rule(symlist, ((symsegment, symsegment.symbol.lower()), (delimit, str(delimit).lower())), SkipReduceAction(), special='nodefaultreduce')
    else:
        rule(symlist, ((symlist, symlist.symbol.lower()), (anothersym, anothersym.symbol.lower())), appendListAction, special='nodefaultreduce')
        rule(symlist, ((sym, sym.symbol.lower()),), makeListAction)

    return symlist

def makeOpt(toopt, withAction, noAction, newname=None):
    if newname is None:
        newname = 'optional ' + toopt.name

    optsym = toopt.symbol + '_OPT'
    optnt = nt(optsym, newname, toopt.base)
    rule(optnt, ((toopt, toopt.symbol.lower()),), withAction)
    rule(optnt, (), noAction)
    return optnt

grammar = []

# rules {{{1
def makeGrammar():
    global augmentRule, augmentSymbol

    CU = nt('CU', 'compilation unit', 'CUB')
    Decl = nt('Decl', 'declaration', 'DeclB', panickable=True)
    FunctionDecl = nt('FunctionDecl', 'function declaration', 'DeclB', panickable=True)
    Stmt = nt('Stmt', 'statement', 'StmtB', panickable=True)
    VarStmt = nt('VarStmt', 'variable statement', 'StmtB', panickable=True)
    ExprStmt = nt('ExprStmt', 'expression statement', 'StmtB', panickable=True)
    RetStmt = nt('RetStmt', 'return statement', 'StmtB', panickable=True)
    VarStmtItem = nt('VarStmtItem', 'variable statement initialization', 'VStmtIB')
    LineEnding = nt('LineEnding', 'line ending', 'LineEndingB')
    Block = nt('Block', 'code block', 'ExprB', panickable=True)
    BracedBlock = nt('BracedBlock', 'braced code block', 'ExprB', panickable=True)
    IndentedBlock = nt('IndentedBlock', 'indented code block', 'ExprB', panickable=True)
    ImplRet = nt('ImplRet', 'implicit return', 'ExprB', panickable=True)
    Type = nt('Type', 'type specifier', 'TypeB')
    PrimitiveType = nt('PrimitiveType', 'primitive type specifier', 'TypeB')
    Arg = nt('Arg', 'argument', 'ArgB', panickable=True)
    Param = nt('Param', 'parameter', 'PListB', panickable=True)
    Expr = nt('Expr', 'expression', 'ExprB')
    BlockedExpr = nt('BlockedExpr', 'braced expression', 'ExprB')
    NotBlockedExpr = nt('NotBlockedExpr', 'non-braced expression', 'ExprB')
    IfExpr = nt('IfExpr', 'if expression', 'ExprB', panickable=True)
    ForExpr = nt('ForExpr', 'for expression', 'ExprB', panickable=True)
    AssignmentExpr = nt('AssignmentExpr', 'assignment expression', 'ExprB')
    BinOrExpr = nt('BinOrExpr', 'binary or expression', 'ExprB')
    BinAndExpr = nt('BinAndExpr', 'binary and expression', 'ExprB')
    CompEQExpr = nt('CompEQExpr', 'equality expression', 'ExprB')
    CompLGTExpr = nt('CompLGTExpr', 'comparison expression', 'ExprB')
    BitXorExpr = nt('BitXorExpr', 'bitwise xor expression', 'ExprB')
    BitOrExpr = nt('BitOrExpr', 'bitwise or expression', 'ExprB')
    BitAndExpr = nt('BitAndExpr', 'bitwise and expression', 'ExprB')
    BitShiftExpr = nt('BitShiftExpr', 'bit shift expression', 'ExprB')
    AdditionExpr = nt('AdditionExpr', 'addition expression', 'ExprB')
    MultExpr = nt('MultExpr', 'multiplication expression', 'ExprB')
    CastExpr = nt('CastExpr', 'type cast expression', 'ExprB')
    UnaryExpr = nt('UnaryExpr', 'unary expression', 'ExprB')
    CallExpr = nt('CallExpr', 'function call expression', 'ExprB')
    PrimaryExpr = nt('PrimaryExpr', 'primary expression', 'ExprB')

    augmentSymbol = NonTerminal('augment', False, '', '')
    augmentRule = rule(augmentSymbol, ((CU, '_'),), None)

    AMPER = Terminal('AMPER')
    BANG = Terminal('BANG')
    BANGEQUAL = Terminal('BANGEQUAL')
    BININTLIT = Terminal('BININTLIT')
    BOOL = Terminal('BOOL')
    CARET = Terminal('CARET')
    CCURB = Terminal('CCURB')
    CHAR = Terminal('CHAR')
    CHARLIT = Terminal('CHARLIT')
    COLON = Terminal('COLON')
    COMMA = Terminal('COMMA')
    CPARN = Terminal('CPARN')
    DECINTLIT = Terminal('DECINTLIT')
    DEDENT = Terminal('DEDENT')
    DOUBLE = Terminal('DOUBLE')
    DOUBLEAMPER = Terminal('DOUBLEAMPER')
    DOUBLEEQUAL = Terminal('DOUBLEEQUAL')
    DOUBLEGREATER = Terminal('DOUBLEGREATER')
    DOUBLELESS = Terminal('DOUBLELESS')
    DOUBLEPIPE = Terminal('DOUBLEPIPE')
    EQUAL = Terminal('EQUAL')
    ELSE = Terminal('ELSE')
    FALSELIT = Terminal('FALSELIT')
    FLOAT = Terminal('FLOAT')
    FLOATLIT = Terminal('FLOATLIT')
    FOR = Terminal('FOR')
    FUN = Terminal('FUN')
    GREATER = Terminal('GREATER')
    GREATEREQUAL = Terminal('GREATEREQUAL')
    HEXINTLIT = Terminal('HEXINTLIT')
    IDENTIFIER = Terminal('IDENTIFIER')
    IF = Terminal('IF')
    INDENT = Terminal('INDENT')
    LEFTARROW = Terminal('LEFTARROW')
    LESS = Terminal('LESS')
    LESSEQUAL = Terminal('LESSEQUAL')
    MINUS = Terminal('MINUS')
    NEWLINE = Terminal('NEWLINE')
    NULLPTRLIT = Terminal('NULLPTRLIT')
    OCTINTLIT = Terminal('OCTINTLIT')
    OCURB = Terminal('OCURB')
    OPARN = Terminal('OPARN')
    PERCENT = Terminal('PERCENT')
    PIPE = Terminal('PIPE')
    PLUS = Terminal('PLUS')
    QUESTION = Terminal('QUESTION')
    RETURN = Terminal('RETURN')
    SEMICOLON = Terminal('SEMICOLON')
    SINT16 = Terminal('SINT16')
    SINT32 = Terminal('SINT32')
    SINT64 = Terminal('SINT64')
    SINT8 = Terminal('SINT8')
    SLASH = Terminal('SLASH')
    STAR = Terminal('STAR')
    STRINGLIT = Terminal('STRINGLIT')
    TILDE = Terminal('TILDE')
    TRUELIT = Terminal('TRUELIT')
    UINT16 = Terminal('UINT16')
    UINT32 = Terminal('UINT32')
    UINT64 = Terminal('UINT64')
    UINT8 = Terminal('UINT8')
    VAR = Terminal('VAR')
    VOID = Terminal('VOID')

    ParamList = listRule(Param, 'PListB', SimpleReduceAction('ParamList', (0,)), VectorPushReduceAction('a0->params', 'std::move(a2)', 'a0'), COMMA)
    ArgList = listRule(Arg, 'ArgB', SimpleReduceAction('ArgList', (0,)), VectorPushReduceAction('a0->args', 'std::move(a2)', 'a0'), COMMA)
    VarStmtItemList = listRule(VarStmtItem, 'VStmtIB', SimpleReduceAction('VarStmtItemList', (0,)), VectorPushReduceAction('a0->items', 'std::move(a2)', 'a0'), COMMA)
    StmtList = listRule(Stmt, 'StmtB', SimpleReduceAction('StmtList', (0,)), VectorPushReduceAction('a0->stmts', 'std::move(a1)', 'a0'))
    DeclList = listRule(Decl, 'DeclB', SimpleReduceAction('DeclList', (0,)), VectorPushReduceAction('a0->decls', 'std::move(a1)', 'a0'))

    ParamListOpt = makeOpt(ParamList, SkipReduceAction(), NullptrReduceAction())
    ArgListOpt = makeOpt(ArgList, SkipReduceAction(), NullptrReduceAction())
    StmtListOpt = makeOpt(StmtList, SkipReduceAction(), NullptrReduceAction())
    ImplRetOpt = makeOpt(ImplRet, SkipReduceAction(), NullptrReduceAction())
    ExprOpt = makeOpt(Expr, SkipReduceAction(), NullptrReduceAction())
    VarStmtOpt = makeOpt(VarStmt, SkipReduceAction(), NullptrReduceAction())
    LineEndingOpt = makeOpt(LineEnding, SkipReduceAction(), NullptrReduceAction())

    rule(CU, ((DeclList, 'dl'),), SimpleReduceAction('CU', (0,)))
    rule(CU, (), SimpleReduceAction('CU', ()))

    rule(Decl, ((FunctionDecl, '_'),), SkipReduceAction())

    rule(FunctionDecl, ((FUN, 'fun'),  (Type, 'retty'),  (IDENTIFIER, 'name'),  (OPARN, 'oparn'),  (ParamListOpt, 'paramlist'),  (CPARN, 'cparn'),  (Block, 'body'), (LineEndingOpt, 'endl')), SimpleReduceAction('FunctionDecl', (1, 2, 4, 6)), 'fun', 'cparn')
    rule(FunctionDecl, ((FUN, 'fun'),  (Type, 'retty'),  (IDENTIFIER, 'name'),  (OPARN, 'oparn'),  (ParamListOpt, 'paramlist'),  (CPARN, 'cparn'),  (LineEnding, 'endl')), SimpleReduceAction('FunctionDecl', (1, 2, 4)), 'fun', 'endl')

    rule(Stmt, ((VarStmt, '_'),), SkipReduceAction())
    rule(Stmt, ((ExprStmt, '_'),), SkipReduceAction())
    rule(Stmt, ((RetStmt, '_'),), SkipReduceAction())

    rule(VarStmt, ((VAR, 'var'),  (Type, 'type'),  (VarStmtItemList, 'assignments'), (LineEnding, 'ending')), SimpleReduceAction('VarStmt', (1, 2)))

    rule(ExprStmt, ((NotBlockedExpr, 'expr'), (LineEnding, 'ending')), SimpleReduceAction('ExprStmt', (0,)))
    rule(ExprStmt, ((BlockedExpr, 'expr'), (LineEndingOpt, 'ending')), SimpleReduceAction('ExprStmt', (0,)))

    rule(RetStmt, ((RETURN, 'ret'), (Expr, 'expr'), (LineEnding, 'ending')), SimpleReduceAction('RetStmt', (1,)))
    rule(RetStmt, ((RETURN, 'ret'), (LineEnding, 'ending')), SimpleReduceAction('RetStmt', ()))

    rule(VarStmtItem, ((IDENTIFIER, 'name'),  (EQUAL, 'equal'),  (Expr, 'expr'), ), SimpleReduceAction('VarStmt', (0, 2)))
    rule(VarStmtItem, ((IDENTIFIER, 'name'),), SimpleReduceAction('VarStmt', (0,)))

    rule(Block, ((BracedBlock, '_'),), SkipReduceAction())
    rule(Block, ((IndentedBlock, '_'),), SkipReduceAction())
    rule(BracedBlock, ((OCURB, 'ocurb'), (StmtListOpt, 'stmts'), (ImplRetOpt, 'implret'), (CCURB, 'ccurb')), SimpleReduceAction('Block', (1, 2)))
    rule(BracedBlock, ((OCURB, 'ocurb'), (NEWLINE, 'newlopt'), (StmtListOpt, 'stmts'), (ImplRetOpt, 'implret'), (CCURB, 'ccurb')), SimpleReduceAction('Block', (2, 3)))
    rule(BracedBlock, ((OCURB, 'ocurb'), (NEWLINE, 'newlopt'), (INDENT, 'indentopt'), (StmtListOpt, 'stmts'), (ImplRetOpt, 'implret'), (DEDENT, 'dedentopt'), (CCURB, 'ccurb')), SimpleReduceAction('Block', (3, 4)))
    rule(IndentedBlock, ((NEWLINE, 'newl'), (INDENT, 'indent'), (StmtListOpt, 'stmts'), (ImplRetOpt, 'implret'), (DEDENT, 'dedent')), SimpleReduceAction('Block', (2, 3)))
    rule(ImplRet, ((LEFTARROW, 'leftarrow'), (Expr, 'expr'), (LineEndingOpt, 'ending')), SimpleReduceAction('ImplRet', (1,)))

    rule(LineEnding, ((NEWLINE, 'tok'),), NullptrReduceAction())
    rule(LineEnding, ((SEMICOLON, 'tok'),), NullptrReduceAction())
    rule(LineEnding, ((SEMICOLON, 'tok'), (NEWLINE, 'tok2')), NullptrReduceAction())

    rule(Type, ((PrimitiveType, '_'),), SkipReduceAction())

    rule(PrimitiveType, ((UINT8, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((UINT16, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((UINT32, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((UINT64, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((SINT8, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((SINT16, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((SINT32, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((SINT64, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((FLOAT, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((BOOL, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((DOUBLE, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((CHAR, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))
    rule(PrimitiveType, ((VOID, 'type'),), SimpleReduceAction('PrimitiveType', (0,)))

    rule(Arg, ((Expr, 'expr'),), SimpleReduceAction('Arg', (0,)))

    rule(Param, ((Type, 'type'),  (IDENTIFIER, 'name')), SimpleReduceAction('Param', (0, 1)))

    rule(Expr, ((BlockedExpr, '_'),), SkipReduceAction())
    rule(Expr, ((NotBlockedExpr, '_'),), SkipReduceAction())

    rule(NotBlockedExpr, ((AssignmentExpr, '_'),), SkipReduceAction())

    rule(BlockedExpr, ((IfExpr, '_'),), SkipReduceAction())
    rule(BlockedExpr, ((ForExpr, '_'),), SkipReduceAction())
    rule(BlockedExpr, ((BracedBlock, '_'),), SkipReduceAction())

    rule(IfExpr, ((IF, 'iftok'), (Expr, 'cond'), (Block, 'trues')), SimpleReduceAction('IfExpr', (1, 2)))
    rule(IfExpr, ((IF, 'iftok'), (Expr, 'cond'), (Block, 'trues'), (ELSE, 'elsetok'), (Block, 'falses')), SimpleReduceAction('IfExpr', (1, 2, 4)))
    rule(IfExpr, ((IF, 'iftok'), (Expr, 'cond'), (Block, 'trues'), (ELSE, 'elsetok'), (IfExpr, 'falses')), SimpleReduceAction('IfExpr', (1, 2, 4)))

    rule(ForExpr, ((FOR, 'fortok'), (VarStmtOpt, 'start'), (SEMICOLON, 'semi1'), (ExprOpt, 'cond'), (SEMICOLON, 'semi2'), (ExprOpt, 'increment'), (CPARN, 'cparn'), (Block, 'body')), SimpleReduceAction('ForExpr', (1, 3, 5, 7)))

    BinaryExprReduceAction = SimpleReduceAction('BinaryExpr', (0, 1, 2))

    rule(AssignmentExpr, ((BinOrExpr, 'target'),  (EQUAL, 'equal'),  (AssignmentExpr, 'value'), ), SimpleReduceAction('AssignmentExpr', (0, 1, 2)))
    rule(AssignmentExpr, ((BinOrExpr, '_'),), SkipReduceAction())
    rule(BinOrExpr, ((BinOrExpr, 'lhs'),  (DOUBLEPIPE, 'op'),  (BinAndExpr, 'rhs'), ), SimpleReduceAction('ShortBinaryExpr', (0, 1, 2)))
    rule(BinOrExpr, ((BinAndExpr, '_'),), SkipReduceAction())
    rule(BinAndExpr, ((BinAndExpr, 'lhs'),  (DOUBLEAMPER, 'op'),  (CompEQExpr, 'rhs'), ), SimpleReduceAction('ShortBinaryExpr', (0, 1, 2)))
    rule(BinAndExpr, ((CompEQExpr, '_'),), SkipReduceAction())
    rule(CompEQExpr, ((CompEQExpr, 'lhs'),  (BANGEQUAL, 'op'),  (CompLGTExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(CompEQExpr, ((CompEQExpr, 'lhs'),  (DOUBLEEQUAL, 'op'),  (CompLGTExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(CompEQExpr, ((CompLGTExpr, '_'),), SkipReduceAction())
    rule(CompLGTExpr, ((CompLGTExpr, 'lhs'),  (LESS, 'op'),  (BitXorExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(CompLGTExpr, ((CompLGTExpr, 'lhs'),  (GREATER, 'op'),  (BitXorExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(CompLGTExpr, ((CompLGTExpr, 'lhs'),  (LESSEQUAL, 'op'),  (BitXorExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(CompLGTExpr, ((CompLGTExpr, 'lhs'),  (GREATEREQUAL, 'op'),  (BitXorExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(CompLGTExpr, ((BitXorExpr, '_'),), SkipReduceAction())
    rule(BitXorExpr, ((BitXorExpr, 'lhs'),  (CARET, 'op'),  (BitOrExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(BitXorExpr, ((BitOrExpr, '_'),), SkipReduceAction())
    rule(BitOrExpr, ((BitOrExpr, 'lhs'),  (PIPE, 'op'),  (BitAndExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(BitOrExpr, ((BitAndExpr, '_'),), SkipReduceAction())
    rule(BitAndExpr, ((BitAndExpr, 'lhs'),  (AMPER, 'op'),  (BitShiftExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(BitAndExpr, ((BitShiftExpr, '_'),), SkipReduceAction())
    rule(BitShiftExpr, ((BitShiftExpr, 'lhs'),  (DOUBLEGREATER, 'op'),  (AdditionExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(BitShiftExpr, ((BitShiftExpr, 'lhs'),  (DOUBLELESS, 'op'),  (AdditionExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(BitShiftExpr, ((AdditionExpr, '_'),), SkipReduceAction())
    rule(AdditionExpr, ((AdditionExpr, 'lhs'),  (PLUS, 'op'),  (MultExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(AdditionExpr, ((AdditionExpr, 'lhs'),  (MINUS, 'op'),  (MultExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(AdditionExpr, ((MultExpr, '_'),), SkipReduceAction())
    rule(MultExpr, ((MultExpr, 'lhs'),  (STAR, 'op'),  (UnaryExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(MultExpr, ((MultExpr, 'lhs'),  (SLASH, 'op'),  (UnaryExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(MultExpr, ((MultExpr, 'lhs'),  (PERCENT, 'op'),  (UnaryExpr, 'rhs'), ), BinaryExprReduceAction)
    rule(MultExpr, ((CastExpr, '_'),), SkipReduceAction())
    rule(CastExpr, ((OPARN, 'oparn'),  (Type, 'type'),  (CPARN, 'cparn'),  (CastExpr, 'operand'), ), SimpleReduceAction('CastExpr', (1, 3)))
    rule(CastExpr, ((UnaryExpr, '_'),), SkipReduceAction())
    rule(UnaryExpr, ((TILDE, 'op'),  (UnaryExpr, 'operand'), ), SimpleReduceAction('UnaryExpr', (0, 1)))
    rule(UnaryExpr, ((MINUS, 'op'),  (UnaryExpr, 'operand'), ), SimpleReduceAction('UnaryExpr', (0, 1)))
    rule(UnaryExpr, ((BANG, 'op'),  (UnaryExpr, 'operand'), ), SimpleReduceAction('UnaryExpr', (0, 1)))
    rule(UnaryExpr, ((CallExpr, '_'),), SkipReduceAction())
    rule(CallExpr, ((CallExpr, 'callee'),  (OPARN, 'oparn'),  (ArgListOpt, 'args'),  (CPARN, 'cparn'), ), SimpleReduceAction('CallExpr', (0, 1, 2)))
    rule(CallExpr, ((PrimaryExpr, '_'),), SkipReduceAction())
    rule(PrimaryExpr, ((TRUELIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((FALSELIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((FLOATLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((NULLPTRLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((DECINTLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((OCTINTLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((BININTLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((HEXINTLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((CHARLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((STRINGLIT, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((IDENTIFIER, 'value'),), SimpleReduceAction('PrimaryExpr', (0,)))
    rule(PrimaryExpr, ((OPARN, 'oparn'),  (Expr, 'expr'),  (CPARN, 'cparn'), ), SkipReduceAction(1))

makeGrammar()

# convert grammar {{{1
eofSym = Terminal('EOF_')

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
        return f'format("either % or %", {l[0]}, {l[1]})'
    elif len(l) == 0:
        return '"nothing"'
    else:
        return 'format("' + ", ".join('%' for _ in l[:-1]) + ', or %", ' +  ', '.join(l) + ')'
# }}}
def genLoop():
    output = []

    output.append(                                '    bool done = false;\n')
    output.append(                                '    bool errored = false;\n')
    output.append(                                '    int steps = 0;\n')
    output.append(                                '    Token lookahead (_lookahead); // for when you need to inject a new token\n')
    output.append(                                '    Token lasttok = lookahead;\n')

    output.append(                                '    while (!done)\n')
    output.append(                                '    {\n')
    output.append(                                '        if (istrial && steps > 5)\n')
    output.append(                                '            return true;\n')
    output.append(                                '        switch (stack.back().state)\n')
    output.append(                                '        {\n')

    for staten, state in sorted(table.items(), key=lambda x:x[0]):
        output.append(                           f'            case {staten}:\n')
        output.append(                            '               switch (lookahead.type)\n')
        output.append(                            '               {\n')

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

        statereduces = [ac for ac in stateactions if type(ac[0]) == ReduceAction]
        reduceOnly = len(statereduces) == 1 and statereduces[0][0].rule.special['defaultreduce']
        for ac, nts in stateactions:
            if type(ac) == ShiftAction:
                for term in nts:
                    output.append(               f'                    case {term.astt()}:\n')
                output.append(                   f'                        shift(p, lasttok, lookahead, stack, steps, {ac.newstate}); break;\n')
                continue

            if reduceOnly:
                output.append(                   f'                    default:\n')
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
                    output.append(               f'                    case {term.astt()}:\n')

            if type(ac) == ReduceAction:
                output.append(                    '                        {\n')

                for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                    if type(sym) == Terminal:
                        output.append(           f'                            auto a{i} (popT(stack));\n')
                    elif type(sym) == NonTerminal:
                        output.append(           f'                            auto a{i} (popA<ASTNS::{str(sym)}>(stack));\n')

                output.append(                   f'                            size_t gotoState = getGoto<ASTNS::{str(ac.rule.symbol)}>(stack.back().state);\n')
                output.append(ac.rule.reduceAction.generate())
                output.append(                    '                        }\n')


            elif type(ac) == AcceptAction:
                output.append(                    '                            done = true;\n')
            else:
                raise Exception('invalid action type')

            output.append(                        '                        break;\n')

        if not reduceOnly:
            def stc(s):
                if type(s) == NonTerminal:
                    return f'"{s.name}"'
                else:
                    return f'stringifyTokenType({s.astt()})'

            output.append(                        '                    default:\n')
            output.append(                        '                        if (istrial) return false;\n')

            futuress = [f'format("expected % for %", {formatList([stc(p) for p in future])}, {stc(nt)})' for nt, future in state.futures.items()]
            terminatess = [f'format("expected % to terminate %", {formatList([stc(p) for p in future])}, {stc(nt)})' for nt, future in state.terminates.items()]
            output.append(                       f'                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {{  {", ".join(futuress + terminatess)}  }});\n')
        output.append(                            '                }\n')
        output.append(                            '                break;\n')

    output.append(                                '            default:\n')
    output.append(                                '                reportAbortNoh(format("Parser reached invalid state: %", stack.back().state));\n')

    output.append(                                '        }\n')
    output.append(                                '    }\n')

    return ''.join(output)
# generate goto code {{{2
def genGoto():
    output = []

    for nonterm in symbols:
        if type(nonterm) == Terminal:
            continue

        if nonterm == augmentSymbol:
            continue

        output.append(                           f'template <> size_t getGoto<ASTNS::{str(nonterm)}>(size_t state)\n')
        output.append(                            '{\n')
        output.append(                            '    switch (state)\n')
        output.append(                            '    {\n')

        returns = {}
        for staten, state in table.items():
            if nonterm in state.goto:
                if state.goto[nonterm] in returns: # there is already a state in which this goto is returned (squishing rows together)
                    returns[state.goto[nonterm]].append(staten)
                else:
                    returns[state.goto[nonterm]] = [staten]

        for retval, states in returns.items():
            for state in states:
                output.append(                   f'        case {state}:\n')
            output.append(                       f'            return {retval};\n')

        output.append(                            '        default:\n')
        output.append(                           f'            reportAbortNoh("retrieve goto of nonterminal {str(nonterm)} in invalid state");\n')
        output.append(                            '    }\n')
        output.append(                            '}\n')

    return ''.join(output)

# generate panic mode error recovery code {{{2
def genPanicMode():
    output = []
    output.append(                               ('#define CHECKASI(ty)\\\n'
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
                                                  '    e.lookahead = e.p.consume(); // prevent infinite panicking loops\n'
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
        if not nonterm.panickable:
            continue

        output.append(                           f'                CHECKASI({str(nonterm)})\n')
        output.append(                            '                       ')
        for follow in follows[nonterm]:
            output.append(                       f' case {follow.astt()}:')
        output.append(                            '\n')
        output.append(                            '                            RECOVERANDDEFBREAK()\n')
        output.append(                            '                FINISHCHECKASI()\n')

    output.append(                               ('            }\n'
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
                                                  '    ERR_PANICKING_INVALID_SYNTAX(e.olh, e.lasttok, e.lookahead, expectations);\n'
                                                  '    return true;\n'))

    return ''.join(output)
# generate single token insertion/deletion/substitution error recovery code {{{2
def genSingleTok():
    output = []
    output.append(                                '#define TRYINSERT(ty) if (tryInsert(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::INSERT, ty}; if (score(f) > score(bestfix)) bestfix = f;}\n')
    output.append(                                '#define TRYSUB(ty) if (trySub(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::SUBSTITUTE, ty}; if (score(f) > score(bestfix)) bestfix = f;}\n')
    output.append(                                '#define TRYTOKTY(ty) TRYINSERT(ty); TRYSUB(ty);\n')

    for terminal in symbols:
        if type(terminal) == Terminal:
            output.append(                       f'    TRYTOKTY({terminal.astt()})\n')

    output.append(                                '    if (tryDel(e.p, e.stack)) {fix f = fix {fix::fixtype::REMOVE, static_cast<TokenType>(-1)}; if (score(f) > score(bestfix)) bestfix = f;};\n')
    return ''.join(output)
# entry {{{1
if __name__ == '__main__':
    printParseTable(False)
