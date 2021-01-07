#!/usr/bin/env python3

# classes {{{1
# symbols {{{2
class NonTerminal:
    nonterminals = []
    def __init__(self, symbol, panickable, name, reduces_to):
        self.symbol = symbol
        self.name = name
        self.panickable = panickable
        self.reduces_to = reduces_to
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
    def __init__(self, symbol, expansion, reduce_action, special):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1
        self.reduce_action = reduce_action
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

    def get_after_dot(self):
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

        self.make_description()

    def set_action(self, sym, action):
        if sym in self.actions.keys():
            conflict_type = 'sr' if isinstance(self.actions[sym], ShiftAction) or isinstance(action, ShiftAction) else 'rr'
            having_parsed = self.set_.kernel[0].rule.expansion[:self.set_.kernel[0].index]
            while_parsing = set(item.rule.symbol for item in self.set_.kernel)
            possible_nexts = set(item.get_after_dot() for item in self.set_.kernel)

            print(f'{conflict_type} conflict')
            print(f'   | in state {self.seti}')
            print(f'   | while parsing {while_parsing}')
            print(f'   | having parsed {" ".join(map(str, having_parsed))}')
            print(f'   | found {sym}')
            print(f'   | possible nexts are {" ".join(map(str, possible_nexts))}')
            print( '   |')
            print(f'   | already have {self.actions[sym].explain()}')
            print(f'   | addding {action.explain()}')
            print()

            self.actions[sym] = None
            return False

        self.actions[sym] = action
        return True

    def set_goto(self, sym, newstate):
        if sym in self.goto.keys():
            self.goto[sym] = None
            print('goto conflict')
            return False

        self.goto[sym] = newstate
        return True

    def make_description(self):
        self.futures = {}
        self.terminates = {}

        for item in self.set_.kernel:
            if item.get_after_dot() is not None:
                if item.rule.symbol not in self.futures:
                    self.futures[item.rule.symbol] = []
                if item.get_after_dot() not in self.futures[item.rule.symbol]:
                    self.futures[item.rule.symbol].append(item.get_after_dot())
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
        for set_ in ItemSet.sets:
            if set_.n == self.newstate:
                new_state_nts = set(item.rule.symbol for item in set_.kernel)
                break
        return f'shift and goto state {self.newstate} in order to start parsing {new_state_nts}'
    def __eq__(self, other):
        return isinstance(self, type(other)) and self.newstate == other.newstate
class ReduceAction:
    def __init__(self, rule):
        self.rule = rule
    def __str__(self):
        return f'r{self.rule.num}'
    def __repr__(self):
        return str(self)
    def explain(self):
        return f'reduce rule "{self.rule}"'
    def __eq__(self, other):
        return isinstance(self, type(other)) and self.rule == other.rule
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
        return (f'std::unique_ptr<ASTNS::{self.classname}> push (std::make_unique<ASTNS::{self.classname}>(p.sourcefile, start, end, {self.args}));\n', 'std::move(push)')
class SkipReduceAction:
    def __init__(self, ind=0):
        self.ind = ind
    def generate(self):
        return ('', f'std::move(a{self.ind})')
class LocationReduceAction(SimpleReduceAction):
    def __init__(self):
        super().__init__('PureLocation', '0')
class NullptrReduceAction:
    def generate(self):
        return ('', 'nullptr')
class VectorPushReduceAction:
    def __init__(self, vector_name, item_to_push, push_back_to_stack):
        self.vector_name = vector_name
        self.item_to_push = item_to_push
        self.push_back_to_stack = push_back_to_stack
    def generate(self):
        return (f'{self.vector_name}.push_back({self.item_to_push});\n', f'std::move({self.push_back_to_stack})')
class VectorPushOneAction:
    def __init__(self, new_class, item, itemtype, vector_name):
        self.new_class = new_class
        self.item = item
        self.itemtype = itemtype
        self.vector_name = vector_name
    def generate(self):
        return (f'''std::unique_ptr<{self.new_class}> push(std::make_unique<{self.new_class}>(p.sourcefile, start, end, std::vector<{self.itemtype}> {{}}));\n
        push->{self.vector_name}.push_back({self.item});\n''', 'std::move(push)')
class WarnAction:
    def __init__(self, warning, other_action):
        self.warning = warning
        self.other_action = other_action
    def generate(self):
        code, ret = self.other_action.generate()
        code = self.warning + code
        return (code, ret)
# helpers {{{1
def make_unique(already, new):
    return [x for x in new if x not in already]
# first and follows functions {{{2
def find_firsts():
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
                if isinstance(rule.expansion[0], NonTerminal):
                    extension = make_unique(first, firsts[rule.expansion[0]])
                    if len(extension) > 0:
                        first.extend(extension)
                        changed = True
                else:
                    if rule.expansion[0] not in first:
                        first.append(rule.expansion[0])
                        changed = True
    return firsts

def find_follows():
    follows = {}
    nt_follows = {}

    for rule in grammar:
        for sym in filter(lambda x: isinstance(x, NonTerminal), [rule.symbol, *rule.expansion]):
            follows[sym] = []
            nt_follows[sym] = []

    follows[AUGMENT_SYM] = [eof_sym]

    changed = True
    while changed:
        changed = False
        for rule in grammar:
            for i, sym in enumerate(rule.expansion):
                if isinstance(sym, NonTerminal):
                    follow = follows[sym]
                    ntfollow = nt_follows[sym]

                    def add_firsts_of(i):
                        nonlocal changed
                        if i >= len(rule.expansion):
                            followextens = make_unique(follow, follows[rule.symbol])
                            ntextens = make_unique(ntfollow, nt_follows[rule.symbol])

                            if len(followextens) > 0:
                                follow.extend(followextens)
                                changed = True
                            if len(ntextens) > 0:
                                ntfollow.extend(ntextens)
                                changed = True
                        else:
                            if isinstance(rule.expansion[i], NonTerminal):
                                followextens = make_unique(follow, [x for x in FIRSTS[rule.expansion[i]] if x != eof_sym])
                                if len(followextens) > 0:
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
                        add_firsts_of(i + addamt)
                        addamt += 1
    return (follows, nt_follows)

# closure {{{2
def get_closurelr0(lr0set):
    kernel = lr0set
    extras = []
    stack = list(lr0set)
    while len(stack) > 0:
        cur = stack.pop(0)

        if cur[1] < len(cur[0].expansion):
            after = cur[0].expansion[cur[1]]
            if isinstance(after, NonTerminal):
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
        for follow in FOLLOWS[kerneli[0].symbol]:
            lr1kernel.append(Item.get(kerneli[0], kerneli[1], follow))

    for extrai in extras:
        for follow in FOLLOWS[extrai[0].symbol]:
            lr1extras.append(Item.get(extrai[0], extrai[1], follow))

    return ItemSet.get(lr1kernel, lr1extras)
# make parser table {{{1
# find item sets {{{2
def get_item_sets():
    initial = get_closurelr0([(AUGMENT_RULE, 0)])

    isets = [initial]
    transitions = []

    stack = [initial]
    while len(stack) > 0:
        origset = stack.pop(0)

        afters = {}
        for item in origset.items():
            after = item.get_after_dot()
            if after is not None:
                if after not in afters:
                    afters[after] = []

                newitem = (item.rule, item.index + 1)
                if newitem not in afters[after]:
                    afters[after].append(newitem)

        for after, afternewset in afters.items():
            newsetlr1 = get_closurelr0(afternewset)
            toseti = newsetlr1.n
            if newsetlr1 not in isets:
                isets.append(newsetlr1)
                stack.append(newsetlr1)
            else:
                toseti = isets[isets.index(newsetlr1)].n

            transitions.append((origset.n, after, toseti))

    return isets, transitions
# fill parsing table {{{2
def fill_parse_table(isets, transitions):
    table = {}
    for iset in isets:
        state = State(iset)
        table[iset.n] = state

    conflicts = 0

    for fromseti, symbol, toseti in transitions:
        state = table[fromseti]
        if isinstance(symbol, Terminal):
            if not state.set_action(symbol, ShiftAction(toseti)):
                conflicts += 1
        else:
            if not state.set_goto(symbol, toseti):
                conflicts += 1

    for iset in isets:
        for item in iset.items():
            if item.get_after_dot() is None:
                state = table[iset.n]
                if item.rule.symbol != AUGMENT_SYM:
                    if not state.set_action(item.lookahead, ReduceAction(item.rule)):
                        conflicts += 1
                else:
                    if not state.set_action(item.lookahead, AcceptAction()):
                        conflicts += 1

    if conflicts > 0:
        print('note: unabriged grammar is')
        print('\n'.join(map(str, grammar)))
        raise Exception(f'{conflicts} conflicts')
    return table
# entry function {{{2
def make_parse_table():
    print('-- finding item sets')
    isets = get_item_sets()
    print('-- getting table')
    table = fill_parse_table(*isets)
    return table
# rule shorthands {{{1
def nt(sym, name, reduces_to, panickable=False):
    return NonTerminal(sym, panickable, name, reduces_to)

def rule(sym, expansion, reduce_action, special=''):
    rsp = {}
    rsp['defaultreduce'] = True

    for sp in special.split(' '):
        if len(sp) == 0:
            continue

        v = not sp.startswith('no')
        rest = sp[2:] if not v else sp
        if rest in rsp:
            rsp[rest] = v
        else:
            raise Exception(f'invalid special {rest}')

    rule = Rule(sym, expansion, reduce_action, rsp)
    grammar.append(rule)
    return rule

def list_rule(sym, make_list_action, append_list_action, list_class, delimit=None):
    anothersym = nt('Another' + sym.symbol, 'another ' + sym.name, sym.reduces_to, panickable=True) # useless rule to take advantage of "expected another x"
    rule(anothersym, (sym,), SkipReduceAction())

    symlist = nt(sym.symbol + 'List', sym.name + ' list', list_class, panickable=True)

    if delimit is not None:
        symsegment = nt(sym.symbol + 'Segment', sym.name + ' list', list_class, panickable=True)
        rule(symsegment, (symsegment, delimit, anothersym), append_list_action)
        rule(symsegment, (sym,), make_list_action)

        rule(symlist, (symsegment,), SkipReduceAction())
        rule(symlist, (symsegment, delimit), SkipReduceAction())
    else:
        rule(symlist, (symlist, anothersym), append_list_action)
        rule(symlist, (sym,), make_list_action)

    return symlist

def make_opt(toopt, with_action, no_action, new_name=None):
    if new_name is None:
        new_name = 'optional ' + toopt.name

    optsym = toopt.symbol + '_OPT'
    optnt = nt(optsym, new_name, toopt.reduces_to)
    rule(optnt, (toopt,), with_action)
    rule(optnt, (), no_action, special='nodefaultreduce')
    return optnt

def braced_rule(braced_nt, inside_block, reduce_off_1, reduce_off_2, reduce_off_3):
    rule(braced_nt, (OCURB, *inside_block, CCURB), reduce_off_1)
    rule(braced_nt, (OCURB, NEWLINE, *inside_block, CCURB), reduce_off_2)
    rule(braced_nt, (OCURB, NEWLINE, INDENT, *inside_block, DEDENT, CCURB), reduce_off_3)

def indented_rule(indented_nt, inside_block, reduce_action):
    rule(indented_nt, (NEWLINE, INDENT, *inside_block, DEDENT), reduce_action)

def skip_to(skip_from, *skip_to):
    for to in skip_to:
        rule(skip_from, (to,), SkipReduceAction())

def token_rule(nt, reduce_action, *tokens):
    for tok in tokens:
        rule(nt, (tok,), reduce_action)

grammar = []

# rules {{{1

AMPER = Terminal('AMPER')
BANG = Terminal('BANG')
BANGEQUAL = Terminal('BANGEQUAL')
BININTLIT = Terminal('BININTLIT')
CARET = Terminal('CARET')
CCURB = Terminal('CCURB')
CHARLIT = Terminal('CHARLIT')
COLON = Terminal('COLON')
COMMA = Terminal('COMMA')
CPARN = Terminal('CPARN')
DECINTLIT = Terminal('DECINTLIT')
DEDENT = Terminal('DEDENT')
DOUBLEAMPER = Terminal('DOUBLEAMPER')
DOUBLECOLON = Terminal('DOUBLECOLON')
DOUBLEEQUAL = Terminal('DOUBLEEQUAL')
DOUBLEGREATER = Terminal('DOUBLEGREATER')
DOUBLELESS = Terminal('DOUBLELESS')
DOUBLEPIPE = Terminal('DOUBLEPIPE')
EQUAL = Terminal('EQUAL')
ELSE = Terminal('ELSE')
FALSELIT = Terminal('FALSELIT')
FLOATLIT = Terminal('FLOATLIT')
FOR = Terminal('FOR')
FUN = Terminal('FUN')
GREATER = Terminal('GREATER')
GREATEREQUAL = Terminal('GREATEREQUAL')
HEXINTLIT = Terminal('HEXINTLIT')
IDENTIFIER = Terminal('IDENTIFIER')
IF = Terminal('IF')
INDENT = Terminal('INDENT')
IMPL = Terminal('IMPL')
LEFTARROW = Terminal('LEFTARROW')
LESS = Terminal('LESS')
LESSEQUAL = Terminal('LESSEQUAL')
MINUS = Terminal('MINUS')
MUT = Terminal('MUT')
NEWLINE = Terminal('NEWLINE')
NULLPTRLIT = Terminal('NULLPTRLIT')
OCTINTLIT = Terminal('OCTINTLIT')
OCURB = Terminal('OCURB')
OPARN = Terminal('OPARN')
PERCENT = Terminal('PERCENT')
PERIOD = Terminal('PERIOD')
PIPE = Terminal('PIPE')
PLUS = Terminal('PLUS')
RETURN = Terminal('RETURN')
RIGHTARROW = Terminal('RIGHTARROW')
SEMICOLON = Terminal('SEMICOLON')
SLASH = Terminal('SLASH')
STAR = Terminal('STAR')
STRINGLIT = Terminal('STRINGLIT')
TILDE = Terminal('TILDE')
TRUELIT = Terminal('TRUELIT')
VAR = Terminal('VAR')

def make_grammar():
    global AUGMENT_RULE, AUGMENT_SYM

    CU = nt('CU', 'compilation unit', 'CU')
    Decl = nt('Decl', 'declaration', 'Decl', panickable=True)
    FunctionDecl = nt('FunctionDecl', 'function declaration', 'FunctionDecl', panickable=True)
    ImplDecl = nt('ImplDecl', 'implementation', 'Decl', panickable=True)
    ImplBody = nt('ImplBody', 'implementation body', 'ImplBody', panickable=True)
    ImplItem = nt('ImplItem', 'implementation item', 'ImplItem', panickable=True)
    Stmt = nt('Stmt', 'statement', 'Stmt', panickable=True)
    VarStmt = nt('VarStmt', 'variable declaration', 'VarStmt', panickable=True)
    ExprStmt = nt('ExprStmt', 'expression statement', 'ExprStmt', panickable=True)
    RetStmt = nt('RetStmt', 'return statement', 'RetStmt', panickable=True)
    VarStmtItem = nt('VarStmtItem', 'variable binding', 'VarStmtItem')
    LineEnding = nt('LineEnding', 'line ending', 'PureLocation')
    Block = nt('Block', 'code block', 'Block', panickable=True)
    BracedBlock = nt('BracedBlock', 'braced code block', 'Block', panickable=True)
    IndentedBlock = nt('IndentedBlock', 'indented code block', 'Block', panickable=True)
    TypeAnnotation = nt('TypeAnnotation', 'required type annotation', 'Type')
    Type = nt('Type', 'type specifier', 'Type')
    PointerType = nt('PointerType', 'pointer type specifier', 'PointerType')
    PathType = nt('PathType', 'path type specifier', 'PathType')
    Arg = nt('Arg', 'argument', 'Arg', panickable=True)
    Param = nt('Param', 'parameter', 'Param', panickable=True)
    Expr = nt('Expr', 'expression', 'Expr')
    BlockedExpr = nt('BlockedExpr', 'braced expression', 'Expr')
    NotBlockedExpr = nt('NotBlockedExpr', 'non-braced expression', 'Expr')
    IfExpr = nt('IfExpr', 'if expression', 'IfExpr', panickable=True)
    ForExpr = nt('ForExpr', 'for expression', 'ForExpr', panickable=True)
    AssignmentExpr = nt('AssignmentExpr', 'assignment expression', 'Expr')
    BinOrExpr = nt('BinOrExpr', 'binary or expression', 'Expr')
    BinAndExpr = nt('BinAndExpr', 'binary and expression', 'Expr')
    CompEQExpr = nt('CompEQExpr', 'equality expression', 'Expr')
    CompLGTExpr = nt('CompLGTExpr', 'comparison expression', 'Expr')
    BitXorExpr = nt('BitXorExpr', 'bitwise xor expression', 'Expr')
    BitOrExpr = nt('BitOrExpr', 'bitwise or expression', 'Expr')
    BitAndExpr = nt('BitAndExpr', 'bitwise and expression', 'Expr')
    BitShiftExpr = nt('BitShiftExpr', 'bit shift expression', 'Expr')
    AdditionExpr = nt('AdditionExpr', 'addition expression', 'Expr')
    MultExpr = nt('MultExpr', 'multiplication expression', 'Expr')
    CastExpr = nt('CastExpr', 'type cast expression', 'Expr')
    UnaryExpr = nt('UnaryExpr', 'unary expression', 'Expr')
    CallExpr = nt('CallExpr', 'function call expression', 'Expr')
    PrimaryExpr = nt('PrimaryExpr', 'primary expression', 'Expr')
    PathExpr = nt('PathExpr', 'path expression', 'Expr')
    Path = nt('Path', 'symbol path', 'Path')

    AUGMENT_SYM = nt('augment', 'augment', '#error augment symbol reduces to class')
    AUGMENT_RULE = rule(AUGMENT_SYM, (CU,), None)

    ParamList = list_rule(Param, VectorPushOneAction('ASTNS::ParamList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Param>', 'params'), VectorPushReduceAction('a0->params', 'std::move(a2)', 'a0'), 'ParamList', COMMA)
    ArgList = list_rule(Arg, VectorPushOneAction('ASTNS::ArgList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Arg>', 'args'), VectorPushReduceAction('a0->args', 'std::move(a2)', 'a0'), 'ArgList', COMMA)
    VarStmtItemList = list_rule(VarStmtItem, VectorPushOneAction('ASTNS::VarStmtItemList', 'std::move(a0)', 'std::unique_ptr<ASTNS::VarStmtItem>', 'items'), VectorPushReduceAction('a0->items', 'std::move(a2)', 'a0'), 'VarStmtItemList', COMMA)
    StmtList = list_rule(Stmt, VectorPushOneAction('ASTNS::StmtList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Stmt>', 'stmts'), VectorPushReduceAction('a0->stmts', 'std::move(a1)', 'a0'), 'StmtList')
    DeclList = list_rule(Decl, VectorPushOneAction('ASTNS::DeclList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Decl>', 'decls'), VectorPushReduceAction('a0->decls', 'std::move(a1)', 'a0'), 'DeclList')
    ImplItemList = list_rule(ImplItem, VectorPushOneAction('ASTNS::ImplBody', 'std::move(a0)', 'std::unique_ptr<ASTNS::ImplItem>', 'items'), VectorPushReduceAction('a0->items', 'std::move(a1)', 'a0'), 'ImplBody')

    ParamListOpt = make_opt(ParamList, SkipReduceAction(), NullptrReduceAction())
    ArgListOpt = make_opt(ArgList, SkipReduceAction(), NullptrReduceAction())
    StmtListOpt = make_opt(StmtList, SkipReduceAction(), NullptrReduceAction())
    ExprOpt = make_opt(Expr, SkipReduceAction(), NullptrReduceAction())
    VarStmtOpt = make_opt(VarStmt, SkipReduceAction(), NullptrReduceAction())
    LineEndingOpt = make_opt(LineEnding, SkipReduceAction(), NullptrReduceAction())
    TypeAnnotationOpt = make_opt(TypeAnnotation, SkipReduceAction(), NullptrReduceAction(), new_name='optional type annotation')
    ImplItemListOpt = make_opt(ImplItemList, SkipReduceAction(), NullptrReduceAction())

    rule(CU, (DeclList,), SimpleReduceAction('CU', 'std::move(a0)'))
    rule(CU, (), NullptrReduceAction())

    skip_to(Decl, FunctionDecl, ImplDecl)

    rule(FunctionDecl, (FUN, IDENTIFIER, OPARN, ParamListOpt, CPARN, TypeAnnotation, Block, LineEndingOpt), SimpleReduceAction('FunctionDecl', 'std::move(a5), a1, std::move(a3), std::move(a6)'))
    rule(FunctionDecl, (FUN, IDENTIFIER, OPARN, ParamListOpt, CPARN, TypeAnnotation, LineEnding), SimpleReduceAction('FunctionDecl', 'std::move(a5), a1, std::move(a3), nullptr'))

    rule(ImplDecl, (IMPL, Type, ImplBody, LineEndingOpt), SimpleReduceAction('ImplDecl', 'std::move(a1), std::move(a2)'))

    braced_rule(ImplBody, (ImplItemListOpt,),
        SkipReduceAction(1),
        SkipReduceAction(2),
        SkipReduceAction(3))
    indented_rule(ImplBody, (ImplItemListOpt,), SkipReduceAction(2))

    rule(ImplItem, (FunctionDecl,), SimpleReduceAction('FunctionImplItem', 'std::move(a0)'))

    skip_to(Stmt, VarStmt, ExprStmt, RetStmt)

    rule(VarStmt, (VAR, VarStmtItemList, LineEnding), SimpleReduceAction('VarStmt', 'std::move(a1)'))

    rule(ExprStmt, (NotBlockedExpr, LineEnding), SimpleReduceAction('ExprStmt', 'std::move(a0), false'))
    rule(ExprStmt, (BlockedExpr, LineEndingOpt), SimpleReduceAction('ExprStmt', 'std::move(a0), false'))
    rule(ExprStmt, (Expr, PERIOD, LineEnding), SimpleReduceAction('ExprStmt', 'std::move(a0), true'))

    rule(RetStmt, (RETURN, Expr, LineEnding), SimpleReduceAction('RetStmt', 'std::move(a1)'))
    rule(RetStmt, (RETURN, LineEnding), SimpleReduceAction('RetStmt', 'nullptr'))

    rule(VarStmtItem, (IDENTIFIER, TypeAnnotation, EQUAL, Expr), SimpleReduceAction('VarStmtItem', 'std::move(a1), false, a0, a2, std::move(a3)'))
    rule(VarStmtItem, (IDENTIFIER, TypeAnnotation), SimpleReduceAction('VarStmtItem', 'std::move(a1), false, a0, a0, nullptr'))
    rule(VarStmtItem, (MUT, IDENTIFIER, TypeAnnotation, EQUAL, Expr), SimpleReduceAction('VarStmtItem', 'std::move(a2), true, a1, a3, std::move(a4)'))
    rule(VarStmtItem, (MUT, IDENTIFIER, TypeAnnotation), SimpleReduceAction('VarStmtItem', 'std::move(a2), true, a1, a1, nullptr'))

    skip_to(Block, BracedBlock, IndentedBlock)
    braced_rule(BracedBlock, (StmtListOpt,),
        SimpleReduceAction('Block', 'std::move(a1)'), # offset 1
        SimpleReduceAction('Block', 'std::move(a2)'), # offset 2
        SimpleReduceAction('Block', 'std::move(a3)')) # offset 3
    indented_rule(IndentedBlock, (StmtListOpt,), SimpleReduceAction('Block', 'std::move(a2)'))

    rule(LineEnding, (NEWLINE,), LocationReduceAction())
    rule(LineEnding, (SEMICOLON,), LocationReduceAction())
    rule(LineEnding, (SEMICOLON, NEWLINE), WarnAction('WARN_EXTRA_SEMI(a0);', LocationReduceAction()))

    skip_to(Type, PathType, PointerType)

    rule(PointerType, (STAR, Type), SimpleReduceAction('PointerType', 'false, std::move(a1)'))
    rule(PointerType, (STAR, MUT, Type), SimpleReduceAction('PointerType', 'true, std::move(a2)'))

    rule(PathType, (Path,), SimpleReduceAction('PathType', 'std::move(a0)'))

    rule(TypeAnnotation, (COLON, Type), SkipReduceAction(1));

    rule(Arg, (Expr,), SimpleReduceAction('Arg', 'std::move(a0)'))

    rule(Param, (IDENTIFIER, TypeAnnotation), SimpleReduceAction('Param', 'std::move(a1), a0, false'))
    rule(Param, (MUT, IDENTIFIER, TypeAnnotation), SimpleReduceAction('Param', 'std::move(a2), a1, true'))

    skip_to(Expr, BlockedExpr, NotBlockedExpr)
    skip_to(NotBlockedExpr, AssignmentExpr)
    skip_to(BlockedExpr, IfExpr, ForExpr, BracedBlock)

    rule(IfExpr, (IF, Expr, Block), SimpleReduceAction('IfExpr', 'a0, std::move(a1), std::move(a2), nullptr'))
    rule(IfExpr, (IF, Expr, Block, ELSE, Block), SimpleReduceAction('IfExpr', 'a0, std::move(a1), std::move(a2), std::move(a4)'))
    rule(IfExpr, (IF, Expr, Block, ELSE, IfExpr), SimpleReduceAction('IfExpr', 'a0, std::move(a1), std::move(a2), std::move(a4)'))

    rule(ForExpr, (FOR, VarStmtOpt, SEMICOLON, ExprOpt, SEMICOLON, ExprOpt, CPARN, Block), SimpleReduceAction('ForExpr', 'std::move(a1), std::move(a3), std::move(a5), std::move(a7)'))

    bin_expr_reduction = SimpleReduceAction('BinaryExpr', 'std::move(a0), a1, std::move(a2)')

    rule(AssignmentExpr, (BinOrExpr, EQUAL, AssignmentExpr, ), SimpleReduceAction('AssignmentExpr', 'std::move(a0), a1, std::move(a2)'))
    rule(AssignmentExpr, (BinOrExpr,), SkipReduceAction())
    rule(BinOrExpr, (BinOrExpr, DOUBLEPIPE, BinAndExpr, ), SimpleReduceAction('ShortCircuitExpr', 'std::move(a0), a1, std::move(a2)'))
    rule(BinOrExpr, (BinAndExpr,), SkipReduceAction())
    rule(BinAndExpr, (BinAndExpr, DOUBLEAMPER, CompEQExpr, ), SimpleReduceAction('ShortCircuitExpr', 'std::move(a0), a1, std::move(a2)'))
    rule(BinAndExpr, (CompEQExpr,), SkipReduceAction())
    rule(CompEQExpr, (CompEQExpr, BANGEQUAL, CompLGTExpr, ), bin_expr_reduction)
    rule(CompEQExpr, (CompEQExpr, DOUBLEEQUAL, CompLGTExpr, ), bin_expr_reduction)
    rule(CompEQExpr, (CompLGTExpr,), SkipReduceAction())
    rule(CompLGTExpr, (CompLGTExpr, LESS, BitXorExpr, ), bin_expr_reduction)
    rule(CompLGTExpr, (CompLGTExpr, GREATER, BitXorExpr, ), bin_expr_reduction)
    rule(CompLGTExpr, (CompLGTExpr, LESSEQUAL, BitXorExpr, ), bin_expr_reduction)
    rule(CompLGTExpr, (CompLGTExpr, GREATEREQUAL, BitXorExpr, ), bin_expr_reduction)
    rule(CompLGTExpr, (BitXorExpr,), SkipReduceAction())
    rule(BitXorExpr, (BitXorExpr, CARET, BitOrExpr, ), bin_expr_reduction)
    rule(BitXorExpr, (BitOrExpr,), SkipReduceAction())
    rule(BitOrExpr, (BitOrExpr, PIPE, BitAndExpr, ), bin_expr_reduction)
    rule(BitOrExpr, (BitAndExpr,), SkipReduceAction())
    rule(BitAndExpr, (BitAndExpr, AMPER, BitShiftExpr, ), bin_expr_reduction)
    rule(BitAndExpr, (BitShiftExpr,), SkipReduceAction())
    rule(BitShiftExpr, (BitShiftExpr, DOUBLEGREATER, AdditionExpr, ), bin_expr_reduction)
    rule(BitShiftExpr, (BitShiftExpr, DOUBLELESS, AdditionExpr, ), bin_expr_reduction)
    rule(BitShiftExpr, (AdditionExpr,), SkipReduceAction())
    rule(AdditionExpr, (AdditionExpr, PLUS, MultExpr, ), bin_expr_reduction)
    rule(AdditionExpr, (AdditionExpr, MINUS, MultExpr, ), bin_expr_reduction)
    rule(AdditionExpr, (MultExpr,), SkipReduceAction())
    rule(MultExpr, (MultExpr, STAR, UnaryExpr, ), bin_expr_reduction)
    rule(MultExpr, (MultExpr, SLASH, UnaryExpr, ), bin_expr_reduction)
    rule(MultExpr, (MultExpr, PERCENT, UnaryExpr, ), bin_expr_reduction)
    rule(MultExpr, (CastExpr,), SkipReduceAction())
    rule(CastExpr, (CastExpr, RIGHTARROW, Type), SimpleReduceAction('CastExpr', 'std::move(a2), std::move(a0)'))
    rule(CastExpr, (UnaryExpr,), SkipReduceAction())
    rule(UnaryExpr, (TILDE, UnaryExpr, ), SimpleReduceAction('UnaryExpr', 'a0, std::move(a1)'))
    rule(UnaryExpr, (MINUS, UnaryExpr, ), SimpleReduceAction('UnaryExpr', 'a0, std::move(a1)'))
    rule(UnaryExpr, (BANG, UnaryExpr, ), SimpleReduceAction('UnaryExpr', 'a0, std::move(a1)'))
    rule(UnaryExpr, (AMPER, UnaryExpr, ), SimpleReduceAction('AddrofExpr', 'a0, std::move(a1), false'))
    rule(UnaryExpr, (AMPER, MUT, UnaryExpr, ), SimpleReduceAction('AddrofExpr', 'a0, std::move(a2), true'))
    rule(UnaryExpr, (STAR, UnaryExpr, ), SimpleReduceAction('DerefExpr', 'a0, std::move(a1)'))
    rule(UnaryExpr, (CallExpr,), SkipReduceAction())
    rule(CallExpr, (CallExpr, OPARN, ArgListOpt, CPARN, ), SimpleReduceAction('CallExpr', 'std::move(a0), a1, std::move(a2)'))
    rule(CallExpr, (PrimaryExpr,), SkipReduceAction())

    token_rule(PrimaryExpr, SimpleReduceAction('PrimaryExpr', 'a0'), TRUELIT, FALSELIT, FLOATLIT, NULLPTRLIT, DECINTLIT, OCTINTLIT, BININTLIT, HEXINTLIT, CHARLIT, STRINGLIT)
    rule(PrimaryExpr, (OPARN, Expr, CPARN), SkipReduceAction(1))
    rule(PrimaryExpr, (PathExpr,), SkipReduceAction())
    rule(PathExpr, (Path,), SimpleReduceAction('PathExpr', 'std::move(a0)'))

    rule(Path, (Path, DOUBLECOLON, IDENTIFIER), VectorPushReduceAction('a0->segments', 'a2', 'a0'))
    rule(Path, (IDENTIFIER,), VectorPushOneAction('ASTNS::Path', 'a0', 'Token', 'segments'))

make_grammar()

# convert grammar {{{1
eof_sym = Terminal('EOF_')

symbols = [eof_sym]
for rule in grammar:
    for sym in [rule.symbol, *rule.expansion]:
        if sym not in symbols:
            symbols.append(sym)

print('-- finding first and follows')
FIRSTS = find_firsts()
FOLLOWS, NT_FOLLOWS = find_follows()

# make the parse table {{{1
table = make_parse_table()
# generating stuff {{{1
# print parse table {{{2
def print_parse_table(pad=True):
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
def format_list(l):
    if len(l) == 1:
        return f'{l[0]}'
    elif len(l) == 2:
        return f'format("either % or %", {l[0]}, {l[1]})'
    elif len(l) == 0:
        return '"nothing"'
    else:
        return 'format("' + ", ".join('%' for _ in l[:-1]) + ', or %", ' +  ', '.join(l) + ')'
# }}}
def gen_loop():
    output = []

    output.append(                                '    bool done = false;\n')
    output.append(                                '    bool errored = false;\n')
    output.append(                                '    int steps = 0;\n')
    output.append(                                '    Token lookahead (_lookahead); // for when you need to inject a new token\n')
    output.append(                                '    Token lasttok = lookahead;\n')

    output.append(                                '    while (!done) {\n')
    output.append(                                '        if (istrial && steps > 5)\n')
    output.append(                                '            return true;\n')
    output.append(                                '        switch (stack.back().state) {\n')

    for staten, state in sorted(table.items(), key=lambda x:x[0]):
        output.append(                           f'            case {staten}:\n')
        output.append(                            '                switch (lookahead.type) {\n')

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

        statereduces = [ac for ac in stateactions if isinstance(ac[0], ReduceAction)]
        reduce_only = len(statereduces) == 1 and statereduces[0][0].rule.special['defaultreduce']
        for ac, nts in stateactions:
            if isinstance(ac, ShiftAction):
                for term in nts:
                    output.append(               f'                    case {term.astt()}:\n')
                output.append(                   f'                        shift(p, lasttok, lookahead, stack, steps, {ac.newstate}); break;\n')
                continue

            if reduce_only:
                output.append(                    '                    default: ')
                # do not check for lookahead, just reduce to have better performance (kind of)
                # if reduce_only, then all the reduce actions of this state reduce the same rule
                # and according to Wikipedia, just reducing regardless of the lookahead in
                # these states will cause a few "harmless reductions", and errors will just be
                # reported after a few reduces
                # this actually helps with error reporting because if you have "return 2",
                # it will reduce 2 up the chain of expression precedence before reporting the error
            else:
                for term in nts:
                    output.append(               f'                    case {term.astt()}:\n')

            if isinstance(ac, ReduceAction):
                output.append(                    '{\n')

                firstterminal = None
                for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                    if isinstance(sym, Terminal):
                        output.append(           f'                            auto a{i} (popT(stack));\n')
                        firstterminal = i
                    elif isinstance(sym, NonTerminal):
                        output.append(           f'                            auto a{i} (popA<ASTNS::{sym.reduces_to}>(stack));\n')

                if len(ac.rule.expansion) > 0:
                    if firstterminal is not None:
                        output.append(                        '                            Location start, end;\n')
                    else:
                        if isinstance(ac.rule.expansion[0], Terminal):
                            output.append(                    '                            Location start, end;\n')
                        else:
                            output.append(                    '                            Location start ((a0.get())), end ((a0.get()));\n')
                    for i in range(len(ac.rule.expansion)):
                        if isinstance(ac.rule.expansion[i], Terminal):
                            if i == 0:
                                output.append(               f'                            start = a{i};\n')
                            else:
                                output.append(               f'                            else start = a{i};\n')
                            break
                        else:
                            if i == 0:
                                output.append(               f'                            if (a{i}) start = a{i}->start();\n')
                            else:
                                output.append(               f'                            else if (a{i}) start = a{i}->start();\n')

                    for i in range(len(ac.rule.expansion) - 1, -1, -1):
                        if isinstance(ac.rule.expansion[i], Terminal):
                            if i == len(ac.rule.expansion) - 1:
                                output.append(               f'                            end = a{i};\n')
                            else:
                                output.append(               f'                            else end = a{i};\n')
                            break
                        else:
                            if i == len(ac.rule.expansion) - 1:
                                output.append(               f'                            if (a{i}) end = a{i}->end();\n')
                            else:
                                output.append(               f'                            else if (a{i}) end = a{i}->end();\n')

                reduce_code, pushitem = ac.rule.reduce_action.generate()
                output.append(reduce_code)
                output.append(                   f'                            std::unique_ptr<ASTNS::{ac.rule.symbol.reduces_to}> pushitem = {pushitem};\n')
                output.append(                   f'                            stack.emplace_back(getGoto(NonTerminal::{ac.rule.symbol.symbol}, stack.back().state), std::move(pushitem), NonTerminal::{ac.rule.symbol.symbol});\n')
                output.append(                    '                        }\n')


            elif isinstance(ac, AcceptAction):
                output.append(                    '                            done = true;\n')
            else:
                raise Exception('invalid action type')

            output.append(                        '                        break;\n')

        if not reduce_only:
            def stc(s):
                if isinstance(s, NonTerminal):
                    return f'"{s.name}"'
                else:
                    return f'stringifyTokenType({s.astt()})'

            output.append(                        '                    default:\n')
            output.append(                        '                        if (istrial) return false;\n')

            futuress = [f'format("expected % for %", {format_list([stc(p) for p in future])}, {stc(nt)})' for nt, future in state.futures.items()]
            terminatess = [f'format("expected % to terminate %", {format_list([stc(p) for p in future])}, {stc(nt)})' for nt, future in state.terminates.items()]
            output.append(                       f'                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {{  {", ".join(futuress + terminatess)}  }});\n')
        output.append(                            '                }\n')
        output.append(                            '                break;\n')

    output.append(                                '            default:\n')
    output.append(                                '                reportAbortNoh(format("Parser reached invalid state: %", stack.back().state));\n')

    output.append(                                '        }\n')
    output.append(                                '    }\n')

    return ''.join(output)
# generate goto code {{{2
def gen_goto():
    output = []

    output.append(                                'size_t getGoto(NonTerminal nterm, size_t state) {\n')
    output.append(                                '    switch (nterm) {\n')
    for nonterm in symbols:
        if isinstance(nonterm, Terminal):
            continue

        output.append(                           f'        case NonTerminal::{nonterm.symbol}:\n')
        output.append(                            '            switch (state) {\n')

        returns = {}
        for staten, state in table.items():
            if nonterm in state.goto:
                if state.goto[nonterm] in returns: # there is already a state in which this goto is returned (squishing rows together)
                    returns[state.goto[nonterm]].append(staten)
                else:
                    returns[state.goto[nonterm]] = [staten]

        for retval, states in returns.items():
            output.append(                        '                ')
            for state in states:
                output.append(                   f'case {state}: ')
            output.append(                       f'\n                    return {retval};\n')

        output.append(                            '                default: reportAbortNoh("get invalid goto");\n')

        output.append(                            '            }\n')

    output.append(                                '    }\n')
    output.append(                                '}\n')

    return ''.join(output)

# generate nonterminal enum {{{2
def gen_non_term_enum():
    output = []
    for symbol in symbols:
        if isinstance(symbol, NonTerminal):
            output.append(f'    {symbol.symbol},\n')
    return ''.join(output)
# generate panic mode error recovery code {{{2
def gen_panic_mode():
    output = []
    output.append(                               ('#define CHECKASI(ty)\\\n'
                                                  '    if (nterm == NonTerminal::ty) {\\\n'
                                                  '        switch (e.lookahead.type) {\n'
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
                                                  '    while (!valid) {\n'
                                                  '        for (auto i = e.stack.rbegin(); i != e.stack.rend() && !valid; ++i) {\n'
                                                  '            if (std::holds_alternative<astitem>(i->item)) {\n'
                                                  '                NonTerminal nterm = std::get<astitem>(i->item).nt;\n'))

    for nonterm in symbols:
        if not isinstance(nonterm, NonTerminal):
            continue
        if nonterm == AUGMENT_SYM:
            continue
        if not nonterm.panickable:
            continue

        output.append(                           f'                CHECKASI({str(nonterm)})\n')
        output.append(                            '                       ')
        for follow in FOLLOWS[nonterm]:
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
def gen_single_tok():
    output = []
    output.append(                                '#define TRYINSERT(ty) if (tryInsert(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::INSERT, ty}; if (score(f) > score(bestfix)) bestfix = f;}\n')
    output.append(                                '#define TRYSUB(ty) if (trySub(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::SUBSTITUTE, ty}; if (score(f) > score(bestfix)) bestfix = f;}\n')
    output.append(                                '#define TRYTOKTY(ty) TRYINSERT(ty); TRYSUB(ty);\n')

    for terminal in symbols:
        if isinstance(terminal, Terminal):
            output.append(                       f'    TRYTOKTY({terminal.astt()})\n')

    output.append(                                '    if (tryDel(e.p, e.stack)) {fix f = fix {fix::fixtype::REMOVE, static_cast<TokenType>(-1)}; if (score(f) > score(bestfix)) bestfix = f;};\n')
    return ''.join(output)
# entry {{{1
if __name__ == '__main__':
    print_parse_table(False)
