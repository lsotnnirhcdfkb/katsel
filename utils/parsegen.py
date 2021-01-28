#!/usr/bin/env python3

# classes {{{1
# symbols {{{2
class NonTerminal:
    __current_id = 0
    def __init__(self, panickable, name, reduces_to):
        self.name = name
        self.panickable = panickable
        self.reduces_to = reduces_to
        self.id = NonTerminal.__current_id
        NonTerminal.__current_id += 1

    def __repr__(self):
        return str(self)
    def __str__(self):
        return f'_{self.id}'

    def __hash__(self):
        return hash(self.id)

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
        return f'Tokens::{self.symbol}'

    def __hash__(self):
        return hash(self.symbol)
# rule {{{2
class Rule:
    __num = 0
    def __init__(self, symbol, expansion, reduce_action, special, loc_start, loc_end):
        self.symbol = symbol
        self.expansion = expansion
        self.num = Rule.__num
        Rule.__num += 1
        self.reduce_action = reduce_action
        self.special = special
        self.loc_start, self.loc_end = loc_start, loc_end

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
        self.after = self.rule.expansion[self.index] if self.index < len(self.rule.expansion) else None

    @staticmethod
    def get(rule, index, lookahead):
        itemtuple = (rule, index, lookahead)
        if found := Item.items.get(itemtuple):
            return found

        i = Item(*itemtuple)
        Item.items[itemtuple] = i
        return i

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
        if sym in self.actions:
            conflict_type = 'sr' if isinstance(self.actions[sym], ShiftAction) or isinstance(action, ShiftAction) else 'rr'
            having_parsed = self.set_.kernel[0].rule.expansion[:self.set_.kernel[0].index]
            while_parsing = set(item.rule.symbol for item in self.set_.kernel)
            possible_nexts = set(item.after for item in self.set_.kernel)

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
        if sym in self.goto:
            self.goto[sym] = None
            print('goto conflict')
            return False

        self.goto[sym] = newstate
        return True

    def make_description(self):
        self.futures = {}
        self.terminates = {}

        for item in self.set_.kernel:
            if item.after is not None:
                if item.rule.symbol not in self.futures:
                    self.futures[item.rule.symbol] = []
                if item.after not in self.futures[item.rule.symbol]:
                    self.futures[item.rule.symbol].append(item.after)
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
        return (f'std::unique_ptr<ASTNS::{self.classname}> push (std::make_unique<ASTNS::{self.classname}>(p.sourcefile, span, {self.args}));\n', 'std::move(push)')
class EmptyVectorAction:
    def __init__(self, classname, ty):
        self.classname = classname
        self.ty = ty
    def generate(self):
        return (f'std::unique_ptr<ASTNS::{self.classname}> push (std::make_unique<ASTNS::{self.classname}>(p.sourcefile, span, std::vector<{self.ty}> {{}}));\n', 'std::move(push)')
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
        return (f'''std::unique_ptr<{self.new_class}> push (std::make_unique<{self.new_class}>(p.sourcefile, span, std::vector<{self.itemtype}> {{}}));\n
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
                    while addamt == 1 or (i + addamt - 1 < len(rule.expansion) and any([len(r.expansion) == 0 for r in grammar if r.symbol == rule.expansion[i + addamt - 1]])):
                        add_firsts_of(i + addamt)
                        addamt += 1
    return (follows, nt_follows)

# closure {{{2
__rules_by_sym = {}
def get_closure_lr0(lr0set):
    if len(__rules_by_sym) == 0:
        for rule in grammar:
            sym = rule.symbol
            if sym not in __rules_by_sym:
                __rules_by_sym[sym] = []
            __rules_by_sym[sym].append(rule)

    kernel = lr0set
    extras = []
    stack = list(lr0set)
    while len(stack) > 0:
        item_rule, index = stack.pop(0)

        if index < len(item_rule.expansion):
            after = item_rule.expansion[index]
            if isinstance(after, NonTerminal):
                for rule in __rules_by_sym[after]:
                    newitem = (rule, 0)
                    if newitem not in extras and newitem not in kernel:
                        extras.append(newitem)
                        stack.append(newitem)

    return lr0_to_lr1(kernel, extras)
# lr0_to_lr1 {{{2
def lr0_to_lr1(kernel, extras):
    lr1kernel = []
    lr1extras = []

    for item_rule, item_ind in kernel:
        for follow in FOLLOWS[item_rule.symbol]:
            lr1kernel.append(Item.get(item_rule, item_ind, follow))

    for item_rule, item_ind in extras:
        for follow in FOLLOWS[item_rule.symbol]:
            lr1extras.append(Item.get(item_rule, item_ind, follow))

    return ItemSet.get(lr1kernel, lr1extras)
# make parser table {{{1
# find item sets {{{2
def get_item_sets():
    initial = get_closure_lr0([(AUGMENT_RULE, 0)])

    isets = [initial]
    transitions = []

    stack = [initial]
    while len(stack) > 0:
        origset = stack.pop(0)

        afters = {}
        for item in origset.items():
            after = item.after
            if after is not None:
                if after not in afters:
                    afters[after] = []

                newitem = (item.rule, item.index + 1)
                if newitem not in afters[after]:
                    afters[after].append(newitem)

        for after, afternewset in afters.items():
            newsetlr1 = get_closure_lr0(afternewset)
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
        for item in filter(lambda i: i.after is None, iset.items()):
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
def nt(name, reduces_to, panickable=False):
    return NonTerminal(panickable, name, reduces_to)

def rule(sym, expansion, reduce_action, special='', loc_start=None, loc_end=None):
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

    if loc_start is None:
        loc_start = 0
    if loc_end is None:
        loc_end = len(expansion) - 1

    rule = Rule(sym, expansion, reduce_action, rsp, loc_start, loc_end)
    grammar.append(rule)
    return rule

def list_rule(sym, make_list_action, append_list_action, list_class, delimit=None):
    anothersym = nt('another ' + sym.name, sym.reduces_to, panickable=True) # useless rule to take advantage of "expected another x"
    rule(anothersym, (sym,), SkipReduceAction())

    symlist = nt(sym.name + ' list', list_class, panickable=True)

    if delimit is not None:
        symsegment = nt(sym.name + ' list', list_class, panickable=True)
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

    optnt = nt(new_name, toopt.reduces_to)
    rule(optnt, (toopt,), with_action)
    rule(optnt, (), no_action, special='nodefaultreduce')
    return optnt

def braced_rule(braced_nt, inside_block, warn_no_indent, reduce_off_1, reduce_off_2, reduce_off_3):
    rule(braced_nt, (OBrace, *inside_block, CBrace), reduce_off_1)
    if warn_no_indent:
        reduce_off_2 = WarnAction(f'WARN_BLOCK_NO_INDENT(a0.span, a{len(inside_block) + 2}.span);', reduce_off_2)
    rule(braced_nt, (OBrace, Newline, *inside_block, CBrace), reduce_off_2)
    rule(braced_nt, (OBrace, Newline, Indent, *inside_block, Dedent, CBrace), reduce_off_3)

def indented_rule(indented_nt, inside_block, reduce_action):
    rule(indented_nt, (Newline, Indent, *inside_block, Dedent), reduce_action)

def skip_to(skip_from, *skip_to):
    for to in skip_to:
        rule(skip_from, (to,), SkipReduceAction())

def token_rule(nt, reduce_action, *tokens):
    for tok in tokens:
        rule(nt, (tok,), reduce_action)

grammar = []

# rules {{{1
OParen = Terminal('OParen')
CParen = Terminal('CParen')
OBrack = Terminal('OBrack')
CBrack = Terminal('CBrack')
OBrace = Terminal('OBrace')
CBrace = Terminal('CBrace')
Comma = Terminal('Comma')
Period = Terminal('Period')
Semicolon = Terminal('Semicolon')
Question = Terminal('Question')
Colon = Terminal('Colon')
Bang = Terminal('Bang')
Plus = Terminal('Plus')
Minus = Terminal('Minus')
Star = Terminal('Star')
Slash = Terminal('Slash')
Percent = Terminal('Percent')
Equal = Terminal('Equal')
Greater = Terminal('Greater')
Less = Terminal('Less')
Tilde = Terminal('Tilde')
Amper = Terminal('Amper')
Pipe = Terminal('Pipe')
Caret = Terminal('Caret')
Dollar = Terminal('Dollar')
Hash = Terminal('Hash')
RightArrow = Terminal('RightArrow')
LeftArrow = Terminal('LeftArrow')
DoublePlus = Terminal('DoublePlus')
DoubleMinus = Terminal('DoubleMinus')
DoubleGreater = Terminal('DoubleGreater')
DoubleLess = Terminal('DoubleLess')
DoubleAmper = Terminal('DoubleAmper')
DoublePipe = Terminal('DoublePipe')
DoubleEqual = Terminal('DoubleEqual')
DoubleColon = Terminal('DoubleColon')
PlusEqual = Terminal('PlusEqual')
MinusEqual = Terminal('MinusEqual')
StarEqual = Terminal('StarEqual')
SlashEqual = Terminal('SlashEqual')
BangEqual = Terminal('BangEqual')
GreaterEqual = Terminal('GreaterEqual')
LessEqual = Terminal('LessEqual')
PercentEqual = Terminal('PercentEqual')
DoubleLessEqual = Terminal('DoubleLessEqual')
DoubleGreaterEqual = Terminal('DoubleGreaterEqual')
AmperEqual = Terminal('AmperEqual')
PipeEqual = Terminal('PipeEqual')
CaretEqual = Terminal('CaretEqual')
Identifier = Terminal('Identifier')
CharLit = Terminal('CharLit')
StringLit = Terminal('StringLit')
IntLit = Terminal('IntLit')
FloatLit = Terminal('FloatLit')
BoolLit = Terminal('BoolLit')
This = Terminal('This')
Var = Terminal('Var')
Fun = Terminal('Fun')
Let = Terminal('Let')
Mut = Terminal('Mut')
Data = Terminal('Data')
Impl = Terminal('Impl')
Return = Terminal('Return')
While = Terminal('While')
For = Terminal('For')
If = Terminal('If')
Else = Terminal('Else')
Case = Terminal('Case')
Break = Terminal('Break')
Continue = Terminal('Continue')
Boom = Terminal('Boom')
Newline = Terminal('Newline')
Indent = Terminal('Indent')
Dedent = Terminal('Dedent')
Error = Terminal('Error')

CU = nt('compilation unit', 'std::unique_ptr<ASTNS::CU>')
Decl = nt('declaration', 'std::unique_ptr<ASTNS::Decl>', panickable=True)
FunctionDecl = nt('function declaration', 'std::unique_ptr<ASTNS::FunctionDecl>', panickable=True)
ImplDecl = nt('implementation', 'std::unique_ptr<ASTNS::Decl>', panickable=True)
ImplBody = nt('implementation body', 'std::unique_ptr<ASTNS::ImplMemberList>', panickable=True)
ImplMember = nt('implementation member', 'std::unique_ptr<ASTNS::ImplMember>', panickable=True)
Stmt = nt('statement', 'std::unique_ptr<ASTNS::Stmt>', panickable=True)
VarStmt = nt('variable declaration', 'std::unique_ptr<ASTNS::VarStmt>', panickable=True)
ExprStmt = nt('expression statement', 'std::unique_ptr<ASTNS::ExprStmt>', panickable=True)
RetStmt = nt('return statement', 'std::unique_ptr<ASTNS::RetStmt>', panickable=True)
VarStmtItem = nt('variable binding', 'std::unique_ptr<ASTNS::VarStmtItem>')
LineEnding = nt('line ending', 'std::unique_ptr<ASTNS::PureLocation>')
Block = nt('code block', 'std::unique_ptr<ASTNS::Block>', panickable=True)
BracedBlock = nt('braced code block', 'std::unique_ptr<ASTNS::Block>', panickable=True)
IndentedBlock = nt('indented code block', 'std::unique_ptr<ASTNS::Block>', panickable=True)
TypeAnnotation = nt('required type annotation', 'std::unique_ptr<ASTNS::Type>')
Type = nt('type specifier', 'std::unique_ptr<ASTNS::Type>')
PointerType = nt('pointer type', 'std::unique_ptr<ASTNS::PointerType>')
ThisType = nt('\'this\' type', 'std::unique_ptr<ASTNS::ThisType>')
PathType = nt('path type', 'std::unique_ptr<ASTNS::PathType>')
Arg = nt('argument', 'std::unique_ptr<ASTNS::Arg>', panickable=True)
Param = nt('function parameter', 'std::unique_ptr<ASTNS::ParamB>', panickable=True)
ThisParam = nt('\'this\' function parameter', 'std::unique_ptr<ASTNS::ThisParam>', panickable=True)
NormalParam = nt('function parameter', 'std::unique_ptr<ASTNS::Param>', panickable=True)
Expr = nt('expression', 'std::unique_ptr<ASTNS::Expr>')
BlockedExpr = nt('braced expression', 'std::unique_ptr<ASTNS::Expr>')
NotBlockedExpr = nt('non-braced expression', 'std::unique_ptr<ASTNS::Expr>')
IfExpr = nt('if expression', 'std::unique_ptr<ASTNS::IfExpr>', panickable=True)
WhileExpr = nt('while loop expression', 'std::unique_ptr<ASTNS::WhileExpr>', panickable=True)
AssignmentExpr = nt('assignment expression', 'std::unique_ptr<ASTNS::Expr>')
BinOrExpr = nt('binary or expression', 'std::unique_ptr<ASTNS::Expr>')
BinAndExpr = nt('binary and expression', 'std::unique_ptr<ASTNS::Expr>')
CompEQExpr = nt('equality expression', 'std::unique_ptr<ASTNS::Expr>')
CompLGTExpr = nt('comparison expression', 'std::unique_ptr<ASTNS::Expr>')
BitXorExpr = nt('bitwise xor expression', 'std::unique_ptr<ASTNS::Expr>')
BitOrExpr = nt('bitwise or expression', 'std::unique_ptr<ASTNS::Expr>')
BitAndExpr = nt('bitwise and expression', 'std::unique_ptr<ASTNS::Expr>')
BitShiftExpr = nt('bit shift expression', 'std::unique_ptr<ASTNS::Expr>')
AdditionExpr = nt('addition expression', 'std::unique_ptr<ASTNS::Expr>')
MultExpr = nt('multiplication expression', 'std::unique_ptr<ASTNS::Expr>')
CastExpr = nt('type cast expression', 'std::unique_ptr<ASTNS::Expr>')
UnaryExpr = nt('unary expression', 'std::unique_ptr<ASTNS::Expr>')
CallExpr = nt('function call expression', 'std::unique_ptr<ASTNS::Expr>')
FieldAccessExpr = nt('field access expression', 'std::unique_ptr<ASTNS::Expr>')
MethodCallExpr = nt('method call expression', 'std::unique_ptr<ASTNS::Expr>')
PrimaryExpr = nt('primary expression', 'std::unique_ptr<ASTNS::Expr>')
PathExpr = nt('path expression', 'std::unique_ptr<ASTNS::Expr>')
Path = nt('symbol path', 'std::unique_ptr<ASTNS::Path>')

AUGMENT_SYM = nt('augment', 'std::unique_ptr<ASTNS::AST>')
AUGMENT_RULE = rule(AUGMENT_SYM, (CU,), None)

ParamList = list_rule(Param, VectorPushOneAction('ASTNS::ParamList', 'std::move(a0)', 'std::unique_ptr<ASTNS::ParamB>', 'params'), VectorPushReduceAction('a0->params', 'std::move(a2)', 'a0'), 'std::unique_ptr<ASTNS::ParamList>', Comma)
ArgList = list_rule(Arg, VectorPushOneAction('ASTNS::ArgList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Arg>', 'args'), VectorPushReduceAction('a0->args', 'std::move(a2)', 'a0'), 'std::unique_ptr<ASTNS::ArgList>', Comma)
VarStmtItemList = list_rule(VarStmtItem, VectorPushOneAction('ASTNS::VarStmtItemList', 'std::move(a0)', 'std::unique_ptr<ASTNS::VarStmtItem>', 'items'), VectorPushReduceAction('a0->items', 'std::move(a2)', 'a0'), 'std::unique_ptr<ASTNS::VarStmtItemList>', Comma)
StmtList = list_rule(Stmt, VectorPushOneAction('ASTNS::StmtList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Stmt>', 'stmts'), VectorPushReduceAction('a0->stmts', 'std::move(a1)', 'a0'), 'std::unique_ptr<ASTNS::StmtList>')
DeclList = list_rule(Decl, VectorPushOneAction('ASTNS::DeclList', 'std::move(a0)', 'std::unique_ptr<ASTNS::Decl>', 'decls'), VectorPushReduceAction('a0->decls', 'std::move(a1)', 'a0'), 'std::unique_ptr<ASTNS::DeclList>')
ImplMemberList = list_rule(ImplMember, VectorPushOneAction('ASTNS::ImplMemberList', 'std::move(a0)', 'std::unique_ptr<ASTNS::ImplMember>', 'members'), VectorPushReduceAction('a0->members', 'std::move(a1)', 'a0'), 'std::unique_ptr<ASTNS::ImplMemberList>')

ParamListOpt = make_opt(ParamList, SkipReduceAction(), EmptyVectorAction('ParamList', 'std::unique_ptr<ASTNS::ParamB>'))
ArgListOpt = make_opt(ArgList, SkipReduceAction(), EmptyVectorAction('ArgList', 'std::unique_ptr<ASTNS::Arg>'))
StmtListOpt = make_opt(StmtList, SkipReduceAction(), EmptyVectorAction('StmtList', 'std::unique_ptr<ASTNS::Stmt>'))
ImplMemberListOpt = make_opt(ImplMemberList, SkipReduceAction(), EmptyVectorAction('ImplMemberList', 'std::unique_ptr<ASTNS::ImplMember>'))
ExprOpt = make_opt(Expr, SkipReduceAction(), NullptrReduceAction())
VarStmtOpt = make_opt(VarStmt, SkipReduceAction(), NullptrReduceAction())
LineEndingOpt = make_opt(LineEnding, SkipReduceAction(), NullptrReduceAction())
TypeAnnotationOpt = make_opt(TypeAnnotation, SkipReduceAction(), NullptrReduceAction(), new_name='optional type annotation')

rule(CU, (DeclList,), SimpleReduceAction('CU', 'std::move(a0->decls)'))
rule(CU, (), NullptrReduceAction())

skip_to(Decl, FunctionDecl, ImplDecl)

rule(FunctionDecl, (Fun, Identifier, OParen, ParamListOpt, CParen, TypeAnnotation, Block, LineEndingOpt), SimpleReduceAction('FunctionDecl', 'std::move(a5), a1, std::move(a3->params), std::move(a6)'), loc_end=5)
rule(FunctionDecl, (Fun, Identifier, OParen, ParamListOpt, CParen, TypeAnnotation, LineEnding), SimpleReduceAction('FunctionDecl', 'std::move(a5), a1, std::move(a3->params), nullptr'))

rule(ImplDecl, (Impl, Type, ImplBody, LineEndingOpt), SimpleReduceAction('ImplDecl', 'std::move(a1), std::move(a2->members)'), loc_end=1)

braced_rule(ImplBody, (ImplMemberListOpt,), True,
    SkipReduceAction(1),
    SkipReduceAction(2),
    SkipReduceAction(3))
indented_rule(ImplBody, (ImplMemberListOpt,), SkipReduceAction(2))

rule(ImplMember, (FunctionDecl,), SimpleReduceAction('FunctionImplMember', 'std::move(a0)'))

skip_to(Stmt, VarStmt, ExprStmt, RetStmt)

rule(VarStmt, (Var, VarStmtItemList, LineEnding), SimpleReduceAction('VarStmt', 'std::move(a1->items)'))

rule(ExprStmt, (NotBlockedExpr,         LineEnding),    SimpleReduceAction('ExprStmt', 'std::move(a0), false, Maybe<Span const>()'))
rule(ExprStmt, (BlockedExpr   ,         LineEndingOpt), SimpleReduceAction('ExprStmt', 'std::move(a0), false, Maybe<Span const>()'))
rule(ExprStmt, (NotBlockedExpr, Dollar, LineEndingOpt), SimpleReduceAction('ExprStmt', 'std::move(a0), true , Maybe<Span const>(a1.span)'))
rule(ExprStmt, (BlockedExpr   , Dollar, LineEndingOpt), SimpleReduceAction('ExprStmt', 'std::move(a0), true , Maybe<Span const>(a1.span)'))

rule(RetStmt, (Return, Expr, LineEnding), SimpleReduceAction('RetStmt', 'std::move(a1)'))
rule(RetStmt, (Return, LineEnding), SimpleReduceAction('RetStmt', 'nullptr'))

rule(VarStmtItem, (     Identifier, TypeAnnotation, Equal, Expr), SimpleReduceAction('VarStmtItem', 'std::move(a1), false, a0, a2, std::move(a3)'))
rule(VarStmtItem, (Mut, Identifier, TypeAnnotation, Equal, Expr), SimpleReduceAction('VarStmtItem', 'std::move(a2), true, a1, a3, std::move(a4)'))

rule(VarStmtItem, (     Identifier, TypeAnnotation), SimpleReduceAction('VarStmtItem', 'std::move(a1), false, a0, Maybe<Located<Tokens::Equal>>(), nullptr'))
rule(VarStmtItem, (Mut, Identifier, TypeAnnotation), SimpleReduceAction('VarStmtItem', 'std::move(a2), true, a1, Maybe<Located<Tokens::Equal>>(), nullptr'))

skip_to(Block, BracedBlock, IndentedBlock)
braced_rule(BracedBlock, (StmtListOpt,), True,
    SimpleReduceAction('Block', 'std::move(a1->stmts)'), # offset 1
    SimpleReduceAction('Block', 'std::move(a2->stmts)'), # offset 2
    SimpleReduceAction('Block', 'std::move(a3->stmts)')) # offset 3
indented_rule(IndentedBlock, (StmtListOpt,), SimpleReduceAction('Block', 'std::move(a2->stmts)'))

rule(LineEnding, (Newline,), LocationReduceAction())
rule(LineEnding, (Semicolon,), LocationReduceAction())
rule(LineEnding, (Semicolon, Newline), WarnAction('WARN_EXTRA_SEMI(a0.span);', LocationReduceAction()))

skip_to(Type, PathType, PointerType, ThisType)

rule(PointerType, (Star, Type), SimpleReduceAction('PointerType', 'false, std::move(a1)'))
rule(PointerType, (Star, Mut, Type), SimpleReduceAction('PointerType', 'true, std::move(a2)'))

rule(PathType, (Path,), SimpleReduceAction('PathType', 'std::move(a0)'))

rule(ThisType, (This,), SimpleReduceAction('ThisType', 'a0'))

rule(TypeAnnotation, (Colon, Type), SkipReduceAction(1))

rule(Arg, (Expr,), SimpleReduceAction('Arg', 'std::move(a0)'))

skip_to(Param, NormalParam, ThisParam)

rule(NormalParam, (Identifier, TypeAnnotation), SimpleReduceAction('Param', 'std::move(a1), a0, false'))
rule(NormalParam, (Mut, Identifier, TypeAnnotation), SimpleReduceAction('Param', 'std::move(a2), a1, true'))
rule(ThisParam, (This,), SimpleReduceAction('ThisParam', 'false, false'))
rule(ThisParam, (Star, This), SimpleReduceAction('ThisParam', 'true, false'))
rule(ThisParam, (Star, Mut, This), SimpleReduceAction('ThisParam', 'true, true'))

skip_to(Expr, BlockedExpr, NotBlockedExpr)
skip_to(NotBlockedExpr, AssignmentExpr)
skip_to(BlockedExpr, IfExpr, WhileExpr, BracedBlock)

rule(IfExpr, (If, Expr, Block), SimpleReduceAction('IfExpr', 'a0, Maybe<Located<Tokens::Else>>(), std::move(a1), std::move(a2), nullptr'))
rule(IfExpr, (If, Expr, Block, Else, Block), SimpleReduceAction('IfExpr', 'a0, a3, std::move(a1), std::move(a2), std::move(a4)'))
rule(IfExpr, (If, Expr, Block, Else, IfExpr), SimpleReduceAction('IfExpr', 'a0, a3, std::move(a1), std::move(a2), std::move(a4)'))

rule(WhileExpr, (While, Expr, Block), SimpleReduceAction('WhileExpr', 'std::move(a1), std::move(a2)'))

bin_expr_reduction = lambda cl, opty, op: SimpleReduceAction(cl, f'std::move(a0), Located<{opty}> {{ a1.span, {op} }}, std::move(a2)')

rule(AssignmentExpr, (BinOrExpr, Equal, AssignmentExpr), bin_expr_reduction('AssignmentExpr', 'ASTNS::AssignOperator', 'ASTNS::AssignOperator::EQUAL'))
rule(AssignmentExpr, (BinOrExpr,), SkipReduceAction())
rule(BinOrExpr, (BinOrExpr, DoublePipe, BinAndExpr), bin_expr_reduction('ShortCircuitExpr', 'ASTNS::ShortCircuitOperator', 'ASTNS::ShortCircuitOperator::DOUBLEPIPE'))
rule(BinOrExpr, (BinAndExpr,), SkipReduceAction())
rule(BinAndExpr, (BinAndExpr, DoubleAmper, CompEQExpr), bin_expr_reduction('ShortCircuitExpr', 'ASTNS::ShortCircuitOperator', 'ASTNS::ShortCircuitOperator::DOUBLEAMPER'))
rule(BinAndExpr, (CompEQExpr,), SkipReduceAction())
rule(CompEQExpr, (CompEQExpr, BangEqual, CompLGTExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::BANGEQUAL'))
rule(CompEQExpr, (CompEQExpr, DoubleEqual, CompLGTExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::DOUBLEEQUAL'))
rule(CompEQExpr, (CompLGTExpr,), SkipReduceAction())
rule(CompLGTExpr, (CompLGTExpr, Less, BitXorExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::LESS'))
rule(CompLGTExpr, (CompLGTExpr, Greater, BitXorExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::GREATER'))
rule(CompLGTExpr, (CompLGTExpr, LessEqual, BitXorExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::LESSEQUAL'))
rule(CompLGTExpr, (CompLGTExpr, GreaterEqual, BitXorExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::GREATEREQUAL'))
rule(CompLGTExpr, (BitXorExpr,), SkipReduceAction())
rule(BitXorExpr, (BitXorExpr, Caret, BitOrExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::CARET'))
rule(BitXorExpr, (BitOrExpr,), SkipReduceAction())
rule(BitOrExpr, (BitOrExpr, Pipe, BitAndExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::PIPE'))
rule(BitOrExpr, (BitAndExpr,), SkipReduceAction())
rule(BitAndExpr, (BitAndExpr, Amper, BitShiftExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::AMPER'))
rule(BitAndExpr, (BitShiftExpr,), SkipReduceAction())
rule(BitShiftExpr, (BitShiftExpr, DoubleGreater, AdditionExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::DOUBLEGREATER'))
rule(BitShiftExpr, (BitShiftExpr, DoubleLess, AdditionExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::DOUBLELESS'))
rule(BitShiftExpr, (AdditionExpr,), SkipReduceAction())
rule(AdditionExpr, (AdditionExpr, Plus, MultExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::PLUS'))
rule(AdditionExpr, (AdditionExpr, Minus, MultExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::MINUS'))
rule(AdditionExpr, (MultExpr,), SkipReduceAction())
rule(MultExpr, (MultExpr, Star, UnaryExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::STAR'))
rule(MultExpr, (MultExpr, Slash, UnaryExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::SLASH'))
rule(MultExpr, (MultExpr, Percent, UnaryExpr), bin_expr_reduction('BinaryExpr', 'ASTNS::BinaryOperator', 'ASTNS::BinaryOperator::PERCENT'))
rule(MultExpr, (CastExpr,), SkipReduceAction())
rule(CastExpr, (CastExpr, RightArrow, Type), SimpleReduceAction('CastExpr', 'std::move(a2), std::move(a0)'))
rule(CastExpr, (UnaryExpr,), SkipReduceAction())
rule(UnaryExpr, (Tilde, UnaryExpr), SimpleReduceAction('UnaryExpr', 'Located<ASTNS::UnaryOperator> { a0.span, ASTNS::UnaryOperator::TILDE }, std::move(a1)'))
rule(UnaryExpr, (Minus, UnaryExpr), SimpleReduceAction('UnaryExpr', 'Located<ASTNS::UnaryOperator> { a0.span, ASTNS::UnaryOperator::MINUS }, std::move(a1)'))
rule(UnaryExpr, (Bang, UnaryExpr), SimpleReduceAction('UnaryExpr', 'Located<ASTNS::UnaryOperator> { a0.span, ASTNS::UnaryOperator::BANG }, std::move(a1)'))
rule(UnaryExpr, (Amper, UnaryExpr), SimpleReduceAction('AddrofExpr', 'a0, std::move(a1), false'))
rule(UnaryExpr, (Amper, Mut, UnaryExpr), SimpleReduceAction('AddrofExpr', 'a0, std::move(a2), true'))
rule(UnaryExpr, (Star, UnaryExpr), SimpleReduceAction('DerefExpr', 'a0, std::move(a1)'))

rule(UnaryExpr, (CallExpr,), SkipReduceAction())
rule(UnaryExpr, (FieldAccessExpr,), SkipReduceAction())
rule(UnaryExpr, (MethodCallExpr,), SkipReduceAction())

field_access_reduce = SimpleReduceAction('FieldAccessExpr', 'std::move(a0), a1, a2')
rule(FieldAccessExpr, (FieldAccessExpr, Period, Identifier), field_access_reduce)
rule(FieldAccessExpr, (MethodCallExpr , Period, Identifier), field_access_reduce)
rule(FieldAccessExpr, (CallExpr       , Period, Identifier), field_access_reduce)

method_call_reduce = SimpleReduceAction('MethodCallExpr', 'std::move(a0), a1, a2, a3, std::move(a4->args)')
rule(MethodCallExpr, (FieldAccessExpr, Period, Identifier, OParen, ArgListOpt, CParen), method_call_reduce)
rule(MethodCallExpr, (MethodCallExpr , Period, Identifier, OParen, ArgListOpt, CParen), method_call_reduce)
rule(MethodCallExpr, (CallExpr       , Period, Identifier, OParen, ArgListOpt, CParen), method_call_reduce)

call_reduce = SimpleReduceAction('CallExpr', 'std::move(a0), a1, std::move(a2->args)')
rule(CallExpr, (MethodCallExpr, OParen, ArgListOpt, CParen), call_reduce)
rule(CallExpr, (CallExpr      , OParen, ArgListOpt, CParen), call_reduce)

rule(CallExpr, (PrimaryExpr,), SkipReduceAction())

rule(PrimaryExpr, (BoolLit,), SimpleReduceAction('BoolLit', 'a0'))
rule(PrimaryExpr, (FloatLit,), SimpleReduceAction('FloatLit', 'a0'))
rule(PrimaryExpr, (IntLit,), SimpleReduceAction('IntLit', 'a0'))
rule(PrimaryExpr, (CharLit,), SimpleReduceAction('CharLit', 'a0'))
rule(PrimaryExpr, (StringLit,), SimpleReduceAction('StringLit', 'a0'))
rule(PrimaryExpr, (This,), SimpleReduceAction('ThisExpr', 'a0'))
rule(PrimaryExpr, (OParen, Expr, CParen), SkipReduceAction(1))
rule(PrimaryExpr, (PathExpr,), SkipReduceAction())
rule(PathExpr, (Path,), SimpleReduceAction('PathExpr', 'std::move(a0)'))

rule(Path, (Identifier,), VectorPushOneAction('ASTNS::Path', 'a0', 'Located<Tokens::Identifier>', 'segments'))
rule(Path, (Path, DoubleColon, Identifier), VectorPushReduceAction('a0->segments', 'a2', 'a0'))

# convert grammar {{{1
eof_sym = Terminal('_EOF')

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
            if sym in state.actions:
                print(padf(str(state.actions[sym])), end='')
            elif sym in state.goto:
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
        return f'format("either {{}} or {{}}", {l[0]}, {l[1]})'
    elif len(l) == 0:
        return '"nothing"'
    else:
        return 'format("' + ", ".join('{}' for _ in l[:-1]) + ', or {}", ' +  ', '.join(l) + ')'
# }}}
def gen_loop():
    output = []

    output.append(                                'bool done = false;\n')
    output.append(                                'bool errored = false;\n')
    output.append(                                'Located<TokenData> next_token = p.consume();\n')
    output.append(                                'Located<TokenData> last_token = next_token;\n')
    output.append(                                'std::vector<StackItem> stack;\n')
    output.append(                                'stack.emplace_back(0); // make initial item\n')

    output.append(                                'while (!done) {\n')
    output.append(                                '    switch (stack.back().state) {\n')

    for staten, state in sorted(table.items(), key=lambda x:x[0]):
        output.append(                           f'        case {staten}:\n')
        output.append(                            '            switch (next_token.value.index()) {\n')

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
            if isinstance(ac, ReduceAction) and reduce_only:
                output.append(                    '                default:\n')
                # do not check for lookahead, just reduce to have better performance (kind of)
                # if reduce_only, then all the reduce actions of this state reduce the same rule
                # and according to Wikipedia, just reducing regardless of the lookahead in
                # these states will cause a few "harmless reductions", and errors will just be
                # reported after a few reduces
                # this actually helps with error reporting because if you have "return 2",
                # it will reduce 2 up the chain of expression precedence before reporting the error
            else:
                for term in nts:
                    output.append(               f'                case Tokens::index_of<{term.astt()}>:\n')

            if isinstance(ac, ShiftAction):
                output.append(                   f'                    stack.emplace_back({ac.newstate}, TokenItem {{ next_token }});\n')
                output.append(                   f'                    last_token = next_token;\n')
                output.append(                   f'                    next_token = p.consume();\n')
            elif isinstance(ac, ReduceAction):
                output.append(                    '                    {\n')

                for i, sym in reversed(list(enumerate(ac.rule.expansion))):
                    if isinstance(sym, Terminal):
                        output.append(           f'                        auto _a{i} (pop_as<TokenItem>(stack).tok);\n')
                        output.append(           f'                        Located<{sym.astt()}> a{i} {{ _a{i}.span, Tokens::as<{sym.astt()}>(_a{i}.value) }};\n')
                    elif isinstance(sym, NonTerminal):
                        output.append(           f'                        auto a{i} (pop_as<ASTItem<{sym.reduces_to}>>(stack).ast);\n')

                if len(ac.rule.expansion) > 0:
                    output.append(                '                        Maybe<Location const> start =\n')
                    for i in range(ac.rule.loc_start, len(ac.rule.expansion)):
                        if isinstance(ac.rule.expansion[i], Terminal):
                            output.append(       f'                            Maybe<Location const>(a{i}.span.start);\n')
                            break
                        else:
                            if i == len(ac.rule.expansion) - 1:
                                output.append(   f'                            a{i} && a{i}->span().has() ? Maybe<Location const>(a{i}->span().get().start) : Maybe<Location const>();\n')
                            else:
                                output.append(   f'                            a{i} && a{i}->span().has() ? Maybe<Location const>(a{i}->span().get().start) :\n')

                    output.append(                '                        Maybe<Location const> end =\n')
                    for i in range(ac.rule.loc_end, -1, -1):
                        if isinstance(ac.rule.expansion[i], Terminal):
                            output.append(       f'                            Maybe<Location const>(a{i}.span.end);\n')
                            break
                        else:
                            if i == 0:
                                output.append(   f'                            a{i} && a{i}->span().has() ? Maybe<Location const>(a{i}->span().get().end) : Maybe<Location const>();\n')
                            else:
                                output.append(   f'                            a{i} && a{i}->span().has() ? Maybe<Location const>(a{i}->span().get().end) :\n')
                else:
                    output.append(                '                        Maybe<Location const> start, end;\n')
                output.append(                    '                        Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();\n')

                reduce_code, pushitem = ac.rule.reduce_action.generate()
                output.append(reduce_code)
                output.append(                   f'                        {ac.rule.symbol.reduces_to} pushitem = {pushitem};\n')
                output.append(                   f'                        stack.emplace_back(get_goto(NonTerminal::_{ac.rule.symbol.id}, stack.back().state), ASTItem<decltype(pushitem)>{{ std::move(pushitem), NonTerminal::_{ac.rule.symbol.id} }});\n')
                output.append(                    '                    }\n')


            elif isinstance(ac, AcceptAction):
                output.append(                    '                    done = true;\n')
            else:
                raise Exception('invalid action type')

            output.append(                        '                    break;\n')

        if not reduce_only:
            def stc(s):
                if isinstance(s, NonTerminal):
                    return f'"{s.name}"'
                else:
                    return f'{s.astt()}::stringify()'

            futuress = [f'format("expected {{}} for {{}}", {format_list([stc(p) for p in future])}, {stc(nt)})' for nt, future in state.futures.items()]
            terminatess = [f'format("expected {{}} to terminate {{}}", {format_list([stc(p) for p in future])}, {stc(nt)})' for nt, future in state.terminates.items()]

            output.append(                        '                default:\n')
            output.append(                       f'                    ERR_UNRECOVERABLE_INVALID_SYNTAX(next_token.span, Tokens::stringify_type(next_token.value), last_token.span, {{ {", ".join(futuress + terminatess)} }} );\n')
            output.append(                        '                    errored = done = true;\n')
        output.append(                            '            }\n')
        output.append(                            '            break;\n')

    output.append(                                '        default:\n')
    output.append(                                '            report_abort_noh(format("parser reached invalid state {}", stack.back().state));\n')

    output.append(                                '    }\n')
    output.append(                                '}\n')

    return ''.join(output)
# generate goto code {{{2
def gen_goto():
    output = []

    output.append(                                'size_t get_goto(NonTerminal nterm, size_t state) {\n')
    output.append(                                '    switch (nterm) {\n')
    for nonterm in symbols:
        if isinstance(nonterm, Terminal):
            continue

        output.append(                           f'        case NonTerminal::_{nonterm.id}:\n')
        output.append(                            '            switch (state) {\n')

        returns = {}
        for staten, state in table.items():
            if nonterm in state.goto:
                if state.goto[nonterm] in returns: # there is already a state in which this goto is returned (squishing rows together)
                    returns[state.goto[nonterm]].append(staten)
                else:
                    returns[state.goto[nonterm]] = [staten]

        for retval, states in returns.items():
            output.append(                       f'                {" ".join(f"case {state}:" for state in states)}')
            output.append(                       f'\n                    return {retval};\n')

        output.append(                            '                default: report_abort_noh("get invalid goto");\n')

        output.append(                            '            }\n')

    output.append(                                '    }\n')
    output.append(                                '}\n')

    return ''.join(output)

# generate nonterminal enum {{{2
def gen_non_term_enum():
    output = []
    for symbol in symbols:
        if isinstance(symbol, NonTerminal):
            output.append(f'_{symbol.id}, ')
    output.append('\n')
    return ''.join(output)
# generate nonterminal types {{{2
def gen_non_term_types():
    things = sorted(list(set([f'ASTItem<{x.reduces_to}>' for x in symbols if isinstance(x, NonTerminal)])))
    return ',\n'.join(things)
# entry {{{1
if __name__ == '__main__':
    print_parse_table(False)
