#include "parse/parser.h"
#include "parsestack.h" // in a private header file
#include <vector>
#include <iostream>
#include "utils/format.h"
#include "utils/assert.h"
#include "utils/maybe.h"
#include "message/errmsgs.h"

// get goto {{{
// GETGOTO START
size_t get_goto(NonTerminal nterm, size_t state) {
    switch (nterm) {
        case NonTerminal::_48:
            switch (state) {
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_0:
            switch (state) {
                case 0:
                    return 1;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_49:
            switch (state) {
                case 49:
                    return 63;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_21:
            switch (state) {
                case 19:
                    return 29;
                case 49:
                    return 64;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_51:
            switch (state) {
                case 19:
                    return 28;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_50:
            switch (state) {
                case 19:
                    return 27;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_52:
            switch (state) {
                case 232:
                    return 242;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_20:
            switch (state) {
                case 175: case 178: case 233: case 234: case 236:
                    return 218;
                case 232:
                    return 243;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_54:
            switch (state) {
                case 175: case 178: case 233: case 234: case 236:
                    return 217;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_53:
            switch (state) {
                case 175: case 178: case 233: case 234: case 236:
                    return 216;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_55:
            switch (state) {
                case 185:
                    return 226;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_10:
            switch (state) {
                case 87:
                    return 136;
                case 185:
                    return 227;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_57:
            switch (state) {
                case 87:
                    return 135;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_56:
            switch (state) {
                case 87:
                    return 134;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_58:
            switch (state) {
                case 82:
                    return 132;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_6:
            switch (state) {
                case 76: case 79: case 81: case 131:
                    return 83;
                case 82:
                    return 133;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_59:
            switch (state) {
                case 76: case 79: case 81: case 131:
                    return 82;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_60:
            switch (state) {
                case 2:
                    return 8;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_1:
            switch (state) {
                case 0:
                    return 3;
                case 2:
                    return 9;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_61:
            switch (state) {
                case 0:
                    return 2;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_62:
            switch (state) {
                case 42:
                    return 59;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_5:
            switch (state) {
                case 21: case 41: case 45: case 58:
                    return 43;
                case 42:
                    return 60;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_63:
            switch (state) {
                case 21: case 41: case 45: case 58:
                    return 42;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_64:
            switch (state) {
                case 19:
                    return 26;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_65:
            switch (state) {
                case 175:
                    return 215;
                case 178:
                    return 222;
                case 233:
                    return 244;
                case 234:
                    return 245;
                case 236:
                    return 246;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_66:
            switch (state) {
                case 76:
                    return 80;
                case 79:
                    return 128;
                case 81:
                    return 130;
                case 131:
                    return 183;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_67:
            switch (state) {
                case 21:
                    return 40;
                case 41:
                    return 57;
                case 45:
                    return 61;
                case 58:
                    return 69;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_68:
            switch (state) {
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_24:
            switch (state) {
                case 90:
                    return 143;
                case 96:
                    return 149;
                case 97:
                    return 150;
                case 124:
                    return 180;
                case 175: case 178: case 232: case 233: case 234: case 236:
                    return 219;
                case 228:
                    return 238;
                case 239:
                    return 247;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_69:
            switch (state) {
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_7:
            switch (state) {
                case 76: case 79: case 81: case 82: case 131:
                    return 84;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_70:
            switch (state) {
                case 20:
                    return 36;
                case 71:
                    return 78;
                case 89:
                    return 141;
                case 140:
                    return 188;
                case 142:
                    return 189;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_11:
            switch (state) {
                case 20: case 71: case 89: case 140: case 142:
                    return 37;
                case 62:
                    return 72;
                case 88:
                    return 139;
                case 90:
                    return 144;
                case 134:
                    return 184;
                case 143:
                    return 190;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_71:
            switch (state) {
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_15:
            switch (state) {
                case 32:
                    return 50;
                case 48:
                    return 62;
                case 52:
                    return 66;
                case 137:
                    return 186;
                case 187:
                    return 229;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_2:
            switch (state) {
                case 0: case 2:
                    return 4;
                case 21: case 41: case 42: case 45: case 58:
                    return 44;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_3:
            switch (state) {
                case 0: case 2:
                    return 5;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_12:
            switch (state) {
                case 62:
                    return 71;
                case 149:
                    return 193;
                case 150:
                    return 195;
                case 230:
                    return 240;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_16:
            switch (state) {
                case 7:
                    return 11;
                case 16:
                    return 24;
                case 25:
                    return 47;
                case 51:
                    return 65;
                case 168:
                    return 213;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_4:
            switch (state) {
                case 11:
                    return 20;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_8:
            switch (state) {
                case 76: case 79: case 81: case 82: case 131:
                    return 85;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_9:
            switch (state) {
                case 76: case 79: case 81: case 82: case 131:
                    return 86;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_26:
            switch (state) {
                case 76: case 79: case 81: case 82: case 131:
                    return 88;
                case 90: case 96: case 97: case 124: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 146;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_25:
            switch (state) {
                case 76: case 79: case 81: case 82: case 131:
                    return 89;
                case 90: case 96: case 97: case 124: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 145;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_13:
            switch (state) {
                case 62: case 149: case 150: case 230:
                    return 73;
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 94;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_14:
            switch (state) {
                case 62: case 149: case 150: case 230:
                    return 74;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_19:
            switch (state) {
                case 7: case 16: case 25: case 51: case 168:
                    return 12;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_17:
            switch (state) {
                case 7: case 16: case 25: case 51: case 168:
                    return 13;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_18:
            switch (state) {
                case 7: case 16: case 25: case 51: case 168:
                    return 14;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_47:
            switch (state) {
                case 7: case 16: case 25: case 51: case 168:
                    return 15;
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 126;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_23:
            switch (state) {
                case 19: case 49:
                    return 30;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_22:
            switch (state) {
                case 19: case 49:
                    return 31;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_29:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 91;
                case 147:
                    return 191;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_27:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 92;
                case 230:
                    return 241;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_28:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 93;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_30:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 95;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_31:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 98;
                case 148:
                    return 192;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_32:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 99;
                case 151:
                    return 196;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_33:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 100;
                case 152:
                    return 197;
                case 153:
                    return 198;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_34:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 101;
                case 154:
                    return 199;
                case 155:
                    return 200;
                case 156:
                    return 201;
                case 157:
                    return 202;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_35:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 102;
                case 158:
                    return 203;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_36:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 103;
                case 159:
                    return 204;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_37:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 104;
                case 160:
                    return 205;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_38:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 105;
                case 161:
                    return 206;
                case 162:
                    return 207;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_39:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 106;
                case 163:
                    return 208;
                case 164:
                    return 209;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_41:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 108;
                case 109:
                    return 169;
                case 110:
                    return 170;
                case 111:
                    return 171;
                case 112:
                    return 172;
                case 113:
                    return 174;
                case 165:
                    return 210;
                case 166:
                    return 211;
                case 167:
                    return 212;
                case 173:
                    return 214;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_40:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 107;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_42:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 114;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_43:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 115;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_44:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 116;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_45:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 117;
                default: report_abort_noh("get invalid goto");
            }
        case NonTerminal::_46:
            switch (state) {
                case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 228: case 232: case 233: case 234: case 236: case 239:
                    return 125;
                default: report_abort_noh("get invalid goto");
            }
    }
}
// GETGOTO END
// }}}

static void shift(Parser &p, Token &last, Token &lookahead, std::vector<stackitem> &stack, int &steps, int const &newst) {
    last = lookahead;
    stack.emplace_back(newst, last);
    lookahead = p.consume();
    ++steps;
}

static Token pop_t(std::vector<stackitem> &stack) {
    stackitem si = std::move(stack.back());

    stack.pop_back();
    return std::get<tokenitem>(si.item).tok;
}

template <typename A>
static std::unique_ptr<A> pop_a(std::vector<stackitem> &stack) {
    stackitem si = std::move(stack.back());
    stack.pop_back();

    astitem &as_astitem = std::get<astitem>(si.item);
    std::unique_ptr<A> converted = std::unique_ptr<A>(static_cast<A*>(as_astitem.ast.release()));

    return converted;
}

static void error(bool &done, bool &errored, errorstate const &ers, std::vector<std::string> const &expectations) {
    errored = true;

    if (!error_recovery(ers, expectations))
        done = true;
}

bool _parse(Parser &p, std::vector<stackitem> &stack, bool istrial, std::unique_ptr<ASTNS::CUB> &out, Token const &_lookahead) {
    // PARSERLOOP START {{{
    bool done = false;
    bool errored = false;
    int steps = 0;
    Token lookahead (_lookahead); // for when you need to inject a new token
    Token lasttok = lookahead;
    while (!done) {
        if (istrial && steps > 5)
            return true;
        switch (stack.back().state) {
            case 0:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                    case Token::index_of<Tokens::Impl>:
                        shift(p, lasttok, lookahead, stack, steps, 7); break;
                    default: {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::CU> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_0, stack.back().state), std::move(pushitem), NonTerminal::_0);
                        }
                        break;
                }
                break;
            case 1:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::_EOF>:
                            done = true;
                        break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} to terminate {}", Tokens::_EOF::stringify(), "augment")  });
                }
                break;
            case 2:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                    case Token::index_of<Tokens::Impl>:
                        shift(p, lasttok, lookahead, stack, steps, 7); break;
                    default: {
                            auto a0 (pop_a<ASTNS::DeclList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::CU> push (std::make_unique<ASTNS::CU>(p.sourcefile, span, std::move(a0->decls)));
                            std::unique_ptr<ASTNS::CU> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_0, stack.back().state), std::move(pushitem), NonTerminal::_0);
                        }
                        break;
                }
                break;
            case 3:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Decl>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::DeclList> push (std::make_unique<ASTNS::DeclList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Decl>> {}));

            push->decls.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::DeclList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_61, stack.back().state), std::move(pushitem), NonTerminal::_61);
                        }
                        break;
                }
                break;
            case 4:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::FunctionDecl>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_1, stack.back().state), std::move(pushitem), NonTerminal::_1);
                        }
                        break;
                }
                break;
            case 5:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Decl>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_1, stack.back().state), std::move(pushitem), NonTerminal::_1);
                        }
                        break;
                }
                break;
            case 6:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 10); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "function declaration")  });
                }
                break;
            case 7:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 16); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 17); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "type specifier", "implementation")  });
                }
                break;
            case 8:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Decl>(stack));
                            auto a0 (pop_a<ASTNS::DeclList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->decls.push_back(std::move(a1));
                            std::unique_ptr<ASTNS::DeclList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_61, stack.back().state), std::move(pushitem), NonTerminal::_61);
                        }
                        break;
                }
                break;
            case 9:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Decl>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_60, stack.back().state), std::move(pushitem), NonTerminal::_60);
                        }
                        break;
                }
                break;
            case 10:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 19); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::OParen::stringify(), "function declaration")  });
                }
                break;
            case 11:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 22); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 21); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "implementation body", "implementation")  });
                }
                break;
            case 12:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::PathType>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_16, stack.back().state), std::move(pushitem), NonTerminal::_16);
                        }
                        break;
                }
                break;
            case 13:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::PointerType>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_16, stack.back().state), std::move(pushitem), NonTerminal::_16);
                        }
                        break;
                }
                break;
            case 14:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ThisType>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_16, stack.back().state), std::move(pushitem), NonTerminal::_16);
                        }
                        break;
                }
                break;
            case 15:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Path>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PathType> push (std::make_unique<ASTNS::PathType>(p.sourcefile, span, std::move(a0)));
                            std::unique_ptr<ASTNS::PathType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_19, stack.back().state), std::move(pushitem), NonTerminal::_19);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleColon>:
                        shift(p, lasttok, lookahead, stack, steps, 23); break;
                }
                break;
            case 16:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 25); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 16); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 17); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "type specifier", Tokens::Mut::stringify()), "pointer type")  });
                }
                break;
            case 17:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ThisType> push (std::make_unique<ASTNS::ThisType>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::ThisType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_18, stack.back().state), std::move(pushitem), NonTerminal::_18);
                        }
                        break;
                }
                break;
            case 18:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Path> push (std::make_unique<ASTNS::Path>(p.sourcefile, span, std::vector<Token> {}));

            push->segments.push_back(a0);
                            std::unique_ptr<ASTNS::Path> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_47, stack.back().state), std::move(pushitem), NonTerminal::_47);
                        }
                        break;
                }
                break;
            case 19:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ParamList> push (std::make_unique<ASTNS::ParamList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ParamB>> {}));
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_64, stack.back().state), std::move(pushitem), NonTerminal::_64);
                        }
                        break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 32); break;
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 33); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 35); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 34); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional function parameter list", "function declaration")  });
                }
                break;
            case 20:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                    case Token::index_of<Tokens::Bang>:
                    case Token::index_of<Tokens::BoolLit>:
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::CharLit>:
                    case Token::index_of<Tokens::Dedent>:
                    case Token::index_of<Tokens::FloatLit>:
                    case Token::index_of<Tokens::Fun>:
                    case Token::index_of<Tokens::Identifier>:
                    case Token::index_of<Tokens::If>:
                    case Token::index_of<Tokens::Impl>:
                    case Token::index_of<Tokens::IntLit>:
                    case Token::index_of<Tokens::Minus>:
                    case Token::index_of<Tokens::OBrace>:
                    case Token::index_of<Tokens::OParen>:
                    case Token::index_of<Tokens::Return>:
                    case Token::index_of<Tokens::Star>:
                    case Token::index_of<Tokens::StringLit>:
                    case Token::index_of<Tokens::This>:
                    case Token::index_of<Tokens::Tilde>:
                    case Token::index_of<Tokens::Var>:
                    case Token::index_of<Tokens::While>:
                    case Token::index_of<Tokens::_EOF>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_70, stack.back().state), std::move(pushitem), NonTerminal::_70);
                        }
                        break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional line ending", "implementation")  });
                }
                break;
            case 21:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), std::move(pushitem), NonTerminal::_67);
                        }
                        break;
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 41); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "optional implementation member list", Tokens::Newline::stringify()), "implementation body")  });
                }
                break;
            case 22:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Indent>:
                        shift(p, lasttok, lookahead, stack, steps, 45); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Indent::stringify(), "implementation body")  });
                }
                break;
            case 23:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 46); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "symbol path")  });
                }
                break;
            case 24:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Type>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PointerType> push (std::make_unique<ASTNS::PointerType>(p.sourcefile, span, false, std::move(a1)));
                            std::unique_ptr<ASTNS::PointerType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_17, stack.back().state), std::move(pushitem), NonTerminal::_17);
                        }
                        break;
                }
                break;
            case 25:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 16); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 17); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "type specifier", "pointer type")  });
                }
                break;
            case 26:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 48); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "function declaration")  });
                }
                break;
            case 27:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ParamList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_64, stack.back().state), std::move(pushitem), NonTerminal::_64);
                        }
                        break;
                }
                break;
            case 28:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ParamList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_50, stack.back().state), std::move(pushitem), NonTerminal::_50);
                        }
                        break;
                    case Token::index_of<Tokens::Comma>:
                        shift(p, lasttok, lookahead, stack, steps, 49); break;
                }
                break;
            case 29:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ParamB>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ParamList> push (std::make_unique<ASTNS::ParamList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ParamB>> {}));

            push->params.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_51, stack.back().state), std::move(pushitem), NonTerminal::_51);
                        }
                        break;
                }
                break;
            case 30:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Param>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_21, stack.back().state), std::move(pushitem), NonTerminal::_21);
                        }
                        break;
                }
                break;
            case 31:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ThisParam>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_21, stack.back().state), std::move(pushitem), NonTerminal::_21);
                        }
                        break;
                }
                break;
            case 32:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Colon>:
                        shift(p, lasttok, lookahead, stack, steps, 51); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "required type annotation", "function parameter")  });
                }
                break;
            case 33:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 52); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "function parameter")  });
                }
                break;
            case 34:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(p.sourcefile, span, false, false));
                            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_22, stack.back().state), std::move(pushitem), NonTerminal::_22);
                        }
                        break;
                }
                break;
            case 35:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 54); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 53); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", Tokens::This::stringify(), Tokens::Mut::stringify()), "'this' function parameter")  });
                }
                break;
            case 36:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_a<ASTNS::PureLocation>(stack));
                            auto a2 (pop_a<ASTNS::ImplMemberList>(stack));
                            auto a1 (pop_a<ASTNS::Type>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ImplDecl> push (std::make_unique<ASTNS::ImplDecl>(p.sourcefile, span, std::move(a1), std::move(a2->members)));
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_3, stack.back().state), std::move(pushitem), NonTerminal::_3);
                        }
                        break;
                }
                break;
            case 37:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::PureLocation>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_70, stack.back().state), std::move(pushitem), NonTerminal::_70);
                        }
                        break;
                }
                break;
            case 38:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), std::move(pushitem), NonTerminal::_11);
                        }
                        break;
                }
                break;
            case 39:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), std::move(pushitem), NonTerminal::_11);
                        }
                        break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 55); break;
                }
                break;
            case 40:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 56); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CBrace::stringify(), "implementation body")  });
                }
                break;
            case 41:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), std::move(pushitem), NonTerminal::_67);
                        }
                        break;
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                    case Token::index_of<Tokens::Indent>:
                        shift(p, lasttok, lookahead, stack, steps, 58); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "optional implementation member list", Tokens::Indent::stringify()), "implementation body")  });
                }
                break;
            case 42:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ImplMemberList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), std::move(pushitem), NonTerminal::_67);
                        }
                        break;
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                }
                break;
            case 43:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ImplMember>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));

            push->members.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_63, stack.back().state), std::move(pushitem), NonTerminal::_63);
                        }
                        break;
                }
                break;
            case 44:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::FunctionDecl>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::FunctionImplMember> push (std::make_unique<ASTNS::FunctionImplMember>(p.sourcefile, span, std::move(a0)));
                            std::unique_ptr<ASTNS::ImplMember> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_5, stack.back().state), std::move(pushitem), NonTerminal::_5);
                        }
                        break;
                }
                break;
            case 45:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), std::move(pushitem), NonTerminal::_67);
                        }
                        break;
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional implementation member list", "implementation body")  });
                }
                break;
            case 46:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Path>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->segments.push_back(a2);
                            std::unique_ptr<ASTNS::Path> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_47, stack.back().state), std::move(pushitem), NonTerminal::_47);
                        }
                        break;
                }
                break;
            case 47:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Type>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PointerType> push (std::make_unique<ASTNS::PointerType>(p.sourcefile, span, true, std::move(a2)));
                            std::unique_ptr<ASTNS::PointerType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_17, stack.back().state), std::move(pushitem), NonTerminal::_17);
                        }
                        break;
                }
                break;
            case 48:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Colon>:
                        shift(p, lasttok, lookahead, stack, steps, 51); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "required type annotation", "function declaration")  });
                }
                break;
            case 49:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::ParamList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_50, stack.back().state), std::move(pushitem), NonTerminal::_50);
                        }
                        break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 32); break;
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 33); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 35); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 34); break;
                }
                break;
            case 50:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Type>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Param> push (std::make_unique<ASTNS::Param>(p.sourcefile, span, std::move(a1), a0, false));
                            std::unique_ptr<ASTNS::Param> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_23, stack.back().state), std::move(pushitem), NonTerminal::_23);
                        }
                        break;
                }
                break;
            case 51:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 16); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 17); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "type specifier", "required type annotation")  });
                }
                break;
            case 52:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Colon>:
                        shift(p, lasttok, lookahead, stack, steps, 51); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "required type annotation", "function parameter")  });
                }
                break;
            case 53:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(p.sourcefile, span, true, false));
                            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_22, stack.back().state), std::move(pushitem), NonTerminal::_22);
                        }
                        break;
                }
                break;
            case 54:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 67); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::This::stringify(), "'this' function parameter")  });
                }
                break;
            case 55:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    WARN_EXTRA_SEMI(a0);std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), std::move(pushitem), NonTerminal::_11);
                        }
                        break;
                }
                break;
            case 56:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_a<ASTNS::ImplMemberList>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), std::move(pushitem), NonTerminal::_4);
                        }
                        break;
                }
                break;
            case 57:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 68); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CBrace::stringify(), "implementation body")  });
                }
                break;
            case 58:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), std::move(pushitem), NonTerminal::_67);
                        }
                        break;
                    case Token::index_of<Tokens::Fun>:
                        shift(p, lasttok, lookahead, stack, steps, 6); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional implementation member list", "implementation body")  });
                }
                break;
            case 59:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::ImplMember>(stack));
                            auto a0 (pop_a<ASTNS::ImplMemberList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->members.push_back(std::move(a1));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_63, stack.back().state), std::move(pushitem), NonTerminal::_63);
                        }
                        break;
                }
                break;
            case 60:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ImplMember>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMember> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_62, stack.back().state), std::move(pushitem), NonTerminal::_62);
                        }
                        break;
                }
                break;
            case 61:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Dedent>:
                        shift(p, lasttok, lookahead, stack, steps, 70); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Dedent::stringify(), "implementation body")  });
                }
                break;
            case 62:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 75); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "code block", "line ending"), "function declaration")  });
                }
                break;
            case 63:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::ParamB>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::ParamList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->params.push_back(std::move(a2));
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_51, stack.back().state), std::move(pushitem), NonTerminal::_51);
                        }
                        break;
                }
                break;
            case 64:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ParamB>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_49, stack.back().state), std::move(pushitem), NonTerminal::_49);
                        }
                        break;
                }
                break;
            case 65:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Type>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_15, stack.back().state), std::move(pushitem), NonTerminal::_15);
                        }
                        break;
                }
                break;
            case 66:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Type>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Param> push (std::make_unique<ASTNS::Param>(p.sourcefile, span, std::move(a2), a1, true));
                            std::unique_ptr<ASTNS::Param> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_23, stack.back().state), std::move(pushitem), NonTerminal::_23);
                        }
                        break;
                }
                break;
            case 67:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(p.sourcefile, span, true, true));
                            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_22, stack.back().state), std::move(pushitem), NonTerminal::_22);
                        }
                        break;
                }
                break;
            case 68:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::ImplMemberList>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    WARN_BLOCK_NO_INDENT(a0, a3);                        std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a2);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), std::move(pushitem), NonTerminal::_4);
                        }
                        break;
                }
                break;
            case 69:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Dedent>:
                        shift(p, lasttok, lookahead, stack, steps, 77); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Dedent::stringify(), "implementation body")  });
                }
                break;
            case 70:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::ImplMemberList>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a2);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), std::move(pushitem), NonTerminal::_4);
                        }
                        break;
                }
                break;
            case 71:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                    case Token::index_of<Tokens::Bang>:
                    case Token::index_of<Tokens::BoolLit>:
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::CharLit>:
                    case Token::index_of<Tokens::Dedent>:
                    case Token::index_of<Tokens::FloatLit>:
                    case Token::index_of<Tokens::Fun>:
                    case Token::index_of<Tokens::Identifier>:
                    case Token::index_of<Tokens::If>:
                    case Token::index_of<Tokens::Impl>:
                    case Token::index_of<Tokens::IntLit>:
                    case Token::index_of<Tokens::Minus>:
                    case Token::index_of<Tokens::OBrace>:
                    case Token::index_of<Tokens::OParen>:
                    case Token::index_of<Tokens::Return>:
                    case Token::index_of<Tokens::Star>:
                    case Token::index_of<Tokens::StringLit>:
                    case Token::index_of<Tokens::This>:
                    case Token::index_of<Tokens::Tilde>:
                    case Token::index_of<Tokens::Var>:
                    case Token::index_of<Tokens::While>:
                    case Token::index_of<Tokens::_EOF>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_70, stack.back().state), std::move(pushitem), NonTerminal::_70);
                        }
                        break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional line ending", "function declaration")  });
                }
                break;
            case 72:
                switch (lookahead.index()) {
                    default: {
                            auto a6 (pop_a<ASTNS::PureLocation>(stack));
                            auto a5 (pop_a<ASTNS::Type>(stack));
                            auto a4 (pop_t(stack));
                            auto a3 (pop_a<ASTNS::ParamList>(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a6 && a6->span().has() ? Maybe<Location const>(a6->span().get().end) :
                                a5 && a5->span().has() ? Maybe<Location const>(a5->span().get().end) :
                                Maybe<Location const>(a4.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::FunctionDecl> push (std::make_unique<ASTNS::FunctionDecl>(p.sourcefile, span, std::move(a5), a1, std::move(a3->params), nullptr));
                            std::unique_ptr<ASTNS::FunctionDecl> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_2, stack.back().state), std::move(pushitem), NonTerminal::_2);
                        }
                        break;
                }
                break;
            case 73:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Block>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_12, stack.back().state), std::move(pushitem), NonTerminal::_12);
                        }
                        break;
                }
                break;
            case 74:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Block>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_12, stack.back().state), std::move(pushitem), NonTerminal::_12);
                        }
                        break;
                }
                break;
            case 75:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), std::move(pushitem), NonTerminal::_11);
                        }
                        break;
                    case Token::index_of<Tokens::Indent>:
                        shift(p, lasttok, lookahead, stack, steps, 79); break;
                }
                break;
            case 76:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), std::move(pushitem), NonTerminal::_66);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 81); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Return>:
                        shift(p, lasttok, lookahead, stack, steps, 90); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::Var>:
                        shift(p, lasttok, lookahead, stack, steps, 87); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "optional statement list", Tokens::Newline::stringify()), "braced code block")  });
                }
                break;
            case 77:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 127); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CBrace::stringify(), "implementation body")  });
                }
                break;
            case 78:
                switch (lookahead.index()) {
                    default: {
                            auto a7 (pop_a<ASTNS::PureLocation>(stack));
                            auto a6 (pop_a<ASTNS::Block>(stack));
                            auto a5 (pop_a<ASTNS::Type>(stack));
                            auto a4 (pop_t(stack));
                            auto a3 (pop_a<ASTNS::ParamList>(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a5 && a5->span().has() ? Maybe<Location const>(a5->span().get().end) :
                                Maybe<Location const>(a4.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::FunctionDecl> push (std::make_unique<ASTNS::FunctionDecl>(p.sourcefile, span, std::move(a5), a1, std::move(a3->params), std::move(a6)));
                            std::unique_ptr<ASTNS::FunctionDecl> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_2, stack.back().state), std::move(pushitem), NonTerminal::_2);
                        }
                        break;
                }
                break;
            case 79:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), std::move(pushitem), NonTerminal::_66);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Return>:
                        shift(p, lasttok, lookahead, stack, steps, 90); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::Var>:
                        shift(p, lasttok, lookahead, stack, steps, 87); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional statement list", "indented code block")  });
                }
                break;
            case 80:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 129); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CBrace::stringify(), "braced code block")  });
                }
                break;
            case 81:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), std::move(pushitem), NonTerminal::_66);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::Indent>:
                        shift(p, lasttok, lookahead, stack, steps, 131); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Return>:
                        shift(p, lasttok, lookahead, stack, steps, 90); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::Var>:
                        shift(p, lasttok, lookahead, stack, steps, 87); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "optional statement list", Tokens::Indent::stringify()), "braced code block")  });
                }
                break;
            case 82:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    default: {
                            auto a0 (pop_a<ASTNS::StmtList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), std::move(pushitem), NonTerminal::_66);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Return>:
                        shift(p, lasttok, lookahead, stack, steps, 90); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::Var>:
                        shift(p, lasttok, lookahead, stack, steps, 87); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                }
                break;
            case 83:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Stmt>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));

            push->stmts.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_59, stack.back().state), std::move(pushitem), NonTerminal::_59);
                        }
                        break;
                }
                break;
            case 84:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::VarStmt>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_6, stack.back().state), std::move(pushitem), NonTerminal::_6);
                        }
                        break;
                }
                break;
            case 85:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ExprStmt>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_6, stack.back().state), std::move(pushitem), NonTerminal::_6);
                        }
                        break;
                }
                break;
            case 86:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::RetStmt>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_6, stack.back().state), std::move(pushitem), NonTerminal::_6);
                        }
                        break;
                }
                break;
            case 87:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 137); break;
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 138); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "variable binding list", "variable declaration")  });
                }
                break;
            case 88:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Dollar>:
                        shift(p, lasttok, lookahead, stack, steps, 140); break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "line ending", Tokens::Dollar::stringify()), "expression statement")  });
                }
                break;
            case 89:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                    case Token::index_of<Tokens::Bang>:
                    case Token::index_of<Tokens::BoolLit>:
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::CharLit>:
                    case Token::index_of<Tokens::Dedent>:
                    case Token::index_of<Tokens::FloatLit>:
                    case Token::index_of<Tokens::Fun>:
                    case Token::index_of<Tokens::Identifier>:
                    case Token::index_of<Tokens::If>:
                    case Token::index_of<Tokens::Impl>:
                    case Token::index_of<Tokens::IntLit>:
                    case Token::index_of<Tokens::Minus>:
                    case Token::index_of<Tokens::OBrace>:
                    case Token::index_of<Tokens::OParen>:
                    case Token::index_of<Tokens::Return>:
                    case Token::index_of<Tokens::Star>:
                    case Token::index_of<Tokens::StringLit>:
                    case Token::index_of<Tokens::This>:
                    case Token::index_of<Tokens::Tilde>:
                    case Token::index_of<Tokens::Var>:
                    case Token::index_of<Tokens::While>:
                    case Token::index_of<Tokens::_EOF>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_70, stack.back().state), std::move(pushitem), NonTerminal::_70);
                        }
                        break;
                    case Token::index_of<Tokens::Dollar>:
                        shift(p, lasttok, lookahead, stack, steps, 142); break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "optional line ending", Tokens::Dollar::stringify()), "expression statement")  });
                }
                break;
            case 90:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "expression", "line ending"), "return statement")  });
                }
                break;
            case 91:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_26, stack.back().state), std::move(pushitem), NonTerminal::_26);
                        }
                        break;
                }
                break;
            case 92:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::IfExpr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_25, stack.back().state), std::move(pushitem), NonTerminal::_25);
                        }
                        break;
                }
                break;
            case 93:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::WhileExpr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_25, stack.back().state), std::move(pushitem), NonTerminal::_25);
                        }
                        break;
                }
                break;
            case 94:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Block>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_25, stack.back().state), std::move(pushitem), NonTerminal::_25);
                        }
                        break;
                }
                break;
            case 95:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_29, stack.back().state), std::move(pushitem), NonTerminal::_29);
                        }
                        break;
                    case Token::index_of<Tokens::DoublePipe>:
                        shift(p, lasttok, lookahead, stack, steps, 148); break;
                    case Token::index_of<Tokens::Equal>:
                        shift(p, lasttok, lookahead, stack, steps, 147); break;
                }
                break;
            case 96:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "expression", "if expression")  });
                }
                break;
            case 97:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "expression", "while loop expression")  });
                }
                break;
            case 98:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_30, stack.back().state), std::move(pushitem), NonTerminal::_30);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleAmper>:
                        shift(p, lasttok, lookahead, stack, steps, 151); break;
                }
                break;
            case 99:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::BangEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 152); break;
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_31, stack.back().state), std::move(pushitem), NonTerminal::_31);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 153); break;
                }
                break;
            case 100:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_32, stack.back().state), std::move(pushitem), NonTerminal::_32);
                        }
                        break;
                    case Token::index_of<Tokens::Greater>:
                        shift(p, lasttok, lookahead, stack, steps, 155); break;
                    case Token::index_of<Tokens::GreaterEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 157); break;
                    case Token::index_of<Tokens::Less>:
                        shift(p, lasttok, lookahead, stack, steps, 154); break;
                    case Token::index_of<Tokens::LessEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 156); break;
                }
                break;
            case 101:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), std::move(pushitem), NonTerminal::_33);
                        }
                        break;
                    case Token::index_of<Tokens::Caret>:
                        shift(p, lasttok, lookahead, stack, steps, 158); break;
                }
                break;
            case 102:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), std::move(pushitem), NonTerminal::_34);
                        }
                        break;
                    case Token::index_of<Tokens::Pipe>:
                        shift(p, lasttok, lookahead, stack, steps, 159); break;
                }
                break;
            case 103:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 160); break;
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_35, stack.back().state), std::move(pushitem), NonTerminal::_35);
                        }
                        break;
                }
                break;
            case 104:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_36, stack.back().state), std::move(pushitem), NonTerminal::_36);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleGreater>:
                        shift(p, lasttok, lookahead, stack, steps, 161); break;
                    case Token::index_of<Tokens::DoubleLess>:
                        shift(p, lasttok, lookahead, stack, steps, 162); break;
                }
                break;
            case 105:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_37, stack.back().state), std::move(pushitem), NonTerminal::_37);
                        }
                        break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 164); break;
                    case Token::index_of<Tokens::Plus>:
                        shift(p, lasttok, lookahead, stack, steps, 163); break;
                }
                break;
            case 106:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_38, stack.back().state), std::move(pushitem), NonTerminal::_38);
                        }
                        break;
                    case Token::index_of<Tokens::Percent>:
                        shift(p, lasttok, lookahead, stack, steps, 167); break;
                    case Token::index_of<Tokens::Slash>:
                        shift(p, lasttok, lookahead, stack, steps, 166); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 165); break;
                }
                break;
            case 107:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), std::move(pushitem), NonTerminal::_39);
                        }
                        break;
                    case Token::index_of<Tokens::RightArrow>:
                        shift(p, lasttok, lookahead, stack, steps, 168); break;
                }
                break;
            case 108:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_40, stack.back().state), std::move(pushitem), NonTerminal::_40);
                        }
                        break;
                }
                break;
            case 109:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "unary expression")  });
                }
                break;
            case 110:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "unary expression")  });
                }
                break;
            case 111:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "unary expression")  });
                }
                break;
            case 112:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 173); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "unary expression", Tokens::Mut::stringify()), "unary expression")  });
                }
                break;
            case 113:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "unary expression")  });
                }
                break;
            case 114:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 175); break;
                    case Token::index_of<Tokens::Period>:
                        shift(p, lasttok, lookahead, stack, steps, 176); break;
                }
                break;
            case 115:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                    case Token::index_of<Tokens::Period>:
                        shift(p, lasttok, lookahead, stack, steps, 177); break;
                }
                break;
            case 116:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 178); break;
                    case Token::index_of<Tokens::Period>:
                        shift(p, lasttok, lookahead, stack, steps, 179); break;
                }
                break;
            case 117:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), std::move(pushitem), NonTerminal::_42);
                        }
                        break;
                }
                break;
            case 118:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 119:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 120:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 121:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 122:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 123:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 124:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "expression", "primary expression")  });
                }
                break;
            case 125:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 126:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Path>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::PathExpr> push (std::make_unique<ASTNS::PathExpr>(p.sourcefile, span, std::move(a0)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), std::move(pushitem), NonTerminal::_46);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleColon>:
                        shift(p, lasttok, lookahead, stack, steps, 23); break;
                }
                break;
            case 127:
                switch (lookahead.index()) {
                    default: {
                            auto a5 (pop_t(stack));
                            auto a4 (pop_t(stack));
                            auto a3 (pop_a<ASTNS::ImplMemberList>(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a3);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), std::move(pushitem), NonTerminal::_4);
                        }
                        break;
                }
                break;
            case 128:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Dedent>:
                        shift(p, lasttok, lookahead, stack, steps, 181); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Dedent::stringify(), "indented code block")  });
                }
                break;
            case 129:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_a<ASTNS::StmtList>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a1->stmts)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_13, stack.back().state), std::move(pushitem), NonTerminal::_13);
                        }
                        break;
                }
                break;
            case 130:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 182); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CBrace::stringify(), "braced code block")  });
                }
                break;
            case 131:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::Dedent>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), std::move(pushitem), NonTerminal::_66);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Return>:
                        shift(p, lasttok, lookahead, stack, steps, 90); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::Var>:
                        shift(p, lasttok, lookahead, stack, steps, 87); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional statement list", "braced code block")  });
                }
                break;
            case 132:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Stmt>(stack));
                            auto a0 (pop_a<ASTNS::StmtList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->stmts.push_back(std::move(a1));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_59, stack.back().state), std::move(pushitem), NonTerminal::_59);
                        }
                        break;
                }
                break;
            case 133:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Stmt>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_58, stack.back().state), std::move(pushitem), NonTerminal::_58);
                        }
                        break;
                }
                break;
            case 134:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "line ending", "variable declaration")  });
                }
                break;
            case 135:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Comma>:
                        shift(p, lasttok, lookahead, stack, steps, 185); break;
                    default: {
                            auto a0 (pop_a<ASTNS::VarStmtItemList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_56, stack.back().state), std::move(pushitem), NonTerminal::_56);
                        }
                        break;
                }
                break;
            case 136:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::VarStmtItem>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::VarStmtItemList> push (std::make_unique<ASTNS::VarStmtItemList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::VarStmtItem>> {}));

            push->items.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_57, stack.back().state), std::move(pushitem), NonTerminal::_57);
                        }
                        break;
                }
                break;
            case 137:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Colon>:
                        shift(p, lasttok, lookahead, stack, steps, 51); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "required type annotation", "variable binding")  });
                }
                break;
            case 138:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 187); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "variable binding")  });
                }
                break;
            case 139:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::PureLocation>(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), false, Maybe<Span const>()));
                            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_8, stack.back().state), std::move(pushitem), NonTerminal::_8);
                        }
                        break;
                }
                break;
            case 140:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                    case Token::index_of<Tokens::Bang>:
                    case Token::index_of<Tokens::BoolLit>:
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::CharLit>:
                    case Token::index_of<Tokens::Dedent>:
                    case Token::index_of<Tokens::FloatLit>:
                    case Token::index_of<Tokens::Fun>:
                    case Token::index_of<Tokens::Identifier>:
                    case Token::index_of<Tokens::If>:
                    case Token::index_of<Tokens::Impl>:
                    case Token::index_of<Tokens::IntLit>:
                    case Token::index_of<Tokens::Minus>:
                    case Token::index_of<Tokens::OBrace>:
                    case Token::index_of<Tokens::OParen>:
                    case Token::index_of<Tokens::Return>:
                    case Token::index_of<Tokens::Star>:
                    case Token::index_of<Tokens::StringLit>:
                    case Token::index_of<Tokens::This>:
                    case Token::index_of<Tokens::Tilde>:
                    case Token::index_of<Tokens::Var>:
                    case Token::index_of<Tokens::While>:
                    case Token::index_of<Tokens::_EOF>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_70, stack.back().state), std::move(pushitem), NonTerminal::_70);
                        }
                        break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional line ending", "expression statement")  });
                }
                break;
            case 141:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::PureLocation>(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), false, Maybe<Span const>()));
                            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_8, stack.back().state), std::move(pushitem), NonTerminal::_8);
                        }
                        break;
                }
                break;
            case 142:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                    case Token::index_of<Tokens::Bang>:
                    case Token::index_of<Tokens::BoolLit>:
                    case Token::index_of<Tokens::CBrace>:
                    case Token::index_of<Tokens::CharLit>:
                    case Token::index_of<Tokens::Dedent>:
                    case Token::index_of<Tokens::FloatLit>:
                    case Token::index_of<Tokens::Fun>:
                    case Token::index_of<Tokens::Identifier>:
                    case Token::index_of<Tokens::If>:
                    case Token::index_of<Tokens::Impl>:
                    case Token::index_of<Tokens::IntLit>:
                    case Token::index_of<Tokens::Minus>:
                    case Token::index_of<Tokens::OBrace>:
                    case Token::index_of<Tokens::OParen>:
                    case Token::index_of<Tokens::Return>:
                    case Token::index_of<Tokens::Star>:
                    case Token::index_of<Tokens::StringLit>:
                    case Token::index_of<Tokens::This>:
                    case Token::index_of<Tokens::Tilde>:
                    case Token::index_of<Tokens::Var>:
                    case Token::index_of<Tokens::While>:
                    case Token::index_of<Tokens::_EOF>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_70, stack.back().state), std::move(pushitem), NonTerminal::_70);
                        }
                        break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional line ending", "expression statement")  });
                }
                break;
            case 143:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 38); break;
                    case Token::index_of<Tokens::Semicolon>:
                        shift(p, lasttok, lookahead, stack, steps, 39); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "line ending", "return statement")  });
                }
                break;
            case 144:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::PureLocation>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::RetStmt> push (std::make_unique<ASTNS::RetStmt>(p.sourcefile, span, nullptr));
                            std::unique_ptr<ASTNS::RetStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_9, stack.back().state), std::move(pushitem), NonTerminal::_9);
                        }
                        break;
                }
                break;
            case 145:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_24, stack.back().state), std::move(pushitem), NonTerminal::_24);
                        }
                        break;
                }
                break;
            case 146:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_24, stack.back().state), std::move(pushitem), NonTerminal::_24);
                        }
                        break;
                }
                break;
            case 147:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "assignment expression", "assignment expression")  });
                }
                break;
            case 148:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "binary and expression", "binary or expression")  });
                }
                break;
            case 149:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 194); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "code block", "if expression")  });
                }
                break;
            case 150:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 194); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "code block", "while loop expression")  });
                }
                break;
            case 151:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "equality expression", "binary and expression")  });
                }
                break;
            case 152:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "comparison expression", "equality expression")  });
                }
                break;
            case 153:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "comparison expression", "equality expression")  });
                }
                break;
            case 154:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bitwise xor expression", "comparison expression")  });
                }
                break;
            case 155:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bitwise xor expression", "comparison expression")  });
                }
                break;
            case 156:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bitwise xor expression", "comparison expression")  });
                }
                break;
            case 157:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bitwise xor expression", "comparison expression")  });
                }
                break;
            case 158:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bitwise or expression", "bitwise xor expression")  });
                }
                break;
            case 159:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bitwise and expression", "bitwise or expression")  });
                }
                break;
            case 160:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "bit shift expression", "bitwise and expression")  });
                }
                break;
            case 161:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "addition expression", "bit shift expression")  });
                }
                break;
            case 162:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "addition expression", "bit shift expression")  });
                }
                break;
            case 163:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "multiplication expression", "addition expression")  });
                }
                break;
            case 164:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "multiplication expression", "addition expression")  });
                }
                break;
            case 165:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "multiplication expression")  });
                }
                break;
            case 166:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "multiplication expression")  });
                }
                break;
            case 167:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "multiplication expression")  });
                }
                break;
            case 168:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 16); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 17); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "type specifier", "type cast expression")  });
                }
                break;
            case 169:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(p.sourcefile, span, a0, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                }
                break;
            case 170:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(p.sourcefile, span, a0, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                }
                break;
            case 171:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(p.sourcefile, span, a0, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                }
                break;
            case 172:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::AddrofExpr> push (std::make_unique<ASTNS::AddrofExpr>(p.sourcefile, span, a0, std::move(a1), false));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                }
                break;
            case 173:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "unary expression", "unary expression")  });
                }
                break;
            case 174:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::DerefExpr> push (std::make_unique<ASTNS::DerefExpr>(p.sourcefile, span, a0, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                }
                break;
            case 175:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CParen>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), std::move(pushitem), NonTerminal::_65);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional argument list", "function call expression")  });
                }
                break;
            case 176:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 220); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "field access expression"), format("expected {} for {}", Tokens::Identifier::stringify(), "method call expression")  });
                }
                break;
            case 177:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 221); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "field access expression"), format("expected {} for {}", Tokens::Identifier::stringify(), "method call expression")  });
                }
                break;
            case 178:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CParen>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), std::move(pushitem), NonTerminal::_65);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional argument list", "function call expression")  });
                }
                break;
            case 179:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 223); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Identifier::stringify(), "field access expression"), format("expected {} for {}", Tokens::Identifier::stringify(), "method call expression")  });
                }
                break;
            case 180:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 224); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "primary expression")  });
                }
                break;
            case 181:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::StmtList>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a2->stmts)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_14, stack.back().state), std::move(pushitem), NonTerminal::_14);
                        }
                        break;
                }
                break;
            case 182:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::StmtList>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    WARN_BLOCK_NO_INDENT(a0, a3);std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a2->stmts)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_13, stack.back().state), std::move(pushitem), NonTerminal::_13);
                        }
                        break;
                }
                break;
            case 183:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Dedent>:
                        shift(p, lasttok, lookahead, stack, steps, 225); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Dedent::stringify(), "braced code block")  });
                }
                break;
            case 184:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::PureLocation>(stack));
                            auto a1 (pop_a<ASTNS::VarStmtItemList>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::VarStmt> push (std::make_unique<ASTNS::VarStmt>(p.sourcefile, span, std::move(a1->items)));
                            std::unique_ptr<ASTNS::VarStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_7, stack.back().state), std::move(pushitem), NonTerminal::_7);
                        }
                        break;
                }
                break;
            case 185:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 137); break;
                    case Token::index_of<Tokens::Mut>:
                        shift(p, lasttok, lookahead, stack, steps, 138); break;
                    default: {
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::VarStmtItemList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_56, stack.back().state), std::move(pushitem), NonTerminal::_56);
                        }
                        break;
                }
                break;
            case 186:
                switch (lookahead.index()) {
                    default: {
                            auto a1 (pop_a<ASTNS::Type>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a1), false, a0, a0, nullptr));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), std::move(pushitem), NonTerminal::_10);
                        }
                        break;
                    case Token::index_of<Tokens::Equal>:
                        shift(p, lasttok, lookahead, stack, steps, 228); break;
                }
                break;
            case 187:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Colon>:
                        shift(p, lasttok, lookahead, stack, steps, 51); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "required type annotation", "variable binding")  });
                }
                break;
            case 188:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::PureLocation>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), true , Maybe<Span const>(a1.span)));
                            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_8, stack.back().state), std::move(pushitem), NonTerminal::_8);
                        }
                        break;
                }
                break;
            case 189:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::PureLocation>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), true , Maybe<Span const>(a1.span)));
                            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_8, stack.back().state), std::move(pushitem), NonTerminal::_8);
                        }
                        break;
                }
                break;
            case 190:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::PureLocation>(stack));
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::RetStmt> push (std::make_unique<ASTNS::RetStmt>(p.sourcefile, span, std::move(a1)));
                            std::unique_ptr<ASTNS::RetStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_9, stack.back().state), std::move(pushitem), NonTerminal::_9);
                        }
                        break;
                }
                break;
            case 191:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::AssignmentExpr> push (std::make_unique<ASTNS::AssignmentExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_29, stack.back().state), std::move(pushitem), NonTerminal::_29);
                        }
                        break;
                }
                break;
            case 192:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ShortCircuitExpr> push (std::make_unique<ASTNS::ShortCircuitExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_30, stack.back().state), std::move(pushitem), NonTerminal::_30);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleAmper>:
                        shift(p, lasttok, lookahead, stack, steps, 151); break;
                }
                break;
            case 193:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Block>(stack));
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(p.sourcefile, span, a0, a0, std::move(a1), std::move(a2), nullptr));
                            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_27, stack.back().state), std::move(pushitem), NonTerminal::_27);
                        }
                        break;
                    case Token::index_of<Tokens::Else>:
                        shift(p, lasttok, lookahead, stack, steps, 230); break;
                }
                break;
            case 194:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Indent>:
                        shift(p, lasttok, lookahead, stack, steps, 79); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::Indent::stringify(), "indented code block")  });
                }
                break;
            case 195:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Block>(stack));
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::WhileExpr> push (std::make_unique<ASTNS::WhileExpr>(p.sourcefile, span, std::move(a1), std::move(a2)));
                            std::unique_ptr<ASTNS::WhileExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_28, stack.back().state), std::move(pushitem), NonTerminal::_28);
                        }
                        break;
                }
                break;
            case 196:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::BangEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 152); break;
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ShortCircuitExpr> push (std::make_unique<ASTNS::ShortCircuitExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_31, stack.back().state), std::move(pushitem), NonTerminal::_31);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 153); break;
                }
                break;
            case 197:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_32, stack.back().state), std::move(pushitem), NonTerminal::_32);
                        }
                        break;
                    case Token::index_of<Tokens::Greater>:
                        shift(p, lasttok, lookahead, stack, steps, 155); break;
                    case Token::index_of<Tokens::GreaterEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 157); break;
                    case Token::index_of<Tokens::Less>:
                        shift(p, lasttok, lookahead, stack, steps, 154); break;
                    case Token::index_of<Tokens::LessEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 156); break;
                }
                break;
            case 198:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_32, stack.back().state), std::move(pushitem), NonTerminal::_32);
                        }
                        break;
                    case Token::index_of<Tokens::Greater>:
                        shift(p, lasttok, lookahead, stack, steps, 155); break;
                    case Token::index_of<Tokens::GreaterEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 157); break;
                    case Token::index_of<Tokens::Less>:
                        shift(p, lasttok, lookahead, stack, steps, 154); break;
                    case Token::index_of<Tokens::LessEqual>:
                        shift(p, lasttok, lookahead, stack, steps, 156); break;
                }
                break;
            case 199:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), std::move(pushitem), NonTerminal::_33);
                        }
                        break;
                    case Token::index_of<Tokens::Caret>:
                        shift(p, lasttok, lookahead, stack, steps, 158); break;
                }
                break;
            case 200:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), std::move(pushitem), NonTerminal::_33);
                        }
                        break;
                    case Token::index_of<Tokens::Caret>:
                        shift(p, lasttok, lookahead, stack, steps, 158); break;
                }
                break;
            case 201:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), std::move(pushitem), NonTerminal::_33);
                        }
                        break;
                    case Token::index_of<Tokens::Caret>:
                        shift(p, lasttok, lookahead, stack, steps, 158); break;
                }
                break;
            case 202:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), std::move(pushitem), NonTerminal::_33);
                        }
                        break;
                    case Token::index_of<Tokens::Caret>:
                        shift(p, lasttok, lookahead, stack, steps, 158); break;
                }
                break;
            case 203:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), std::move(pushitem), NonTerminal::_34);
                        }
                        break;
                    case Token::index_of<Tokens::Pipe>:
                        shift(p, lasttok, lookahead, stack, steps, 159); break;
                }
                break;
            case 204:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 160); break;
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_35, stack.back().state), std::move(pushitem), NonTerminal::_35);
                        }
                        break;
                }
                break;
            case 205:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_36, stack.back().state), std::move(pushitem), NonTerminal::_36);
                        }
                        break;
                    case Token::index_of<Tokens::DoubleGreater>:
                        shift(p, lasttok, lookahead, stack, steps, 161); break;
                    case Token::index_of<Tokens::DoubleLess>:
                        shift(p, lasttok, lookahead, stack, steps, 162); break;
                }
                break;
            case 206:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_37, stack.back().state), std::move(pushitem), NonTerminal::_37);
                        }
                        break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 164); break;
                    case Token::index_of<Tokens::Plus>:
                        shift(p, lasttok, lookahead, stack, steps, 163); break;
                }
                break;
            case 207:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_37, stack.back().state), std::move(pushitem), NonTerminal::_37);
                        }
                        break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 164); break;
                    case Token::index_of<Tokens::Plus>:
                        shift(p, lasttok, lookahead, stack, steps, 163); break;
                }
                break;
            case 208:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_38, stack.back().state), std::move(pushitem), NonTerminal::_38);
                        }
                        break;
                    case Token::index_of<Tokens::Percent>:
                        shift(p, lasttok, lookahead, stack, steps, 167); break;
                    case Token::index_of<Tokens::Slash>:
                        shift(p, lasttok, lookahead, stack, steps, 166); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 165); break;
                }
                break;
            case 209:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_38, stack.back().state), std::move(pushitem), NonTerminal::_38);
                        }
                        break;
                    case Token::index_of<Tokens::Percent>:
                        shift(p, lasttok, lookahead, stack, steps, 167); break;
                    case Token::index_of<Tokens::Slash>:
                        shift(p, lasttok, lookahead, stack, steps, 166); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 165); break;
                }
                break;
            case 210:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), std::move(pushitem), NonTerminal::_39);
                        }
                        break;
                }
                break;
            case 211:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), std::move(pushitem), NonTerminal::_39);
                        }
                        break;
                }
                break;
            case 212:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), std::move(pushitem), NonTerminal::_39);
                        }
                        break;
                }
                break;
            case 213:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Type>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::CastExpr> push (std::make_unique<ASTNS::CastExpr>(p.sourcefile, span, std::move(a2), std::move(a0)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_40, stack.back().state), std::move(pushitem), NonTerminal::_40);
                        }
                        break;
                }
                break;
            case 214:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Expr>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::AddrofExpr> push (std::make_unique<ASTNS::AddrofExpr>(p.sourcefile, span, a0, std::move(a2), true));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), std::move(pushitem), NonTerminal::_41);
                        }
                        break;
                }
                break;
            case 215:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 231); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "function call expression")  });
                }
                break;
            case 216:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ArgList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), std::move(pushitem), NonTerminal::_65);
                        }
                        break;
                }
                break;
            case 217:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::ArgList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_53, stack.back().state), std::move(pushitem), NonTerminal::_53);
                        }
                        break;
                    case Token::index_of<Tokens::Comma>:
                        shift(p, lasttok, lookahead, stack, steps, 232); break;
                }
                break;
            case 218:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Arg>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));

            push->args.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_54, stack.back().state), std::move(pushitem), NonTerminal::_54);
                        }
                        break;
                }
                break;
            case 219:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Arg> push (std::make_unique<ASTNS::Arg>(p.sourcefile, span, std::move(a0)));
                            std::unique_ptr<ASTNS::Arg> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_20, stack.back().state), std::move(pushitem), NonTerminal::_20);
                        }
                        break;
                }
                break;
            case 220:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(p.sourcefile, span, std::move(a0), a1, a2));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_43, stack.back().state), std::move(pushitem), NonTerminal::_43);
                        }
                        break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 233); break;
                }
                break;
            case 221:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(p.sourcefile, span, std::move(a0), a1, a2));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_43, stack.back().state), std::move(pushitem), NonTerminal::_43);
                        }
                        break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 234); break;
                }
                break;
            case 222:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 235); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "function call expression")  });
                }
                break;
            case 223:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(p.sourcefile, span, std::move(a0), a1, a2));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_43, stack.back().state), std::move(pushitem), NonTerminal::_43);
                        }
                        break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 236); break;
                }
                break;
            case 224:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_t(stack));
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), std::move(pushitem), NonTerminal::_45);
                        }
                        break;
                }
                break;
            case 225:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 237); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CBrace::stringify(), "braced code block")  });
                }
                break;
            case 226:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::VarStmtItem>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::VarStmtItemList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->items.push_back(std::move(a2));
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_57, stack.back().state), std::move(pushitem), NonTerminal::_57);
                        }
                        break;
                }
                break;
            case 227:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::VarStmtItem>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_55, stack.back().state), std::move(pushitem), NonTerminal::_55);
                        }
                        break;
                }
                break;
            case 228:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "expression", "variable binding")  });
                }
                break;
            case 229:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Type>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a2), true, a1, a1, nullptr));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), std::move(pushitem), NonTerminal::_10);
                        }
                        break;
                    case Token::index_of<Tokens::Equal>:
                        shift(p, lasttok, lookahead, stack, steps, 239); break;
                }
                break;
            case 230:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::Newline>:
                        shift(p, lasttok, lookahead, stack, steps, 194); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", format("either {} or {}", "code block", "if expression"), "if expression")  });
                }
                break;
            case 231:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::ArgList>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::CallExpr> push (std::make_unique<ASTNS::CallExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), std::move(pushitem), NonTerminal::_42);
                        }
                        break;
                }
                break;
            case 232:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    default: {
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::ArgList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_53, stack.back().state), std::move(pushitem), NonTerminal::_53);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                }
                break;
            case 233:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CParen>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), std::move(pushitem), NonTerminal::_65);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional argument list", "method call expression")  });
                }
                break;
            case 234:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CParen>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), std::move(pushitem), NonTerminal::_65);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional argument list", "method call expression")  });
                }
                break;
            case 235:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::ArgList>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::CallExpr> push (std::make_unique<ASTNS::CallExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), std::move(pushitem), NonTerminal::_42);
                        }
                        break;
                }
                break;
            case 236:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CParen>:
    {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), std::move(pushitem), NonTerminal::_65);
                        }
                        break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "optional argument list", "method call expression")  });
                }
                break;
            case 237:
                switch (lookahead.index()) {
                    default: {
                            auto a5 (pop_t(stack));
                            auto a4 (pop_t(stack));
                            auto a3 (pop_a<ASTNS::StmtList>(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a3->stmts)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_13, stack.back().state), std::move(pushitem), NonTerminal::_13);
                        }
                        break;
                }
                break;
            case 238:
                switch (lookahead.index()) {
                    default: {
                            auto a3 (pop_a<ASTNS::Expr>(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_a<ASTNS::Type>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a3 && a3->span().has() ? Maybe<Location const>(a3->span().get().end) :
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a1), false, a0, a2, std::move(a3)));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), std::move(pushitem), NonTerminal::_10);
                        }
                        break;
                }
                break;
            case 239:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::Amper>:
                        shift(p, lasttok, lookahead, stack, steps, 112); break;
                    case Token::index_of<Tokens::Bang>:
                        shift(p, lasttok, lookahead, stack, steps, 111); break;
                    case Token::index_of<Tokens::BoolLit>:
                        shift(p, lasttok, lookahead, stack, steps, 118); break;
                    case Token::index_of<Tokens::CharLit>:
                        shift(p, lasttok, lookahead, stack, steps, 121); break;
                    case Token::index_of<Tokens::FloatLit>:
                        shift(p, lasttok, lookahead, stack, steps, 119); break;
                    case Token::index_of<Tokens::Identifier>:
                        shift(p, lasttok, lookahead, stack, steps, 18); break;
                    case Token::index_of<Tokens::If>:
                        shift(p, lasttok, lookahead, stack, steps, 96); break;
                    case Token::index_of<Tokens::IntLit>:
                        shift(p, lasttok, lookahead, stack, steps, 120); break;
                    case Token::index_of<Tokens::Minus>:
                        shift(p, lasttok, lookahead, stack, steps, 110); break;
                    case Token::index_of<Tokens::OBrace>:
                        shift(p, lasttok, lookahead, stack, steps, 76); break;
                    case Token::index_of<Tokens::OParen>:
                        shift(p, lasttok, lookahead, stack, steps, 124); break;
                    case Token::index_of<Tokens::Star>:
                        shift(p, lasttok, lookahead, stack, steps, 113); break;
                    case Token::index_of<Tokens::StringLit>:
                        shift(p, lasttok, lookahead, stack, steps, 122); break;
                    case Token::index_of<Tokens::This>:
                        shift(p, lasttok, lookahead, stack, steps, 123); break;
                    case Token::index_of<Tokens::Tilde>:
                        shift(p, lasttok, lookahead, stack, steps, 109); break;
                    case Token::index_of<Tokens::While>:
                        shift(p, lasttok, lookahead, stack, steps, 97); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", "expression", "variable binding")  });
                }
                break;
            case 240:
                switch (lookahead.index()) {
                    default: {
                            auto a4 (pop_a<ASTNS::Block>(stack));
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::Block>(stack));
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(p.sourcefile, span, a0, a3, std::move(a1), std::move(a2), std::move(a4)));
                            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_27, stack.back().state), std::move(pushitem), NonTerminal::_27);
                        }
                        break;
                }
                break;
            case 241:
                switch (lookahead.index()) {
                    default: {
                            auto a4 (pop_a<ASTNS::IfExpr>(stack));
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::Block>(stack));
                            auto a1 (pop_a<ASTNS::Expr>(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(p.sourcefile, span, a0, a3, std::move(a1), std::move(a2), std::move(a4)));
                            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_27, stack.back().state), std::move(pushitem), NonTerminal::_27);
                        }
                        break;
                }
                break;
            case 242:
                switch (lookahead.index()) {
                    default: {
                            auto a2 (pop_a<ASTNS::Arg>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::ArgList>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    a0->args.push_back(std::move(a2));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_54, stack.back().state), std::move(pushitem), NonTerminal::_54);
                        }
                        break;
                }
                break;
            case 243:
                switch (lookahead.index()) {
                    default: {
                            auto a0 (pop_a<ASTNS::Arg>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Arg> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_52, stack.back().state), std::move(pushitem), NonTerminal::_52);
                        }
                        break;
                }
                break;
            case 244:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 248); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "method call expression")  });
                }
                break;
            case 245:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 249); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "method call expression")  });
                }
                break;
            case 246:
                switch (lookahead.index()) {
                    case Token::index_of<Tokens::CParen>:
                        shift(p, lasttok, lookahead, stack, steps, 250); break;
                    default:
                        if (istrial) return false;
                        error(done, errored, errorstate(p, stack, lasttok, lookahead), std::vector<std::string> {  format("expected {} for {}", Tokens::CParen::stringify(), "method call expression")  });
                }
                break;
            case 247:
                switch (lookahead.index()) {
                    default: {
                            auto a4 (pop_a<ASTNS::Expr>(stack));
                            auto a3 (pop_t(stack));
                            auto a2 (pop_a<ASTNS::Type>(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_t(stack));
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a2), true, a1, a3, std::move(a4)));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), std::move(pushitem), NonTerminal::_10);
                        }
                        break;
                }
                break;
            case 248:
                switch (lookahead.index()) {
                    default: {
                            auto a5 (pop_t(stack));
                            auto a4 (pop_a<ASTNS::ArgList>(stack));
                            auto a3 (pop_t(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(p.sourcefile, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_44, stack.back().state), std::move(pushitem), NonTerminal::_44);
                        }
                        break;
                }
                break;
            case 249:
                switch (lookahead.index()) {
                    default: {
                            auto a5 (pop_t(stack));
                            auto a4 (pop_a<ASTNS::ArgList>(stack));
                            auto a3 (pop_t(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(p.sourcefile, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_44, stack.back().state), std::move(pushitem), NonTerminal::_44);
                        }
                        break;
                }
                break;
            case 250:
                switch (lookahead.index()) {
                    default: {
                            auto a5 (pop_t(stack));
                            auto a4 (pop_a<ASTNS::ArgList>(stack));
                            auto a3 (pop_t(stack));
                            auto a2 (pop_t(stack));
                            auto a1 (pop_t(stack));
                            auto a0 (pop_a<ASTNS::Expr>(stack));
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
    std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(p.sourcefile, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_44, stack.back().state), std::move(pushitem), NonTerminal::_44);
                        }
                        break;
                }
                break;
            default:
                report_abort_noh(format("Parser reached invalid state: {}", stack.back().state));
        }
    }
    // PARSERLOOP END }}}

    if (istrial)
        return true;

    if (errored) {
        p.errored = true;
        out = nullptr;
        return false;
    }

    stackitem topsi (std::move(stack.back()));
    ASSERT(std::holds_alternative<astitem>(topsi.item))

    astitem &ai = std::get<astitem>(topsi.item);
    std::unique_ptr<ASTNS::AST> astu (std::move(ai.ast));
    ASTNS::CUB *cub = static_cast<ASTNS::CUB*>(astu.release());
    out = std::unique_ptr<ASTNS::CUB>(cub);
    return true;
}
