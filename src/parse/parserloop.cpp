#include "parse/parser.h"
#include "parserlocal.h"
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

template <typename T>
struct RetAndToken {
    T value;
    Located<TokenData> lookahead;
};

// PARSESTATES START {{{
static RetAndToken<std::unique_ptr<ASTNS::CUB>> state_0(Parser &parser);
static RetAndToken<std::unique_ptr<ASTNS::CUB>> state_1(Parser &parser, std::unique_ptr<ASTNS::CU> const &a0);
static RetAndToken<std::variant<std::unique_ptr<ASTNS::CU>, std::unique_ptr<ASTNS::DeclList>>> state_2(Parser &parser, std::unique_ptr<ASTNS::DeclList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::DeclList>> state_3(Parser &parser, std::unique_ptr<ASTNS::Decl> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_4(Parser &parser, std::unique_ptr<ASTNS::FunctionDecl> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_5(Parser &parser, std::unique_ptr<ASTNS::Decl> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_6(Parser &parser, Located<Tokens::Fun> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_7(Parser &parser, Located<Tokens::Impl> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::DeclList>> state_8(Parser &parser, std::unique_ptr<ASTNS::DeclList> const &a0, std::unique_ptr<ASTNS::Decl> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_9(Parser &parser, std::unique_ptr<ASTNS::Decl> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_10(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_11(Parser &parser, Located<Tokens::Impl> const &a0, std::unique_ptr<ASTNS::Type> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Type>> state_12(Parser &parser, std::unique_ptr<ASTNS::PathType> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Type>> state_13(Parser &parser, std::unique_ptr<ASTNS::PointerType> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Type>> state_14(Parser &parser, std::unique_ptr<ASTNS::ThisType> const &a0);
static RetAndToken<std::variant<std::unique_ptr<ASTNS::Path>, std::unique_ptr<ASTNS::PathType>>> state_15(Parser &parser, std::unique_ptr<ASTNS::Path> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::PointerType>> state_16(Parser &parser, Located<Tokens::Star> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ThisType>> state_17(Parser &parser, Located<Tokens::This> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Path>> state_18(Parser &parser, Located<Tokens::Identifier> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_19(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_20(Parser &parser, Located<Tokens::Impl> const &a0, std::unique_ptr<ASTNS::Type> const &a1, std::unique_ptr<ASTNS::ImplMemberList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_21(Parser &parser, Located<Tokens::OBrace> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_22(Parser &parser, Located<Tokens::Newline> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Path>> state_23(Parser &parser, std::unique_ptr<ASTNS::Path> const &a0, Located<Tokens::DoubleColon> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::PointerType>> state_24(Parser &parser, Located<Tokens::Star> const &a0, std::unique_ptr<ASTNS::Type> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::PointerType>> state_25(Parser &parser, Located<Tokens::Star> const &a0, Located<Tokens::Mut> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_26(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2, std::unique_ptr<ASTNS::ParamList> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::ParamList>> state_27(Parser &parser, std::unique_ptr<ASTNS::ParamList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ParamList>> state_28(Parser &parser, std::unique_ptr<ASTNS::ParamList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ParamList>> state_29(Parser &parser, std::unique_ptr<ASTNS::ParamB> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ParamB>> state_30(Parser &parser, std::unique_ptr<ASTNS::Param> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ParamB>> state_31(Parser &parser, std::unique_ptr<ASTNS::ThisParam> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Param>> state_32(Parser &parser, Located<Tokens::Identifier> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Param>> state_33(Parser &parser, Located<Tokens::Mut> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ThisParam>> state_34(Parser &parser, Located<Tokens::This> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ThisParam>> state_35(Parser &parser, Located<Tokens::Star> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Decl>> state_36(Parser &parser, Located<Tokens::Impl> const &a0, std::unique_ptr<ASTNS::Type> const &a1, std::unique_ptr<ASTNS::ImplMemberList> const &a2, std::unique_ptr<ASTNS::PureLocation> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::PureLocation>> state_37(Parser &parser, std::unique_ptr<ASTNS::PureLocation> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::PureLocation>> state_38(Parser &parser, Located<Tokens::Newline> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::PureLocation>> state_39(Parser &parser, Located<Tokens::Semicolon> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_40(Parser &parser, Located<Tokens::OBrace> const &a0, std::unique_ptr<ASTNS::ImplMemberList> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_41(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_42(Parser &parser, std::unique_ptr<ASTNS::ImplMemberList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_43(Parser &parser, std::unique_ptr<ASTNS::ImplMember> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMember>> state_44(Parser &parser, std::unique_ptr<ASTNS::FunctionDecl> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_45(Parser &parser, Located<Tokens::Newline> const &a0, Located<Tokens::Indent> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Path>> state_46(Parser &parser, std::unique_ptr<ASTNS::Path> const &a0, Located<Tokens::DoubleColon> const &a1, Located<Tokens::Identifier> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::PointerType>> state_47(Parser &parser, Located<Tokens::Star> const &a0, Located<Tokens::Mut> const &a1, std::unique_ptr<ASTNS::Type> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_48(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2, std::unique_ptr<ASTNS::ParamList> const &a3, Located<Tokens::CParen> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::ParamList>> state_49(Parser &parser, std::unique_ptr<ASTNS::ParamList> const &a0, Located<Tokens::Comma> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Param>> state_50(Parser &parser, Located<Tokens::Identifier> const &a0, std::unique_ptr<ASTNS::Type> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Type>> state_51(Parser &parser, Located<Tokens::Colon> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Param>> state_52(Parser &parser, Located<Tokens::Mut> const &a0, Located<Tokens::Identifier> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ThisParam>> state_53(Parser &parser, Located<Tokens::Star> const &a0, Located<Tokens::This> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ThisParam>> state_54(Parser &parser, Located<Tokens::Star> const &a0, Located<Tokens::Mut> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::PureLocation>> state_55(Parser &parser, Located<Tokens::Semicolon> const &a0, Located<Tokens::Newline> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_56(Parser &parser, Located<Tokens::OBrace> const &a0, std::unique_ptr<ASTNS::ImplMemberList> const &a1, Located<Tokens::CBrace> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_57(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, std::unique_ptr<ASTNS::ImplMemberList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_58(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_59(Parser &parser, std::unique_ptr<ASTNS::ImplMemberList> const &a0, std::unique_ptr<ASTNS::ImplMember> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ImplMember>> state_60(Parser &parser, std::unique_ptr<ASTNS::ImplMember> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_61(Parser &parser, Located<Tokens::Newline> const &a0, Located<Tokens::Indent> const &a1, std::unique_ptr<ASTNS::ImplMemberList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_62(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2, std::unique_ptr<ASTNS::ParamList> const &a3, Located<Tokens::CParen> const &a4, std::unique_ptr<ASTNS::Type> const &a5);
static RetAndToken<std::unique_ptr<ASTNS::ParamList>> state_63(Parser &parser, std::unique_ptr<ASTNS::ParamList> const &a0, Located<Tokens::Comma> const &a1, std::unique_ptr<ASTNS::ParamB> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ParamB>> state_64(Parser &parser, std::unique_ptr<ASTNS::ParamB> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Type>> state_65(Parser &parser, Located<Tokens::Colon> const &a0, std::unique_ptr<ASTNS::Type> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Param>> state_66(Parser &parser, Located<Tokens::Mut> const &a0, Located<Tokens::Identifier> const &a1, std::unique_ptr<ASTNS::Type> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ThisParam>> state_67(Parser &parser, Located<Tokens::Star> const &a0, Located<Tokens::Mut> const &a1, Located<Tokens::This> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_68(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, std::unique_ptr<ASTNS::ImplMemberList> const &a2, Located<Tokens::CBrace> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_69(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2, std::unique_ptr<ASTNS::ImplMemberList> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_70(Parser &parser, Located<Tokens::Newline> const &a0, Located<Tokens::Indent> const &a1, std::unique_ptr<ASTNS::ImplMemberList> const &a2, Located<Tokens::Dedent> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_71(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2, std::unique_ptr<ASTNS::ParamList> const &a3, Located<Tokens::CParen> const &a4, std::unique_ptr<ASTNS::Type> const &a5, std::unique_ptr<ASTNS::Block> const &a6);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_72(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2, std::unique_ptr<ASTNS::ParamList> const &a3, Located<Tokens::CParen> const &a4, std::unique_ptr<ASTNS::Type> const &a5, std::unique_ptr<ASTNS::PureLocation> const &a6);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_73(Parser &parser, std::unique_ptr<ASTNS::Block> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_74(Parser &parser, std::unique_ptr<ASTNS::Block> const &a0);
static RetAndToken<std::variant<std::unique_ptr<ASTNS::Block>, std::unique_ptr<ASTNS::PureLocation>>> state_75(Parser &parser, Located<Tokens::Newline> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_76(Parser &parser, Located<Tokens::OBrace> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_77(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2, std::unique_ptr<ASTNS::ImplMemberList> const &a3, Located<Tokens::Dedent> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>> state_78(Parser &parser, Located<Tokens::Fun> const &a0, Located<Tokens::Identifier> const &a1, Located<Tokens::OParen> const &a2, std::unique_ptr<ASTNS::ParamList> const &a3, Located<Tokens::CParen> const &a4, std::unique_ptr<ASTNS::Type> const &a5, std::unique_ptr<ASTNS::Block> const &a6, std::unique_ptr<ASTNS::PureLocation> const &a7);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_79(Parser &parser, Located<Tokens::Newline> const &a0, Located<Tokens::Indent> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_80(Parser &parser, Located<Tokens::OBrace> const &a0, std::unique_ptr<ASTNS::StmtList> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_81(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::StmtList>> state_82(Parser &parser, std::unique_ptr<ASTNS::StmtList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::StmtList>> state_83(Parser &parser, std::unique_ptr<ASTNS::Stmt> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Stmt>> state_84(Parser &parser, std::unique_ptr<ASTNS::VarStmt> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Stmt>> state_85(Parser &parser, std::unique_ptr<ASTNS::ExprStmt> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Stmt>> state_86(Parser &parser, std::unique_ptr<ASTNS::RetStmt> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::VarStmt>> state_87(Parser &parser, Located<Tokens::Var> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_88(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_89(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::RetStmt>> state_90(Parser &parser, Located<Tokens::Return> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_91(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_92(Parser &parser, std::unique_ptr<ASTNS::IfExpr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_93(Parser &parser, std::unique_ptr<ASTNS::WhileExpr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_94(Parser &parser, std::unique_ptr<ASTNS::Block> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_95(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::IfExpr>> state_96(Parser &parser, Located<Tokens::If> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::WhileExpr>> state_97(Parser &parser, Located<Tokens::While> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_98(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_99(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_100(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_101(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_102(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_103(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_104(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_105(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_106(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_107(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_108(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_109(Parser &parser, Located<Tokens::Tilde> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_110(Parser &parser, Located<Tokens::Minus> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_111(Parser &parser, Located<Tokens::Bang> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_112(Parser &parser, Located<Tokens::Amper> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_113(Parser &parser, Located<Tokens::Star> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_114(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_115(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_116(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_117(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_118(Parser &parser, Located<Tokens::BoolLit> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_119(Parser &parser, Located<Tokens::FloatLit> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_120(Parser &parser, Located<Tokens::IntLit> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_121(Parser &parser, Located<Tokens::CharLit> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_122(Parser &parser, Located<Tokens::StringLit> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_123(Parser &parser, Located<Tokens::This> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_124(Parser &parser, Located<Tokens::OParen> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_125(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::variant<std::unique_ptr<ASTNS::Expr>, std::unique_ptr<ASTNS::Path>>> state_126(Parser &parser, std::unique_ptr<ASTNS::Path> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>> state_127(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2, std::unique_ptr<ASTNS::ImplMemberList> const &a3, Located<Tokens::Dedent> const &a4, Located<Tokens::CBrace> const &a5);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_128(Parser &parser, Located<Tokens::Newline> const &a0, Located<Tokens::Indent> const &a1, std::unique_ptr<ASTNS::StmtList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_129(Parser &parser, Located<Tokens::OBrace> const &a0, std::unique_ptr<ASTNS::StmtList> const &a1, Located<Tokens::CBrace> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_130(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, std::unique_ptr<ASTNS::StmtList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_131(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::StmtList>> state_132(Parser &parser, std::unique_ptr<ASTNS::StmtList> const &a0, std::unique_ptr<ASTNS::Stmt> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Stmt>> state_133(Parser &parser, std::unique_ptr<ASTNS::Stmt> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::VarStmt>> state_134(Parser &parser, Located<Tokens::Var> const &a0, std::unique_ptr<ASTNS::VarStmtItemList> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>> state_135(Parser &parser, std::unique_ptr<ASTNS::VarStmtItemList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>> state_136(Parser &parser, std::unique_ptr<ASTNS::VarStmtItem> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_137(Parser &parser, Located<Tokens::Identifier> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_138(Parser &parser, Located<Tokens::Mut> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_139(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, std::unique_ptr<ASTNS::PureLocation> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_140(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Dollar> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_141(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, std::unique_ptr<ASTNS::PureLocation> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_142(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Dollar> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::RetStmt>> state_143(Parser &parser, Located<Tokens::Return> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::RetStmt>> state_144(Parser &parser, Located<Tokens::Return> const &a0, std::unique_ptr<ASTNS::PureLocation> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_145(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_146(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_147(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Equal> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_148(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoublePipe> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::IfExpr>> state_149(Parser &parser, Located<Tokens::If> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::WhileExpr>> state_150(Parser &parser, Located<Tokens::While> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_151(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleAmper> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_152(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::BangEqual> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_153(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleEqual> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_154(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Less> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_155(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Greater> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_156(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::LessEqual> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_157(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::GreaterEqual> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_158(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Caret> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_159(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Pipe> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_160(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Amper> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_161(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleGreater> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_162(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleLess> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_163(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Plus> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_164(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Minus> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_165(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Star> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_166(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Slash> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_167(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Percent> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_168(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::RightArrow> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_169(Parser &parser, Located<Tokens::Tilde> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_170(Parser &parser, Located<Tokens::Minus> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_171(Parser &parser, Located<Tokens::Bang> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_172(Parser &parser, Located<Tokens::Amper> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_173(Parser &parser, Located<Tokens::Amper> const &a0, Located<Tokens::Mut> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_174(Parser &parser, Located<Tokens::Star> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_175(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::OParen> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_176(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_177(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_178(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::OParen> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_179(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_180(Parser &parser, Located<Tokens::OParen> const &a0, std::unique_ptr<ASTNS::Expr> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_181(Parser &parser, Located<Tokens::Newline> const &a0, Located<Tokens::Indent> const &a1, std::unique_ptr<ASTNS::StmtList> const &a2, Located<Tokens::Dedent> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_182(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, std::unique_ptr<ASTNS::StmtList> const &a2, Located<Tokens::CBrace> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_183(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2, std::unique_ptr<ASTNS::StmtList> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::VarStmt>> state_184(Parser &parser, Located<Tokens::Var> const &a0, std::unique_ptr<ASTNS::VarStmtItemList> const &a1, std::unique_ptr<ASTNS::PureLocation> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>> state_185(Parser &parser, std::unique_ptr<ASTNS::VarStmtItemList> const &a0, Located<Tokens::Comma> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_186(Parser &parser, Located<Tokens::Identifier> const &a0, std::unique_ptr<ASTNS::Type> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_187(Parser &parser, Located<Tokens::Mut> const &a0, Located<Tokens::Identifier> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_188(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Dollar> const &a1, std::unique_ptr<ASTNS::PureLocation> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>> state_189(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Dollar> const &a1, std::unique_ptr<ASTNS::PureLocation> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::RetStmt>> state_190(Parser &parser, Located<Tokens::Return> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, std::unique_ptr<ASTNS::PureLocation> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_191(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Equal> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_192(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoublePipe> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::IfExpr>> state_193(Parser &parser, Located<Tokens::If> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, std::unique_ptr<ASTNS::Block> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_194(Parser &parser, Located<Tokens::Newline> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::WhileExpr>> state_195(Parser &parser, Located<Tokens::While> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, std::unique_ptr<ASTNS::Block> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_196(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleAmper> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_197(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::BangEqual> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_198(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleEqual> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_199(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Less> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_200(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Greater> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_201(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::LessEqual> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_202(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::GreaterEqual> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_203(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Caret> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_204(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Pipe> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_205(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Amper> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_206(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleGreater> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_207(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::DoubleLess> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_208(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Plus> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_209(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Minus> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_210(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Star> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_211(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Slash> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_212(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Percent> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_213(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::RightArrow> const &a1, std::unique_ptr<ASTNS::Type> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_214(Parser &parser, Located<Tokens::Amper> const &a0, Located<Tokens::Mut> const &a1, std::unique_ptr<ASTNS::Expr> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_215(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::OParen> const &a1, std::unique_ptr<ASTNS::ArgList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::ArgList>> state_216(Parser &parser, std::unique_ptr<ASTNS::ArgList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ArgList>> state_217(Parser &parser, std::unique_ptr<ASTNS::ArgList> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::ArgList>> state_218(Parser &parser, std::unique_ptr<ASTNS::Arg> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Arg>> state_219(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_220(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_221(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_222(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::OParen> const &a1, std::unique_ptr<ASTNS::ArgList> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_223(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_224(Parser &parser, Located<Tokens::OParen> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, Located<Tokens::CParen> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_225(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2, std::unique_ptr<ASTNS::StmtList> const &a3, Located<Tokens::Dedent> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>> state_226(Parser &parser, std::unique_ptr<ASTNS::VarStmtItemList> const &a0, Located<Tokens::Comma> const &a1, std::unique_ptr<ASTNS::VarStmtItem> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_227(Parser &parser, std::unique_ptr<ASTNS::VarStmtItem> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_228(Parser &parser, Located<Tokens::Identifier> const &a0, std::unique_ptr<ASTNS::Type> const &a1, Located<Tokens::Equal> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_229(Parser &parser, Located<Tokens::Mut> const &a0, Located<Tokens::Identifier> const &a1, std::unique_ptr<ASTNS::Type> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::IfExpr>> state_230(Parser &parser, Located<Tokens::If> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, std::unique_ptr<ASTNS::Block> const &a2, Located<Tokens::Else> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_231(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::OParen> const &a1, std::unique_ptr<ASTNS::ArgList> const &a2, Located<Tokens::CParen> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::ArgList>> state_232(Parser &parser, std::unique_ptr<ASTNS::ArgList> const &a0, Located<Tokens::Comma> const &a1);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_233(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_234(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_235(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::OParen> const &a1, std::unique_ptr<ASTNS::ArgList> const &a2, Located<Tokens::CParen> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_236(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::Block>> state_237(Parser &parser, Located<Tokens::OBrace> const &a0, Located<Tokens::Newline> const &a1, Located<Tokens::Indent> const &a2, std::unique_ptr<ASTNS::StmtList> const &a3, Located<Tokens::Dedent> const &a4, Located<Tokens::CBrace> const &a5);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_238(Parser &parser, Located<Tokens::Identifier> const &a0, std::unique_ptr<ASTNS::Type> const &a1, Located<Tokens::Equal> const &a2, std::unique_ptr<ASTNS::Expr> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_239(Parser &parser, Located<Tokens::Mut> const &a0, Located<Tokens::Identifier> const &a1, std::unique_ptr<ASTNS::Type> const &a2, Located<Tokens::Equal> const &a3);
static RetAndToken<std::unique_ptr<ASTNS::IfExpr>> state_240(Parser &parser, Located<Tokens::If> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, std::unique_ptr<ASTNS::Block> const &a2, Located<Tokens::Else> const &a3, std::unique_ptr<ASTNS::Block> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::IfExpr>> state_241(Parser &parser, Located<Tokens::If> const &a0, std::unique_ptr<ASTNS::Expr> const &a1, std::unique_ptr<ASTNS::Block> const &a2, Located<Tokens::Else> const &a3, std::unique_ptr<ASTNS::IfExpr> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::ArgList>> state_242(Parser &parser, std::unique_ptr<ASTNS::ArgList> const &a0, Located<Tokens::Comma> const &a1, std::unique_ptr<ASTNS::Arg> const &a2);
static RetAndToken<std::unique_ptr<ASTNS::Arg>> state_243(Parser &parser, std::unique_ptr<ASTNS::Arg> const &a0);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_244(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3, std::unique_ptr<ASTNS::ArgList> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_245(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3, std::unique_ptr<ASTNS::ArgList> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_246(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3, std::unique_ptr<ASTNS::ArgList> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>> state_247(Parser &parser, Located<Tokens::Mut> const &a0, Located<Tokens::Identifier> const &a1, std::unique_ptr<ASTNS::Type> const &a2, Located<Tokens::Equal> const &a3, std::unique_ptr<ASTNS::Expr> const &a4);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_248(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3, std::unique_ptr<ASTNS::ArgList> const &a4, Located<Tokens::CParen> const &a5);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_249(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3, std::unique_ptr<ASTNS::ArgList> const &a4, Located<Tokens::CParen> const &a5);
static RetAndToken<std::unique_ptr<ASTNS::Expr>> state_250(Parser &parser, std::unique_ptr<ASTNS::Expr> const &a0, Located<Tokens::Period> const &a1, Located<Tokens::Identifier> const &a2, Located<Tokens::OParen> const &a3, std::unique_ptr<ASTNS::ArgList> const &a4, Located<Tokens::CParen> const &a5);

static RetAndToken<std::unique_ptr<ASTNS::CUB>>
    state_0 (
        Parser &parser
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, downcasted_token);
        }
        case Tokens::index_of<Tokens::Impl>:
        {
            Located<Tokens::Impl> downcasted_token { next_token.span, Tokens::as<Tokens::Impl>(next_token.value) };
            return state_7(parser, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::CU> pushitem = nullptr;
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::CUB>>
    state_1 (
        Parser &parser,
        std::unique_ptr<ASTNS::CU> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::_EOF>:
        {
            accept(); // somehow
        }
    }
}

static RetAndToken<std::variant<std::unique_ptr<ASTNS::CU>, std::unique_ptr<ASTNS::DeclList>>>
    state_2 (
        Parser &parser,
        std::unique_ptr<ASTNS::DeclList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Impl>:
        {
            Located<Tokens::Impl> downcasted_token { next_token.span, Tokens::as<Tokens::Impl>(next_token.value) };
            return state_7(parser, a0, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::CU> push (std::make_unique<ASTNS::CU>(p.sourcefile, span, std::move(a0->decls)));
            std::unique_ptr<ASTNS::CU> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::DeclList>>
    state_3 (
        Parser &parser,
        std::unique_ptr<ASTNS::Decl> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::DeclList> push (std::make_unique<ASTNS::DeclList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Decl>> {}));
            push->decls.push_back(std::move(a0));
            std::unique_ptr<ASTNS::DeclList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_4 (
        Parser &parser,
        std::unique_ptr<ASTNS::FunctionDecl> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_5 (
        Parser &parser,
        std::unique_ptr<ASTNS::Decl> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_6 (
        Parser &parser,
        Located<Tokens::Fun> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_10(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_7 (
        Parser &parser,
        Located<Tokens::Impl> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_16(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_17(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::DeclList>>
    state_8 (
        Parser &parser,
        std::unique_ptr<ASTNS::DeclList> const &a0,
        std::unique_ptr<ASTNS::Decl> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->decls.push_back(std::move(a1));
            std::unique_ptr<ASTNS::DeclList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_9 (
        Parser &parser,
        std::unique_ptr<ASTNS::Decl> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_10 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_19(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_11 (
        Parser &parser,
        Located<Tokens::Impl> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_22(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_21(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Type>>
    state_12 (
        Parser &parser,
        std::unique_ptr<ASTNS::PathType> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Type>>
    state_13 (
        Parser &parser,
        std::unique_ptr<ASTNS::PointerType> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Type>>
    state_14 (
        Parser &parser,
        std::unique_ptr<ASTNS::ThisType> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::variant<std::unique_ptr<ASTNS::Path>, std::unique_ptr<ASTNS::PathType>>>
    state_15 (
        Parser &parser,
        std::unique_ptr<ASTNS::Path> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PathType> push (std::make_unique<ASTNS::PathType>(p.sourcefile, span, std::move(a0)));
            std::unique_ptr<ASTNS::PathType> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleColon>:
        {
            Located<Tokens::DoubleColon> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleColon>(next_token.value) };
            return state_23(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PointerType>>
    state_16 (
        Parser &parser,
        Located<Tokens::Star> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_25(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_16(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_17(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ThisType>>
    state_17 (
        Parser &parser,
        Located<Tokens::This> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ThisType> push (std::make_unique<ASTNS::ThisType>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::ThisType> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Path>>
    state_18 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Path> push (std::make_unique<ASTNS::Path>(p.sourcefile, span, std::vector<Token> {}));
            push->segments.push_back(a0);
            std::unique_ptr<ASTNS::Path> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_19 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamList> push (std::make_unique<ASTNS::ParamList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ParamB>> {}));
            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_32(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_33(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_35(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_34(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_20 (
        Parser &parser,
        Located<Tokens::Impl> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1,
        std::unique_ptr<ASTNS::ImplMemberList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        case Tokens::index_of<Tokens::Bang>:
        case Tokens::index_of<Tokens::BoolLit>:
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::CharLit>:
        case Tokens::index_of<Tokens::Dedent>:
        case Tokens::index_of<Tokens::FloatLit>:
        case Tokens::index_of<Tokens::Fun>:
        case Tokens::index_of<Tokens::Identifier>:
        case Tokens::index_of<Tokens::If>:
        case Tokens::index_of<Tokens::Impl>:
        case Tokens::index_of<Tokens::IntLit>:
        case Tokens::index_of<Tokens::Minus>:
        case Tokens::index_of<Tokens::OBrace>:
        case Tokens::index_of<Tokens::OParen>:
        case Tokens::index_of<Tokens::Return>:
        case Tokens::index_of<Tokens::Star>:
        case Tokens::index_of<Tokens::StringLit>:
        case Tokens::index_of<Tokens::This>:
        case Tokens::index_of<Tokens::Tilde>:
        case Tokens::index_of<Tokens::Var>:
        case Tokens::index_of<Tokens::While>:
        case Tokens::index_of<Tokens::_EOF>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_21 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_41(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_22 (
        Parser &parser,
        Located<Tokens::Newline> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Indent>:
        {
            Located<Tokens::Indent> downcasted_token { next_token.span, Tokens::as<Tokens::Indent>(next_token.value) };
            return state_45(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Path>>
    state_23 (
        Parser &parser,
        std::unique_ptr<ASTNS::Path> const &a0,
        Located<Tokens::DoubleColon> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_46(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PointerType>>
    state_24 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PointerType> push (std::make_unique<ASTNS::PointerType>(p.sourcefile, span, false, std::move(a1)));
            std::unique_ptr<ASTNS::PointerType> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PointerType>>
    state_25 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        Located<Tokens::Mut> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_16(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_17(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_26 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2,
        std::unique_ptr<ASTNS::ParamList> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_48(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamList>>
    state_27 (
        Parser &parser,
        std::unique_ptr<ASTNS::ParamList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamList>>
    state_28 (
        Parser &parser,
        std::unique_ptr<ASTNS::ParamList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Comma>:
        {
            Located<Tokens::Comma> downcasted_token { next_token.span, Tokens::as<Tokens::Comma>(next_token.value) };
            return state_49(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamList>>
    state_29 (
        Parser &parser,
        std::unique_ptr<ASTNS::ParamB> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamList> push (std::make_unique<ASTNS::ParamList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ParamB>> {}));
            push->params.push_back(std::move(a0));
            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamB>>
    state_30 (
        Parser &parser,
        std::unique_ptr<ASTNS::Param> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamB>>
    state_31 (
        Parser &parser,
        std::unique_ptr<ASTNS::ThisParam> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Param>>
    state_32 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Colon>:
        {
            Located<Tokens::Colon> downcasted_token { next_token.span, Tokens::as<Tokens::Colon>(next_token.value) };
            return state_51(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Param>>
    state_33 (
        Parser &parser,
        Located<Tokens::Mut> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_52(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ThisParam>>
    state_34 (
        Parser &parser,
        Located<Tokens::This> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(p.sourcefile, span, false, false));
            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ThisParam>>
    state_35 (
        Parser &parser,
        Located<Tokens::Star> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_54(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_53(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Decl>>
    state_36 (
        Parser &parser,
        Located<Tokens::Impl> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1,
        std::unique_ptr<ASTNS::ImplMemberList> const &a2,
        std::unique_ptr<ASTNS::PureLocation> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplDecl> push (std::make_unique<ASTNS::ImplDecl>(p.sourcefile, span, std::move(a1), std::move(a2->members)));
            std::unique_ptr<ASTNS::Decl> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PureLocation>>
    state_37 (
        Parser &parser,
        std::unique_ptr<ASTNS::PureLocation> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PureLocation>>
    state_38 (
        Parser &parser,
        Located<Tokens::Newline> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PureLocation>>
    state_39 (
        Parser &parser,
        Located<Tokens::Semicolon> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_55(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_40 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        std::unique_ptr<ASTNS::ImplMemberList> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        {
            Located<Tokens::CBrace> downcasted_token { next_token.span, Tokens::as<Tokens::CBrace>(next_token.value) };
            return state_56(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_41 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Indent>:
        {
            Located<Tokens::Indent> downcasted_token { next_token.span, Tokens::as<Tokens::Indent>(next_token.value) };
            return state_58(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_42 (
        Parser &parser,
        std::unique_ptr<ASTNS::ImplMemberList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_43 (
        Parser &parser,
        std::unique_ptr<ASTNS::ImplMember> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
            push->members.push_back(std::move(a0));
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMember>>
    state_44 (
        Parser &parser,
        std::unique_ptr<ASTNS::FunctionDecl> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::FunctionImplMember> push (std::make_unique<ASTNS::FunctionImplMember>(p.sourcefile, span, std::move(a0)));
            std::unique_ptr<ASTNS::ImplMember> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_45 (
        Parser &parser,
        Located<Tokens::Newline> const &a0,
        Located<Tokens::Indent> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Path>>
    state_46 (
        Parser &parser,
        std::unique_ptr<ASTNS::Path> const &a0,
        Located<Tokens::DoubleColon> const &a1,
        Located<Tokens::Identifier> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->segments.push_back(a2);
            std::unique_ptr<ASTNS::Path> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PointerType>>
    state_47 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        Located<Tokens::Mut> const &a1,
        std::unique_ptr<ASTNS::Type> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PointerType> push (std::make_unique<ASTNS::PointerType>(p.sourcefile, span, true, std::move(a2)));
            std::unique_ptr<ASTNS::PointerType> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_48 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2,
        std::unique_ptr<ASTNS::ParamList> const &a3,
        Located<Tokens::CParen> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Colon>:
        {
            Located<Tokens::Colon> downcasted_token { next_token.span, Tokens::as<Tokens::Colon>(next_token.value) };
            return state_51(parser, a0, a1, a2, a3, a4, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamList>>
    state_49 (
        Parser &parser,
        std::unique_ptr<ASTNS::ParamList> const &a0,
        Located<Tokens::Comma> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_32(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_33(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_35(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_34(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Param>>
    state_50 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Param> push (std::make_unique<ASTNS::Param>(p.sourcefile, span, std::move(a1), a0, false));
            std::unique_ptr<ASTNS::Param> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Type>>
    state_51 (
        Parser &parser,
        Located<Tokens::Colon> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_16(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_17(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Param>>
    state_52 (
        Parser &parser,
        Located<Tokens::Mut> const &a0,
        Located<Tokens::Identifier> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Colon>:
        {
            Located<Tokens::Colon> downcasted_token { next_token.span, Tokens::as<Tokens::Colon>(next_token.value) };
            return state_51(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ThisParam>>
    state_53 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        Located<Tokens::This> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(p.sourcefile, span, true, false));
            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ThisParam>>
    state_54 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        Located<Tokens::Mut> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_67(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::PureLocation>>
    state_55 (
        Parser &parser,
        Located<Tokens::Semicolon> const &a0,
        Located<Tokens::Newline> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            WARN_EXTRA_SEMI(a0);std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_56 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        std::unique_ptr<ASTNS::ImplMemberList> const &a1,
        Located<Tokens::CBrace> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a1);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_57 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        std::unique_ptr<ASTNS::ImplMemberList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        {
            Located<Tokens::CBrace> downcasted_token { next_token.span, Tokens::as<Tokens::CBrace>(next_token.value) };
            return state_68(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_58 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Fun>:
        {
            Located<Tokens::Fun> downcasted_token { next_token.span, Tokens::as<Tokens::Fun>(next_token.value) };
            return state_6(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_59 (
        Parser &parser,
        std::unique_ptr<ASTNS::ImplMemberList> const &a0,
        std::unique_ptr<ASTNS::ImplMember> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->members.push_back(std::move(a1));
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMember>>
    state_60 (
        Parser &parser,
        std::unique_ptr<ASTNS::ImplMember> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMember> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_61 (
        Parser &parser,
        Located<Tokens::Newline> const &a0,
        Located<Tokens::Indent> const &a1,
        std::unique_ptr<ASTNS::ImplMemberList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Dedent>:
        {
            Located<Tokens::Dedent> downcasted_token { next_token.span, Tokens::as<Tokens::Dedent>(next_token.value) };
            return state_70(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_62 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2,
        std::unique_ptr<ASTNS::ParamList> const &a3,
        Located<Tokens::CParen> const &a4,
        std::unique_ptr<ASTNS::Type> const &a5
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_75(parser, a0, a1, a2, a3, a4, a5, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, a3, a4, a5, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, a2, a3, a4, a5, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamList>>
    state_63 (
        Parser &parser,
        std::unique_ptr<ASTNS::ParamList> const &a0,
        Located<Tokens::Comma> const &a1,
        std::unique_ptr<ASTNS::ParamB> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->params.push_back(std::move(a2));
            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ParamB>>
    state_64 (
        Parser &parser,
        std::unique_ptr<ASTNS::ParamB> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Type>>
    state_65 (
        Parser &parser,
        Located<Tokens::Colon> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Type> pushitem = std::move(a1);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Param>>
    state_66 (
        Parser &parser,
        Located<Tokens::Mut> const &a0,
        Located<Tokens::Identifier> const &a1,
        std::unique_ptr<ASTNS::Type> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Param> push (std::make_unique<ASTNS::Param>(p.sourcefile, span, std::move(a2), a1, true));
            std::unique_ptr<ASTNS::Param> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ThisParam>>
    state_67 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        Located<Tokens::Mut> const &a1,
        Located<Tokens::This> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(p.sourcefile, span, true, true));
            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_68 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        std::unique_ptr<ASTNS::ImplMemberList> const &a2,
        Located<Tokens::CBrace> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            WARN_BLOCK_NO_INDENT(a0, a3);
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a2);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_69 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2,
        std::unique_ptr<ASTNS::ImplMemberList> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Dedent>:
        {
            Located<Tokens::Dedent> downcasted_token { next_token.span, Tokens::as<Tokens::Dedent>(next_token.value) };
            return state_77(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_70 (
        Parser &parser,
        Located<Tokens::Newline> const &a0,
        Located<Tokens::Indent> const &a1,
        std::unique_ptr<ASTNS::ImplMemberList> const &a2,
        Located<Tokens::Dedent> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a2);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_71 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2,
        std::unique_ptr<ASTNS::ParamList> const &a3,
        Located<Tokens::CParen> const &a4,
        std::unique_ptr<ASTNS::Type> const &a5,
        std::unique_ptr<ASTNS::Block> const &a6
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        case Tokens::index_of<Tokens::Bang>:
        case Tokens::index_of<Tokens::BoolLit>:
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::CharLit>:
        case Tokens::index_of<Tokens::Dedent>:
        case Tokens::index_of<Tokens::FloatLit>:
        case Tokens::index_of<Tokens::Fun>:
        case Tokens::index_of<Tokens::Identifier>:
        case Tokens::index_of<Tokens::If>:
        case Tokens::index_of<Tokens::Impl>:
        case Tokens::index_of<Tokens::IntLit>:
        case Tokens::index_of<Tokens::Minus>:
        case Tokens::index_of<Tokens::OBrace>:
        case Tokens::index_of<Tokens::OParen>:
        case Tokens::index_of<Tokens::Return>:
        case Tokens::index_of<Tokens::Star>:
        case Tokens::index_of<Tokens::StringLit>:
        case Tokens::index_of<Tokens::This>:
        case Tokens::index_of<Tokens::Tilde>:
        case Tokens::index_of<Tokens::Var>:
        case Tokens::index_of<Tokens::While>:
        case Tokens::index_of<Tokens::_EOF>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, a1, a2, a3, a4, a5, a6, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, a2, a3, a4, a5, a6, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_72 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2,
        std::unique_ptr<ASTNS::ParamList> const &a3,
        Located<Tokens::CParen> const &a4,
        std::unique_ptr<ASTNS::Type> const &a5,
        std::unique_ptr<ASTNS::PureLocation> const &a6
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a6 && a6->span().has() ? Maybe<Location const>(a6->span().get().end) :
                a5 && a5->span().has() ? Maybe<Location const>(a5->span().get().end) :
            Maybe<Location const>(a4.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::FunctionDecl> push (std::make_unique<ASTNS::FunctionDecl>(p.sourcefile, span, std::move(a5), a1, std::move(a3->params), nullptr));
            std::unique_ptr<ASTNS::FunctionDecl> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_73 (
        Parser &parser,
        std::unique_ptr<ASTNS::Block> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Block> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_74 (
        Parser &parser,
        std::unique_ptr<ASTNS::Block> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Block> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::variant<std::unique_ptr<ASTNS::Block>, std::unique_ptr<ASTNS::PureLocation>>>
    state_75 (
        Parser &parser,
        Located<Tokens::Newline> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(p.sourcefile, span, 0));
            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Indent>:
        {
            Located<Tokens::Indent> downcasted_token { next_token.span, Tokens::as<Tokens::Indent>(next_token.value) };
            return state_79(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_76 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_81(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Return>:
        {
            Located<Tokens::Return> downcasted_token { next_token.span, Tokens::as<Tokens::Return>(next_token.value) };
            return state_90(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Var>:
        {
            Located<Tokens::Var> downcasted_token { next_token.span, Tokens::as<Tokens::Var>(next_token.value) };
            return state_87(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_77 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2,
        std::unique_ptr<ASTNS::ImplMemberList> const &a3,
        Located<Tokens::Dedent> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        {
            Located<Tokens::CBrace> downcasted_token { next_token.span, Tokens::as<Tokens::CBrace>(next_token.value) };
            return state_127(parser, a0, a1, a2, a3, a4, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::FunctionDecl>>
    state_78 (
        Parser &parser,
        Located<Tokens::Fun> const &a0,
        Located<Tokens::Identifier> const &a1,
        Located<Tokens::OParen> const &a2,
        std::unique_ptr<ASTNS::ParamList> const &a3,
        Located<Tokens::CParen> const &a4,
        std::unique_ptr<ASTNS::Type> const &a5,
        std::unique_ptr<ASTNS::Block> const &a6,
        std::unique_ptr<ASTNS::PureLocation> const &a7
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a5 && a5->span().has() ? Maybe<Location const>(a5->span().get().end) :
            Maybe<Location const>(a4.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::FunctionDecl> push (std::make_unique<ASTNS::FunctionDecl>(p.sourcefile, span, std::move(a5), a1, std::move(a3->params), std::move(a6)));
            std::unique_ptr<ASTNS::FunctionDecl> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_79 (
        Parser &parser,
        Located<Tokens::Newline> const &a0,
        Located<Tokens::Indent> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Return>:
        {
            Located<Tokens::Return> downcasted_token { next_token.span, Tokens::as<Tokens::Return>(next_token.value) };
            return state_90(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Var>:
        {
            Located<Tokens::Var> downcasted_token { next_token.span, Tokens::as<Tokens::Var>(next_token.value) };
            return state_87(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_80 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        std::unique_ptr<ASTNS::StmtList> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        {
            Located<Tokens::CBrace> downcasted_token { next_token.span, Tokens::as<Tokens::CBrace>(next_token.value) };
            return state_129(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_81 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Indent>:
        {
            Located<Tokens::Indent> downcasted_token { next_token.span, Tokens::as<Tokens::Indent>(next_token.value) };
            return state_131(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Return>:
        {
            Located<Tokens::Return> downcasted_token { next_token.span, Tokens::as<Tokens::Return>(next_token.value) };
            return state_90(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Var>:
        {
            Located<Tokens::Var> downcasted_token { next_token.span, Tokens::as<Tokens::Var>(next_token.value) };
            return state_87(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::StmtList>>
    state_82 (
        Parser &parser,
        std::unique_ptr<ASTNS::StmtList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Return>:
        {
            Located<Tokens::Return> downcasted_token { next_token.span, Tokens::as<Tokens::Return>(next_token.value) };
            return state_90(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Var>:
        {
            Located<Tokens::Var> downcasted_token { next_token.span, Tokens::as<Tokens::Var>(next_token.value) };
            return state_87(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::StmtList>>
    state_83 (
        Parser &parser,
        std::unique_ptr<ASTNS::Stmt> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
            push->stmts.push_back(std::move(a0));
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Stmt>>
    state_84 (
        Parser &parser,
        std::unique_ptr<ASTNS::VarStmt> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Stmt>>
    state_85 (
        Parser &parser,
        std::unique_ptr<ASTNS::ExprStmt> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Stmt>>
    state_86 (
        Parser &parser,
        std::unique_ptr<ASTNS::RetStmt> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmt>>
    state_87 (
        Parser &parser,
        Located<Tokens::Var> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_137(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_138(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_88 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Dollar>:
        {
            Located<Tokens::Dollar> downcasted_token { next_token.span, Tokens::as<Tokens::Dollar>(next_token.value) };
            return state_140(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_89 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        case Tokens::index_of<Tokens::Bang>:
        case Tokens::index_of<Tokens::BoolLit>:
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::CharLit>:
        case Tokens::index_of<Tokens::Dedent>:
        case Tokens::index_of<Tokens::FloatLit>:
        case Tokens::index_of<Tokens::Fun>:
        case Tokens::index_of<Tokens::Identifier>:
        case Tokens::index_of<Tokens::If>:
        case Tokens::index_of<Tokens::Impl>:
        case Tokens::index_of<Tokens::IntLit>:
        case Tokens::index_of<Tokens::Minus>:
        case Tokens::index_of<Tokens::OBrace>:
        case Tokens::index_of<Tokens::OParen>:
        case Tokens::index_of<Tokens::Return>:
        case Tokens::index_of<Tokens::Star>:
        case Tokens::index_of<Tokens::StringLit>:
        case Tokens::index_of<Tokens::This>:
        case Tokens::index_of<Tokens::Tilde>:
        case Tokens::index_of<Tokens::Var>:
        case Tokens::index_of<Tokens::While>:
        case Tokens::index_of<Tokens::_EOF>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Dollar>:
        {
            Located<Tokens::Dollar> downcasted_token { next_token.span, Tokens::as<Tokens::Dollar>(next_token.value) };
            return state_142(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::RetStmt>>
    state_90 (
        Parser &parser,
        Located<Tokens::Return> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_91 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_92 (
        Parser &parser,
        std::unique_ptr<ASTNS::IfExpr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_93 (
        Parser &parser,
        std::unique_ptr<ASTNS::WhileExpr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_94 (
        Parser &parser,
        std::unique_ptr<ASTNS::Block> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_95 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoublePipe>:
        {
            Located<Tokens::DoublePipe> downcasted_token { next_token.span, Tokens::as<Tokens::DoublePipe>(next_token.value) };
            return state_148(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Equal>:
        {
            Located<Tokens::Equal> downcasted_token { next_token.span, Tokens::as<Tokens::Equal>(next_token.value) };
            return state_147(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::IfExpr>>
    state_96 (
        Parser &parser,
        Located<Tokens::If> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::WhileExpr>>
    state_97 (
        Parser &parser,
        Located<Tokens::While> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_98 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleAmper>:
        {
            Located<Tokens::DoubleAmper> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleAmper>(next_token.value) };
            return state_151(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_99 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::BangEqual>:
        {
            Located<Tokens::BangEqual> downcasted_token { next_token.span, Tokens::as<Tokens::BangEqual>(next_token.value) };
            return state_152(parser, a0, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleEqual>:
        {
            Located<Tokens::DoubleEqual> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleEqual>(next_token.value) };
            return state_153(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_100 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Greater>:
        {
            Located<Tokens::Greater> downcasted_token { next_token.span, Tokens::as<Tokens::Greater>(next_token.value) };
            return state_155(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::GreaterEqual>:
        {
            Located<Tokens::GreaterEqual> downcasted_token { next_token.span, Tokens::as<Tokens::GreaterEqual>(next_token.value) };
            return state_157(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Less>:
        {
            Located<Tokens::Less> downcasted_token { next_token.span, Tokens::as<Tokens::Less>(next_token.value) };
            return state_154(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::LessEqual>:
        {
            Located<Tokens::LessEqual> downcasted_token { next_token.span, Tokens::as<Tokens::LessEqual>(next_token.value) };
            return state_156(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_101 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Caret>:
        {
            Located<Tokens::Caret> downcasted_token { next_token.span, Tokens::as<Tokens::Caret>(next_token.value) };
            return state_158(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_102 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Pipe>:
        {
            Located<Tokens::Pipe> downcasted_token { next_token.span, Tokens::as<Tokens::Pipe>(next_token.value) };
            return state_159(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_103 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_160(parser, a0, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_104 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleGreater>:
        {
            Located<Tokens::DoubleGreater> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleGreater>(next_token.value) };
            return state_161(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::DoubleLess>:
        {
            Located<Tokens::DoubleLess> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleLess>(next_token.value) };
            return state_162(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_105 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_164(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Plus>:
        {
            Located<Tokens::Plus> downcasted_token { next_token.span, Tokens::as<Tokens::Plus>(next_token.value) };
            return state_163(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_106 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Percent>:
        {
            Located<Tokens::Percent> downcasted_token { next_token.span, Tokens::as<Tokens::Percent>(next_token.value) };
            return state_167(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Slash>:
        {
            Located<Tokens::Slash> downcasted_token { next_token.span, Tokens::as<Tokens::Slash>(next_token.value) };
            return state_166(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_165(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_107 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::RightArrow>:
        {
            Located<Tokens::RightArrow> downcasted_token { next_token.span, Tokens::as<Tokens::RightArrow>(next_token.value) };
            return state_168(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_108 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_109 (
        Parser &parser,
        Located<Tokens::Tilde> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_110 (
        Parser &parser,
        Located<Tokens::Minus> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_111 (
        Parser &parser,
        Located<Tokens::Bang> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_112 (
        Parser &parser,
        Located<Tokens::Amper> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_173(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_113 (
        Parser &parser,
        Located<Tokens::Star> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_114 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_175(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Period>:
        {
            Located<Tokens::Period> downcasted_token { next_token.span, Tokens::as<Tokens::Period>(next_token.value) };
            return state_176(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_115 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Period>:
        {
            Located<Tokens::Period> downcasted_token { next_token.span, Tokens::as<Tokens::Period>(next_token.value) };
            return state_177(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_116 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_178(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Period>:
        {
            Located<Tokens::Period> downcasted_token { next_token.span, Tokens::as<Tokens::Period>(next_token.value) };
            return state_179(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_117 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_118 (
        Parser &parser,
        Located<Tokens::BoolLit> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_119 (
        Parser &parser,
        Located<Tokens::FloatLit> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_120 (
        Parser &parser,
        Located<Tokens::IntLit> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_121 (
        Parser &parser,
        Located<Tokens::CharLit> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_122 (
        Parser &parser,
        Located<Tokens::StringLit> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_123 (
        Parser &parser,
        Located<Tokens::This> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PrimaryExpr> push (std::make_unique<ASTNS::PrimaryExpr>(p.sourcefile, span, a0));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_124 (
        Parser &parser,
        Located<Tokens::OParen> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_125 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::variant<std::unique_ptr<ASTNS::Expr>, std::unique_ptr<ASTNS::Path>>>
    state_126 (
        Parser &parser,
        std::unique_ptr<ASTNS::Path> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PathExpr> push (std::make_unique<ASTNS::PathExpr>(p.sourcefile, span, std::move(a0)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleColon>:
        {
            Located<Tokens::DoubleColon> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleColon>(next_token.value) };
            return state_23(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ImplMemberList>>
    state_127 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2,
        std::unique_ptr<ASTNS::ImplMemberList> const &a3,
        Located<Tokens::Dedent> const &a4,
        Located<Tokens::CBrace> const &a5
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a5.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a3);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_128 (
        Parser &parser,
        Located<Tokens::Newline> const &a0,
        Located<Tokens::Indent> const &a1,
        std::unique_ptr<ASTNS::StmtList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Dedent>:
        {
            Located<Tokens::Dedent> downcasted_token { next_token.span, Tokens::as<Tokens::Dedent>(next_token.value) };
            return state_181(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_129 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        std::unique_ptr<ASTNS::StmtList> const &a1,
        Located<Tokens::CBrace> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a1->stmts)));
            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_130 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        std::unique_ptr<ASTNS::StmtList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        {
            Located<Tokens::CBrace> downcasted_token { next_token.span, Tokens::as<Tokens::CBrace>(next_token.value) };
            return state_182(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_131 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::Dedent>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Return>:
        {
            Located<Tokens::Return> downcasted_token { next_token.span, Tokens::as<Tokens::Return>(next_token.value) };
            return state_90(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Var>:
        {
            Located<Tokens::Var> downcasted_token { next_token.span, Tokens::as<Tokens::Var>(next_token.value) };
            return state_87(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::StmtList>>
    state_132 (
        Parser &parser,
        std::unique_ptr<ASTNS::StmtList> const &a0,
        std::unique_ptr<ASTNS::Stmt> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->stmts.push_back(std::move(a1));
            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Stmt>>
    state_133 (
        Parser &parser,
        std::unique_ptr<ASTNS::Stmt> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmt>>
    state_134 (
        Parser &parser,
        Located<Tokens::Var> const &a0,
        std::unique_ptr<ASTNS::VarStmtItemList> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>>
    state_135 (
        Parser &parser,
        std::unique_ptr<ASTNS::VarStmtItemList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Comma>:
        {
            Located<Tokens::Comma> downcasted_token { next_token.span, Tokens::as<Tokens::Comma>(next_token.value) };
            return state_185(parser, a0, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>>
    state_136 (
        Parser &parser,
        std::unique_ptr<ASTNS::VarStmtItem> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItemList> push (std::make_unique<ASTNS::VarStmtItemList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::VarStmtItem>> {}));
            push->items.push_back(std::move(a0));
            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_137 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Colon>:
        {
            Located<Tokens::Colon> downcasted_token { next_token.span, Tokens::as<Tokens::Colon>(next_token.value) };
            return state_51(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_138 (
        Parser &parser,
        Located<Tokens::Mut> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_187(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_139 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        std::unique_ptr<ASTNS::PureLocation> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), false, Maybe<Span const>()));
            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_140 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Dollar> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        case Tokens::index_of<Tokens::Bang>:
        case Tokens::index_of<Tokens::BoolLit>:
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::CharLit>:
        case Tokens::index_of<Tokens::Dedent>:
        case Tokens::index_of<Tokens::FloatLit>:
        case Tokens::index_of<Tokens::Fun>:
        case Tokens::index_of<Tokens::Identifier>:
        case Tokens::index_of<Tokens::If>:
        case Tokens::index_of<Tokens::Impl>:
        case Tokens::index_of<Tokens::IntLit>:
        case Tokens::index_of<Tokens::Minus>:
        case Tokens::index_of<Tokens::OBrace>:
        case Tokens::index_of<Tokens::OParen>:
        case Tokens::index_of<Tokens::Return>:
        case Tokens::index_of<Tokens::Star>:
        case Tokens::index_of<Tokens::StringLit>:
        case Tokens::index_of<Tokens::This>:
        case Tokens::index_of<Tokens::Tilde>:
        case Tokens::index_of<Tokens::Var>:
        case Tokens::index_of<Tokens::While>:
        case Tokens::index_of<Tokens::_EOF>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_141 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        std::unique_ptr<ASTNS::PureLocation> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), false, Maybe<Span const>()));
            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_142 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Dollar> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        case Tokens::index_of<Tokens::Bang>:
        case Tokens::index_of<Tokens::BoolLit>:
        case Tokens::index_of<Tokens::CBrace>:
        case Tokens::index_of<Tokens::CharLit>:
        case Tokens::index_of<Tokens::Dedent>:
        case Tokens::index_of<Tokens::FloatLit>:
        case Tokens::index_of<Tokens::Fun>:
        case Tokens::index_of<Tokens::Identifier>:
        case Tokens::index_of<Tokens::If>:
        case Tokens::index_of<Tokens::Impl>:
        case Tokens::index_of<Tokens::IntLit>:
        case Tokens::index_of<Tokens::Minus>:
        case Tokens::index_of<Tokens::OBrace>:
        case Tokens::index_of<Tokens::OParen>:
        case Tokens::index_of<Tokens::Return>:
        case Tokens::index_of<Tokens::Star>:
        case Tokens::index_of<Tokens::StringLit>:
        case Tokens::index_of<Tokens::This>:
        case Tokens::index_of<Tokens::Tilde>:
        case Tokens::index_of<Tokens::Var>:
        case Tokens::index_of<Tokens::While>:
        case Tokens::index_of<Tokens::_EOF>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::PureLocation> pushitem = nullptr;
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::RetStmt>>
    state_143 (
        Parser &parser,
        Located<Tokens::Return> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_38(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Semicolon>:
        {
            Located<Tokens::Semicolon> downcasted_token { next_token.span, Tokens::as<Tokens::Semicolon>(next_token.value) };
            return state_39(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::RetStmt>>
    state_144 (
        Parser &parser,
        Located<Tokens::Return> const &a0,
        std::unique_ptr<ASTNS::PureLocation> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::RetStmt> push (std::make_unique<ASTNS::RetStmt>(p.sourcefile, span, nullptr));
            std::unique_ptr<ASTNS::RetStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_145 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_146 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_147 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Equal> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_148 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoublePipe> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::IfExpr>>
    state_149 (
        Parser &parser,
        Located<Tokens::If> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_194(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::WhileExpr>>
    state_150 (
        Parser &parser,
        Located<Tokens::While> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_194(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_151 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleAmper> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_152 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::BangEqual> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_153 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleEqual> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_154 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Less> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_155 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Greater> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_156 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::LessEqual> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_157 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::GreaterEqual> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_158 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Caret> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_159 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Pipe> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_160 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Amper> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_161 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleGreater> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_162 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleLess> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_163 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Plus> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_164 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Minus> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_165 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Star> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_166 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Slash> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_167 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Percent> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_168 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::RightArrow> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_16(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_17(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_169 (
        Parser &parser,
        Located<Tokens::Tilde> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(p.sourcefile, span, a0, std::move(a1)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_170 (
        Parser &parser,
        Located<Tokens::Minus> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(p.sourcefile, span, a0, std::move(a1)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_171 (
        Parser &parser,
        Located<Tokens::Bang> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(p.sourcefile, span, a0, std::move(a1)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_172 (
        Parser &parser,
        Located<Tokens::Amper> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::AddrofExpr> push (std::make_unique<ASTNS::AddrofExpr>(p.sourcefile, span, a0, std::move(a1), false));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_173 (
        Parser &parser,
        Located<Tokens::Amper> const &a0,
        Located<Tokens::Mut> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_174 (
        Parser &parser,
        Located<Tokens::Star> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::DerefExpr> push (std::make_unique<ASTNS::DerefExpr>(p.sourcefile, span, a0, std::move(a1)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_175 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::OParen> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CParen>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_176 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_220(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_177 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_221(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_178 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::OParen> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::CParen>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_179 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_223(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_180 (
        Parser &parser,
        Located<Tokens::OParen> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_224(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_181 (
        Parser &parser,
        Located<Tokens::Newline> const &a0,
        Located<Tokens::Indent> const &a1,
        std::unique_ptr<ASTNS::StmtList> const &a2,
        Located<Tokens::Dedent> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a2->stmts)));
            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_182 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        std::unique_ptr<ASTNS::StmtList> const &a2,
        Located<Tokens::CBrace> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            WARN_BLOCK_NO_INDENT(a0, a3);std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a2->stmts)));
            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_183 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2,
        std::unique_ptr<ASTNS::StmtList> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Dedent>:
        {
            Located<Tokens::Dedent> downcasted_token { next_token.span, Tokens::as<Tokens::Dedent>(next_token.value) };
            return state_225(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmt>>
    state_184 (
        Parser &parser,
        Located<Tokens::Var> const &a0,
        std::unique_ptr<ASTNS::VarStmtItemList> const &a1,
        std::unique_ptr<ASTNS::PureLocation> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmt> push (std::make_unique<ASTNS::VarStmt>(p.sourcefile, span, std::move(a1->items)));
            std::unique_ptr<ASTNS::VarStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>>
    state_185 (
        Parser &parser,
        std::unique_ptr<ASTNS::VarStmtItemList> const &a0,
        Located<Tokens::Comma> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_137(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Mut>:
        {
            Located<Tokens::Mut> downcasted_token { next_token.span, Tokens::as<Tokens::Mut>(next_token.value) };
            return state_138(parser, a0, a1, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_186 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a1), false, a0, a0, nullptr));
            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Equal>:
        {
            Located<Tokens::Equal> downcasted_token { next_token.span, Tokens::as<Tokens::Equal>(next_token.value) };
            return state_228(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_187 (
        Parser &parser,
        Located<Tokens::Mut> const &a0,
        Located<Tokens::Identifier> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Colon>:
        {
            Located<Tokens::Colon> downcasted_token { next_token.span, Tokens::as<Tokens::Colon>(next_token.value) };
            return state_51(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_188 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Dollar> const &a1,
        std::unique_ptr<ASTNS::PureLocation> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), true , Maybe<Span const>(a1.span)));
            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ExprStmt>>
    state_189 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Dollar> const &a1,
        std::unique_ptr<ASTNS::PureLocation> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(p.sourcefile, span, std::move(a0), true , Maybe<Span const>(a1.span)));
            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::RetStmt>>
    state_190 (
        Parser &parser,
        Located<Tokens::Return> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        std::unique_ptr<ASTNS::PureLocation> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::RetStmt> push (std::make_unique<ASTNS::RetStmt>(p.sourcefile, span, std::move(a1)));
            std::unique_ptr<ASTNS::RetStmt> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_191 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Equal> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::AssignmentExpr> push (std::make_unique<ASTNS::AssignmentExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_192 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoublePipe> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ShortCircuitExpr> push (std::make_unique<ASTNS::ShortCircuitExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleAmper>:
        {
            Located<Tokens::DoubleAmper> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleAmper>(next_token.value) };
            return state_151(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::IfExpr>>
    state_193 (
        Parser &parser,
        Located<Tokens::If> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        std::unique_ptr<ASTNS::Block> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(p.sourcefile, span, a0, a0, std::move(a1), std::move(a2), nullptr));
            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Else>:
        {
            Located<Tokens::Else> downcasted_token { next_token.span, Tokens::as<Tokens::Else>(next_token.value) };
            return state_230(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_194 (
        Parser &parser,
        Located<Tokens::Newline> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Indent>:
        {
            Located<Tokens::Indent> downcasted_token { next_token.span, Tokens::as<Tokens::Indent>(next_token.value) };
            return state_79(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::WhileExpr>>
    state_195 (
        Parser &parser,
        Located<Tokens::While> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        std::unique_ptr<ASTNS::Block> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
            Maybe<Location const>(a0.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::WhileExpr> push (std::make_unique<ASTNS::WhileExpr>(p.sourcefile, span, std::move(a1), std::move(a2)));
            std::unique_ptr<ASTNS::WhileExpr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_196 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleAmper> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::BangEqual>:
        {
            Located<Tokens::BangEqual> downcasted_token { next_token.span, Tokens::as<Tokens::BangEqual>(next_token.value) };
            return state_152(parser, a0, a1, a2, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ShortCircuitExpr> push (std::make_unique<ASTNS::ShortCircuitExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleEqual>:
        {
            Located<Tokens::DoubleEqual> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleEqual>(next_token.value) };
            return state_153(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_197 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::BangEqual> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Greater>:
        {
            Located<Tokens::Greater> downcasted_token { next_token.span, Tokens::as<Tokens::Greater>(next_token.value) };
            return state_155(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::GreaterEqual>:
        {
            Located<Tokens::GreaterEqual> downcasted_token { next_token.span, Tokens::as<Tokens::GreaterEqual>(next_token.value) };
            return state_157(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Less>:
        {
            Located<Tokens::Less> downcasted_token { next_token.span, Tokens::as<Tokens::Less>(next_token.value) };
            return state_154(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::LessEqual>:
        {
            Located<Tokens::LessEqual> downcasted_token { next_token.span, Tokens::as<Tokens::LessEqual>(next_token.value) };
            return state_156(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_198 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleEqual> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Greater>:
        {
            Located<Tokens::Greater> downcasted_token { next_token.span, Tokens::as<Tokens::Greater>(next_token.value) };
            return state_155(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::GreaterEqual>:
        {
            Located<Tokens::GreaterEqual> downcasted_token { next_token.span, Tokens::as<Tokens::GreaterEqual>(next_token.value) };
            return state_157(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Less>:
        {
            Located<Tokens::Less> downcasted_token { next_token.span, Tokens::as<Tokens::Less>(next_token.value) };
            return state_154(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::LessEqual>:
        {
            Located<Tokens::LessEqual> downcasted_token { next_token.span, Tokens::as<Tokens::LessEqual>(next_token.value) };
            return state_156(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_199 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Less> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Caret>:
        {
            Located<Tokens::Caret> downcasted_token { next_token.span, Tokens::as<Tokens::Caret>(next_token.value) };
            return state_158(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_200 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Greater> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Caret>:
        {
            Located<Tokens::Caret> downcasted_token { next_token.span, Tokens::as<Tokens::Caret>(next_token.value) };
            return state_158(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_201 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::LessEqual> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Caret>:
        {
            Located<Tokens::Caret> downcasted_token { next_token.span, Tokens::as<Tokens::Caret>(next_token.value) };
            return state_158(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_202 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::GreaterEqual> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Caret>:
        {
            Located<Tokens::Caret> downcasted_token { next_token.span, Tokens::as<Tokens::Caret>(next_token.value) };
            return state_158(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_203 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Caret> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Pipe>:
        {
            Located<Tokens::Pipe> downcasted_token { next_token.span, Tokens::as<Tokens::Pipe>(next_token.value) };
            return state_159(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_204 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Pipe> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_160(parser, a0, a1, a2, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_205 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Amper> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::DoubleGreater>:
        {
            Located<Tokens::DoubleGreater> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleGreater>(next_token.value) };
            return state_161(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::DoubleLess>:
        {
            Located<Tokens::DoubleLess> downcasted_token { next_token.span, Tokens::as<Tokens::DoubleLess>(next_token.value) };
            return state_162(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_206 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleGreater> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_164(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Plus>:
        {
            Located<Tokens::Plus> downcasted_token { next_token.span, Tokens::as<Tokens::Plus>(next_token.value) };
            return state_163(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_207 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::DoubleLess> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_164(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Plus>:
        {
            Located<Tokens::Plus> downcasted_token { next_token.span, Tokens::as<Tokens::Plus>(next_token.value) };
            return state_163(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_208 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Plus> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Percent>:
        {
            Located<Tokens::Percent> downcasted_token { next_token.span, Tokens::as<Tokens::Percent>(next_token.value) };
            return state_167(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Slash>:
        {
            Located<Tokens::Slash> downcasted_token { next_token.span, Tokens::as<Tokens::Slash>(next_token.value) };
            return state_166(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_165(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_209 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Minus> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Percent>:
        {
            Located<Tokens::Percent> downcasted_token { next_token.span, Tokens::as<Tokens::Percent>(next_token.value) };
            return state_167(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Slash>:
        {
            Located<Tokens::Slash> downcasted_token { next_token.span, Tokens::as<Tokens::Slash>(next_token.value) };
            return state_166(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_165(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_210 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Star> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_211 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Slash> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_212 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Percent> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_213 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::RightArrow> const &a1,
        std::unique_ptr<ASTNS::Type> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::CastExpr> push (std::make_unique<ASTNS::CastExpr>(p.sourcefile, span, std::move(a2), std::move(a0)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_214 (
        Parser &parser,
        Located<Tokens::Amper> const &a0,
        Located<Tokens::Mut> const &a1,
        std::unique_ptr<ASTNS::Expr> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::AddrofExpr> push (std::make_unique<ASTNS::AddrofExpr>(p.sourcefile, span, a0, std::move(a2), true));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_215 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::OParen> const &a1,
        std::unique_ptr<ASTNS::ArgList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_231(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ArgList>>
    state_216 (
        Parser &parser,
        std::unique_ptr<ASTNS::ArgList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ArgList>>
    state_217 (
        Parser &parser,
        std::unique_ptr<ASTNS::ArgList> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Comma>:
        {
            Located<Tokens::Comma> downcasted_token { next_token.span, Tokens::as<Tokens::Comma>(next_token.value) };
            return state_232(parser, a0, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ArgList>>
    state_218 (
        Parser &parser,
        std::unique_ptr<ASTNS::Arg> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
            push->args.push_back(std::move(a0));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Arg>>
    state_219 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Arg> push (std::make_unique<ASTNS::Arg>(p.sourcefile, span, std::move(a0)));
            std::unique_ptr<ASTNS::Arg> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_220 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(p.sourcefile, span, std::move(a0), a1, a2));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_233(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_221 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(p.sourcefile, span, std::move(a0), a1, a2));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_234(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_222 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::OParen> const &a1,
        std::unique_ptr<ASTNS::ArgList> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_235(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_223 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(p.sourcefile, span, std::move(a0), a1, a2));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_236(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_224 (
        Parser &parser,
        Located<Tokens::OParen> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        Located<Tokens::CParen> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a1);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_225 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2,
        std::unique_ptr<ASTNS::StmtList> const &a3,
        Located<Tokens::Dedent> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CBrace>:
        {
            Located<Tokens::CBrace> downcasted_token { next_token.span, Tokens::as<Tokens::CBrace>(next_token.value) };
            return state_237(parser, a0, a1, a2, a3, a4, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItemList>>
    state_226 (
        Parser &parser,
        std::unique_ptr<ASTNS::VarStmtItemList> const &a0,
        Located<Tokens::Comma> const &a1,
        std::unique_ptr<ASTNS::VarStmtItem> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->items.push_back(std::move(a2));
            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_227 (
        Parser &parser,
        std::unique_ptr<ASTNS::VarStmtItem> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_228 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1,
        Located<Tokens::Equal> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, a2, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_229 (
        Parser &parser,
        Located<Tokens::Mut> const &a0,
        Located<Tokens::Identifier> const &a1,
        std::unique_ptr<ASTNS::Type> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a2), true, a1, a1, nullptr));
            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::Equal>:
        {
            Located<Tokens::Equal> downcasted_token { next_token.span, Tokens::as<Tokens::Equal>(next_token.value) };
            return state_239(parser, a0, a1, a2, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::IfExpr>>
    state_230 (
        Parser &parser,
        Located<Tokens::If> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        std::unique_ptr<ASTNS::Block> const &a2,
        Located<Tokens::Else> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Newline>:
        {
            Located<Tokens::Newline> downcasted_token { next_token.span, Tokens::as<Tokens::Newline>(next_token.value) };
            return state_194(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_231 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::OParen> const &a1,
        std::unique_ptr<ASTNS::ArgList> const &a2,
        Located<Tokens::CParen> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::CallExpr> push (std::make_unique<ASTNS::CallExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2->args)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ArgList>>
    state_232 (
        Parser &parser,
        std::unique_ptr<ASTNS::ArgList> const &a0,
        Located<Tokens::Comma> const &a1
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, downcasted_token);
        }
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_233 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::CParen>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_234 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::CParen>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_235 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::OParen> const &a1,
        std::unique_ptr<ASTNS::ArgList> const &a2,
        Located<Tokens::CParen> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::CallExpr> push (std::make_unique<ASTNS::CallExpr>(p.sourcefile, span, std::move(a0), a1, std::move(a2->args)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_236 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::CParen>:
        {
            Maybe<Location const> start, end;
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(p.sourcefile, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Block>>
    state_237 (
        Parser &parser,
        Located<Tokens::OBrace> const &a0,
        Located<Tokens::Newline> const &a1,
        Located<Tokens::Indent> const &a2,
        std::unique_ptr<ASTNS::StmtList> const &a3,
        Located<Tokens::Dedent> const &a4,
        Located<Tokens::CBrace> const &a5
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a5.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(p.sourcefile, span, std::move(a3->stmts)));
            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_238 (
        Parser &parser,
        Located<Tokens::Identifier> const &a0,
        std::unique_ptr<ASTNS::Type> const &a1,
        Located<Tokens::Equal> const &a2,
        std::unique_ptr<ASTNS::Expr> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a3 && a3->span().has() ? Maybe<Location const>(a3->span().get().end) :
            Maybe<Location const>(a2.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a1), false, a0, a2, std::move(a3)));
            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_239 (
        Parser &parser,
        Located<Tokens::Mut> const &a0,
        Located<Tokens::Identifier> const &a1,
        std::unique_ptr<ASTNS::Type> const &a2,
        Located<Tokens::Equal> const &a3
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::Amper>:
        {
            Located<Tokens::Amper> downcasted_token { next_token.span, Tokens::as<Tokens::Amper>(next_token.value) };
            return state_112(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Bang>:
        {
            Located<Tokens::Bang> downcasted_token { next_token.span, Tokens::as<Tokens::Bang>(next_token.value) };
            return state_111(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::BoolLit>:
        {
            Located<Tokens::BoolLit> downcasted_token { next_token.span, Tokens::as<Tokens::BoolLit>(next_token.value) };
            return state_118(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::CharLit>:
        {
            Located<Tokens::CharLit> downcasted_token { next_token.span, Tokens::as<Tokens::CharLit>(next_token.value) };
            return state_121(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::FloatLit>:
        {
            Located<Tokens::FloatLit> downcasted_token { next_token.span, Tokens::as<Tokens::FloatLit>(next_token.value) };
            return state_119(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Identifier>:
        {
            Located<Tokens::Identifier> downcasted_token { next_token.span, Tokens::as<Tokens::Identifier>(next_token.value) };
            return state_18(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::If>:
        {
            Located<Tokens::If> downcasted_token { next_token.span, Tokens::as<Tokens::If>(next_token.value) };
            return state_96(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::IntLit>:
        {
            Located<Tokens::IntLit> downcasted_token { next_token.span, Tokens::as<Tokens::IntLit>(next_token.value) };
            return state_120(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Minus>:
        {
            Located<Tokens::Minus> downcasted_token { next_token.span, Tokens::as<Tokens::Minus>(next_token.value) };
            return state_110(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OBrace>:
        {
            Located<Tokens::OBrace> downcasted_token { next_token.span, Tokens::as<Tokens::OBrace>(next_token.value) };
            return state_76(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::OParen>:
        {
            Located<Tokens::OParen> downcasted_token { next_token.span, Tokens::as<Tokens::OParen>(next_token.value) };
            return state_124(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Star>:
        {
            Located<Tokens::Star> downcasted_token { next_token.span, Tokens::as<Tokens::Star>(next_token.value) };
            return state_113(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::StringLit>:
        {
            Located<Tokens::StringLit> downcasted_token { next_token.span, Tokens::as<Tokens::StringLit>(next_token.value) };
            return state_122(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::This>:
        {
            Located<Tokens::This> downcasted_token { next_token.span, Tokens::as<Tokens::This>(next_token.value) };
            return state_123(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::Tilde>:
        {
            Located<Tokens::Tilde> downcasted_token { next_token.span, Tokens::as<Tokens::Tilde>(next_token.value) };
            return state_109(parser, a0, a1, a2, a3, downcasted_token);
        }
        case Tokens::index_of<Tokens::While>:
        {
            Located<Tokens::While> downcasted_token { next_token.span, Tokens::as<Tokens::While>(next_token.value) };
            return state_97(parser, a0, a1, a2, a3, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::IfExpr>>
    state_240 (
        Parser &parser,
        Located<Tokens::If> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        std::unique_ptr<ASTNS::Block> const &a2,
        Located<Tokens::Else> const &a3,
        std::unique_ptr<ASTNS::Block> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(p.sourcefile, span, a0, a3, std::move(a1), std::move(a2), std::move(a4)));
            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::IfExpr>>
    state_241 (
        Parser &parser,
        Located<Tokens::If> const &a0,
        std::unique_ptr<ASTNS::Expr> const &a1,
        std::unique_ptr<ASTNS::Block> const &a2,
        Located<Tokens::Else> const &a3,
        std::unique_ptr<ASTNS::IfExpr> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(p.sourcefile, span, a0, a3, std::move(a1), std::move(a2), std::move(a4)));
            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::ArgList>>
    state_242 (
        Parser &parser,
        std::unique_ptr<ASTNS::ArgList> const &a0,
        Located<Tokens::Comma> const &a1,
        std::unique_ptr<ASTNS::Arg> const &a2
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
            Maybe<Location const>(a1.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            a0->args.push_back(std::move(a2));
            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Arg>>
    state_243 (
        Parser &parser,
        std::unique_ptr<ASTNS::Arg> const &a0
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
            Maybe<Location const> end =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::Arg> pushitem = std::move(a0);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_244 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3,
        std::unique_ptr<ASTNS::ArgList> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_248(parser, a0, a1, a2, a3, a4, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_245 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3,
        std::unique_ptr<ASTNS::ArgList> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_249(parser, a0, a1, a2, a3, a4, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_246 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3,
        std::unique_ptr<ASTNS::ArgList> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        case Tokens::index_of<Tokens::CParen>:
        {
            Located<Tokens::CParen> downcasted_token { next_token.span, Tokens::as<Tokens::CParen>(next_token.value) };
            return state_250(parser, a0, a1, a2, a3, a4, downcasted_token);
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::VarStmtItem>>
    state_247 (
        Parser &parser,
        Located<Tokens::Mut> const &a0,
        Located<Tokens::Identifier> const &a1,
        std::unique_ptr<ASTNS::Type> const &a2,
        Located<Tokens::Equal> const &a3,
        std::unique_ptr<ASTNS::Expr> const &a4
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
            Maybe<Location const>(a0.span.start);
            Maybe<Location const> end =
                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
            Maybe<Location const>(a3.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(p.sourcefile, span, std::move(a2), true, a1, a3, std::move(a4)));
            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_248 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3,
        std::unique_ptr<ASTNS::ArgList> const &a4,
        Located<Tokens::CParen> const &a5
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a5.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(p.sourcefile, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_249 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3,
        std::unique_ptr<ASTNS::ArgList> const &a4,
        Located<Tokens::CParen> const &a5
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a5.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(p.sourcefile, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}

static RetAndToken<std::unique_ptr<ASTNS::Expr>>
    state_250 (
        Parser &parser,
        std::unique_ptr<ASTNS::Expr> const &a0,
        Located<Tokens::Period> const &a1,
        Located<Tokens::Identifier> const &a2,
        Located<Tokens::OParen> const &a3,
        std::unique_ptr<ASTNS::ArgList> const &a4,
        Located<Tokens::CParen> const &a5
    ) {
    Located<TokenData> next_token = parser.consume();
    switch (next_token.value.index()) {
        default:
        {
            Maybe<Location const> start =
                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
            Maybe<Location const>(a1.span.start);
            Maybe<Location const> end =
            Maybe<Location const>(a5.span.end);
            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
            std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(p.sourcefile, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
            return RetAndToken { pushitem, next_token };
        }
    }
}
// PARSESTATES END }}}

std::unique_ptr<ASTNS::CUB> _parse(Parser &p) {
    return state_0(p);
}
