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

std::unique_ptr<ASTNS::CUB> _parse(Parser &p) {
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
// PARSESTATES START {{{
void state_0()void state_1()void state_2()void state_3()void state_4()void state_5()void state_6()void state_7()void state_8()void state_9()void state_10()void state_11()void state_12()void state_13()void state_14()void state_15()void state_16()void state_17()void state_18()void state_19()void state_20()void state_21()void state_22()void state_23()void state_24()void state_25()void state_26()void state_27()void state_28()void state_29()void state_30()void state_31()void state_32()void state_33()void state_34()void state_35()void state_36()void state_37()void state_38()void state_39()void state_40()void state_41()void state_42()void state_43()void state_44()void state_45()void state_46()void state_47()void state_48()void state_49()void state_50()void state_51()void state_52()void state_53()void state_54()void state_55()void state_56()void state_57()void state_58()void state_59()void state_60()void state_61()void state_62()void state_63()void state_64()void state_65()void state_66()void state_67()void state_68()void state_69()void state_70()void state_71()void state_72()void state_73()void state_74()void state_75()void state_76()void state_77()void state_78()void state_79()void state_80()void state_81()void state_82()void state_83()void state_84()void state_85()void state_86()void state_87()void state_88()void state_89()void state_90()void state_91()void state_92()void state_93()void state_94()void state_95()void state_96()void state_97()void state_98()void state_99()void state_100()void state_101()void state_102()void state_103()void state_104()void state_105()void state_106()void state_107()void state_108()void state_109()void state_110()void state_111()void state_112()void state_113()void state_114()void state_115()void state_116()void state_117()void state_118()void state_119()void state_120()void state_121()void state_122()void state_123()void state_124()void state_125()void state_126()void state_127()void state_128()void state_129()void state_130()void state_131()void state_132()void state_133()void state_134()void state_135()void state_136()void state_137()void state_138()void state_139()void state_140()void state_141()void state_142()void state_143()void state_144()void state_145()void state_146()void state_147()void state_148()void state_149()void state_150()void state_151()void state_152()void state_153()void state_154()void state_155()void state_156()void state_157()void state_158()void state_159()void state_160()void state_161()void state_162()void state_163()void state_164()void state_165()void state_166()void state_167()void state_168()void state_169()void state_170()void state_171()void state_172()void state_173()void state_174()void state_175()void state_176()void state_177()void state_178()void state_179()void state_180()void state_181()void state_182()void state_183()void state_184()void state_185()void state_186()void state_187()void state_188()void state_189()void state_190()void state_191()void state_192()void state_193()void state_194()void state_195()void state_196()void state_197()void state_198()void state_199()void state_200()void state_201()void state_202()void state_203()void state_204()void state_205()void state_206()void state_207()void state_208()void state_209()void state_210()void state_211()void state_212()void state_213()void state_214()void state_215()void state_216()void state_217()void state_218()void state_219()void state_220()void state_221()void state_222()void state_223()void state_224()void state_225()void state_226()void state_227()void state_228()void state_229()void state_230()void state_231()void state_232()void state_233()void state_234()void state_235()void state_236()void state_237()void state_238()void state_239()void state_240()void state_241()void state_242()void state_243()void state_244()void state_245()void state_246()void state_247()void state_248()void state_249()void state_250()
// PARSESTATES END }}}
