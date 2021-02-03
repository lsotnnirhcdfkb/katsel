#include <vector>
#include <memory>
#include <variant>

#include "parse/parser.h"
#include "lex/lexer.h"
#include "ast/ast.h"
#include "message/errmsgs.h"

// Token stream {{{
class TokenStream {
public:
    TokenStream(Lexer &lex): errored(false), lex(lex) {
        ensure(0);
    }

    void advance() {
        bool lastboom = false;
        while (true) {
            Located<TokenData> cur (lex.next_token());
            if (Tokens::is<Tokens::Error>(cur.value)) {
                errored = true;
                (*Tokens::as<Tokens::Error>(cur.value).errf)(cur.span);
            } else if (Tokens::is<Tokens::Boom>(cur.value)) {
                lastboom = true;
            } else if (lastboom && Tokens::is<Tokens::Newline>(cur.value)) {
                lastboom = false;
            } else {
                tokens.push_back(cur);
                return;
            }
        }
    }

    void ensure(size_t i) {
        while (tokens.size() < i + 1)
            advance();
    }

    Located<TokenData> at(size_t i) {
        return tokens[i];
    }

    bool errored;

private:
    Lexer &lex;
    std::vector<Located<TokenData>> tokens;
    // TODO: maybe it is not a good idea to store the token list of the entire file, because error recovery only rewinds about 1-2 times
};
class TokenStreamView {
public:
    TokenStreamView(TokenStream &ts, size_t loc=0):
        ts(ts), loc(loc) {}

    void advance() {
        ++loc;
        ts.ensure(loc);
    }
    void rewind() {
        --loc;
    }

    Located<TokenData> next() {
        return ts.at(loc);
    }
    Located<TokenData> prev() {
        return ts.at(loc == 0 ? loc : loc - 1);
    }

private:
    TokenStream &ts;
    size_t loc;
};
// }}}
// _Parser {{{
class _Parser {
    // nonterminal enum {{{
    enum class NonTerminal {
        // NONTERM ENUM START
        _49, _0, _50, _22, _52, _51, _53, _21, _55, _54, _56, _10, _58, _57, _59, _6, _60, _61, _1, _62, _63, _5, _64, _65, _66, _67, _68, _69, _25, _70, _7, _71, _11, _72, _12, _73, _16, _2, _3, _13, _17, _4, _8, _9, _27, _26, _14, _15, _20, _18, _19, _48, _24, _23, _30, _28, _29, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _42, _41, _43, _44, _45, _46, _47, 
        // NONTERM ENUM END
    };
    // }}}
    // stack items {{{
    template <typename T>
    struct ASTItem { T ast; NonTerminal nt; };
    struct TokenItem { Located<TokenData> tok; };
    struct InitialItem { int dummy; };

    struct StackItem {
        int state;
        std::variant<TokenItem, InitialItem,
            // PARSESTACK ITEM TYPES START {{{
            ASTItem<std::unique_ptr<ASTNS::AST>>, ASTItem<std::unique_ptr<ASTNS::Arg>>, ASTItem<std::unique_ptr<ASTNS::ArgList>>, ASTItem<std::unique_ptr<ASTNS::Block>>, ASTItem<std::unique_ptr<ASTNS::CU>>, ASTItem<std::unique_ptr<ASTNS::Decl>>, ASTItem<std::unique_ptr<ASTNS::DeclList>>, ASTItem<std::unique_ptr<ASTNS::Expr>>, ASTItem<std::unique_ptr<ASTNS::ExprStmt>>, ASTItem<std::unique_ptr<ASTNS::FunctionDecl>>, ASTItem<std::unique_ptr<ASTNS::IfExpr>>, ASTItem<std::unique_ptr<ASTNS::ImplMember>>, ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>, ASTItem<std::unique_ptr<ASTNS::Param>>, ASTItem<std::unique_ptr<ASTNS::ParamB>>, ASTItem<std::unique_ptr<ASTNS::ParamList>>, ASTItem<std::unique_ptr<ASTNS::Path>>, ASTItem<std::unique_ptr<ASTNS::PathType>>, ASTItem<std::unique_ptr<ASTNS::PointerType>>, ASTItem<std::unique_ptr<ASTNS::PureLocation>>, ASTItem<std::unique_ptr<ASTNS::RetStmt>>, ASTItem<std::unique_ptr<ASTNS::Stmt>>, ASTItem<std::unique_ptr<ASTNS::StmtList>>, ASTItem<std::unique_ptr<ASTNS::ThisParam>>, ASTItem<std::unique_ptr<ASTNS::ThisType>>, ASTItem<std::unique_ptr<ASTNS::Type>>, ASTItem<std::unique_ptr<ASTNS::VarStmt>>, ASTItem<std::unique_ptr<ASTNS::VarStmtItem>>, ASTItem<std::unique_ptr<ASTNS::VarStmtItemList>>, ASTItem<std::unique_ptr<ASTNS::WhileExpr>>
            // PARSESTACK ITEM TYPES END }}}
            > item;

        template <typename T>
        StackItem(int state, T &&thing): state(state), item(std::forward<T>(thing)) {}

        StackItem(int state): state(state), item(InitialItem {}) {}
    };
    // }}}
    // get goto {{{
    // GETGOTO START
    size_t get_goto(NonTerminal nterm, size_t state) {
        switch (nterm) {
            case NonTerminal::_49:
                switch (state) {
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_0:
                switch (state) {
                    case 0:
                        return 1;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_50:
                switch (state) {
                    case 49:
                        return 63;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_22:
                switch (state) {
                    case 19:
                        return 29;
                    case 49:
                        return 64;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_52:
                switch (state) {
                    case 19:
                        return 28;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_51:
                switch (state) {
                    case 19:
                        return 27;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_53:
                switch (state) {
                    case 235:
                        return 245;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_21:
                switch (state) {
                    case 175: case 178: case 236: case 237: case 239:
                        return 218;
                    case 235:
                        return 246;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_55:
                switch (state) {
                    case 175: case 178: case 236: case 237: case 239:
                        return 217;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_54:
                switch (state) {
                    case 175: case 178: case 236: case 237: case 239:
                        return 216;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_56:
                switch (state) {
                    case 187:
                        return 229;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_10:
                switch (state) {
                    case 87:
                        return 138;
                    case 187:
                        return 230;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_58:
                switch (state) {
                    case 87:
                        return 137;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_57:
                switch (state) {
                    case 87:
                        return 136;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_59:
                switch (state) {
                    case 82:
                        return 134;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_6:
                switch (state) {
                    case 76: case 79: case 81: case 133:
                        return 83;
                    case 82:
                        return 135;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_60:
                switch (state) {
                    case 76: case 79: case 81: case 133:
                        return 82;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_61:
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
            case NonTerminal::_62:
                switch (state) {
                    case 0:
                        return 2;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_63:
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
            case NonTerminal::_64:
                switch (state) {
                    case 21: case 41: case 45: case 58:
                        return 42;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_65:
                switch (state) {
                    case 19:
                        return 26;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_66:
                switch (state) {
                    case 175:
                        return 215;
                    case 178:
                        return 222;
                    case 236:
                        return 247;
                    case 237:
                        return 248;
                    case 239:
                        return 249;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_67:
                switch (state) {
                    case 76:
                        return 80;
                    case 79:
                        return 128;
                    case 81:
                        return 132;
                    case 133:
                        return 185;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_68:
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
            case NonTerminal::_69:
                switch (state) {
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_25:
                switch (state) {
                    case 90:
                        return 143;
                    case 96:
                        return 149;
                    case 97:
                        return 150;
                    case 124:
                        return 180;
                    case 131:
                        return 183;
                    case 175: case 178: case 235: case 236: case 237: case 239:
                        return 219;
                    case 231:
                        return 241;
                    case 242:
                        return 251;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_70:
                switch (state) {
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_7:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 133:
                        return 84;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_71:
                switch (state) {
                    case 20:
                        return 36;
                    case 71:
                        return 78;
                    case 89:
                        return 142;
                    case 183:
                        return 226;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_11:
                switch (state) {
                    case 20: case 71: case 89: case 183:
                        return 37;
                    case 62:
                        return 72;
                    case 88:
                        return 141;
                    case 90:
                        return 144;
                    case 136:
                        return 186;
                    case 143:
                        return 190;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_72:
                switch (state) {
                    case 80:
                        return 129;
                    case 128:
                        return 181;
                    case 132:
                        return 184;
                    case 185:
                        return 228;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_12:
                switch (state) {
                    case 80: case 128: case 132: case 185:
                        return 130;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_73:
                switch (state) {
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_16:
                switch (state) {
                    case 32:
                        return 50;
                    case 48:
                        return 62;
                    case 52:
                        return 66;
                    case 139:
                        return 188;
                    case 189:
                        return 232;
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
            case NonTerminal::_13:
                switch (state) {
                    case 62:
                        return 71;
                    case 149:
                        return 193;
                    case 150:
                        return 195;
                    case 233:
                        return 243;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_17:
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
                    case 76: case 79: case 81: case 82: case 133:
                        return 85;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_9:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 133:
                        return 86;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_27:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 133:
                        return 88;
                    case 90: case 96: case 97: case 124: case 131: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 146;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_26:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 133:
                        return 89;
                    case 90: case 96: case 97: case 124: case 131: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 145;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_14:
                switch (state) {
                    case 62: case 149: case 150: case 233:
                        return 73;
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 94;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_15:
                switch (state) {
                    case 62: case 149: case 150: case 233:
                        return 74;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_20:
                switch (state) {
                    case 7: case 16: case 25: case 51: case 168:
                        return 12;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_18:
                switch (state) {
                    case 7: case 16: case 25: case 51: case 168:
                        return 13;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_19:
                switch (state) {
                    case 7: case 16: case 25: case 51: case 168:
                        return 14;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_48:
                switch (state) {
                    case 7: case 16: case 25: case 51: case 168:
                        return 15;
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 126;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_24:
                switch (state) {
                    case 19: case 49:
                        return 30;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_23:
                switch (state) {
                    case 19: case 49:
                        return 31;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_30:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 91;
                    case 147:
                        return 191;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_28:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 92;
                    case 233:
                        return 244;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_29:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 93;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_31:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 95;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_32:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 98;
                    case 148:
                        return 192;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_33:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 99;
                    case 151:
                        return 196;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_34:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 100;
                    case 152:
                        return 197;
                    case 153:
                        return 198;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_35:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
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
            case NonTerminal::_36:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 102;
                    case 158:
                        return 203;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_37:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 103;
                    case 159:
                        return 204;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_38:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 104;
                    case 160:
                        return 205;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_39:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 105;
                    case 161:
                        return 206;
                    case 162:
                        return 207;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_40:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 106;
                    case 163:
                        return 208;
                    case 164:
                        return 209;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_42:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
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
            case NonTerminal::_41:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 107;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_43:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 114;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_44:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 115;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_45:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 116;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_46:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 117;
                    default: report_abort_noh("get invalid goto");
                }
            case NonTerminal::_47:
                switch (state) {
                    case 76: case 79: case 81: case 82: case 90: case 96: case 97: case 109: case 110: case 111: case 112: case 113: case 124: case 131: case 133: case 147: case 148: case 151: case 152: case 153: case 154: case 155: case 156: case 157: case 158: case 159: case 160: case 161: case 162: case 163: case 164: case 165: case 166: case 167: case 173: case 175: case 178: case 231: case 235: case 236: case 237: case 239: case 242:
                        return 125;
                    default: report_abort_noh("get invalid goto");
                }
        }
    }
    // GETGOTO END
    // }}}

public:
    _Parser(TokenStreamView &tsv, File &file):
        tsv(tsv), file(file), done(false), errored(false) {}

    std::unique_ptr<ASTNS::CUB> parse() {
        stack.emplace_back(0);
        while (!done) {
            next_move();
        }

        if (errored)
            return nullptr;
        else
            return pop_as<ASTItem<std::unique_ptr<ASTNS::CU>>>(stack).ast;
    }

    void next_move() {
        // PARSERLOOP START {{{
        switch (stack.back().state) {
            case 0:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Impl>:
                        stack.emplace_back(7, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::CU> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_0, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_0 });
                        }
                        break;
                }
                break;
            case 1:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::_EOF>:
                        done = true;
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} after {}", Tokens::_EOF::stringify(), "augment") } );
                        errored = done = true;
                }
                break;
            case 2:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Impl>:
                        stack.emplace_back(7, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::DeclList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::CU> push (std::make_unique<ASTNS::CU>(file, span, std::move(a0->decls)));
                            std::unique_ptr<ASTNS::CU> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_0, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_0 });
                        }
                        break;
                }
                break;
            case 3:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Decl>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::DeclList> push (std::make_unique<ASTNS::DeclList>(file, span, std::vector<std::unique_ptr<ASTNS::Decl>> {}));

                push->decls.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::DeclList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_62, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_62 });
                        }
                        break;
                }
                break;
            case 4:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::FunctionDecl>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_1, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_1 });
                        }
                        break;
                }
                break;
            case 5:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Decl>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_1, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_1 });
                        }
                        break;
                }
                break;
            case 6:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(10, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "function declaration") } );
                        errored = done = true;
                }
                break;
            case 7:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(16, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(17, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "type specifier", "implementation") } );
                        errored = done = true;
                }
                break;
            case 8:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Decl>>>(stack).ast);
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::DeclList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->decls.push_back(std::move(a1));
                            std::unique_ptr<ASTNS::DeclList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_62, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_62 });
                        }
                        break;
                }
                break;
            case 9:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Decl>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_61, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_61 });
                        }
                        break;
                }
                break;
            case 10:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(19, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::OParen::stringify(), "function declaration") } );
                        errored = done = true;
                }
                break;
            case 11:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(22, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(21, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "implementation body", "implementation") } );
                        errored = done = true;
                }
                break;
            case 12:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::PathType>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_17, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_17 });
                        }
                        break;
                }
                break;
            case 13:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::PointerType>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_17, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_17 });
                        }
                        break;
                }
                break;
            case 14:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ThisType>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_17, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_17 });
                        }
                        break;
                }
                break;
            case 15:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Path>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PathType> push (std::make_unique<ASTNS::PathType>(file, span, std::move(a0)));
                            std::unique_ptr<ASTNS::PathType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_20, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_20 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleColon>:
                        stack.emplace_back(23, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 16:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(25, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(16, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(17, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "type specifier", Tokens::Mut::stringify()), "pointer type") } );
                        errored = done = true;
                }
                break;
            case 17:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::This> a0 { _a0.span, Tokens::as<Tokens::This>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ThisType> push (std::make_unique<ASTNS::ThisType>(file, span, a0));
                            std::unique_ptr<ASTNS::ThisType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_19, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_19 });
                        }
                        break;
                }
                break;
            case 18:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a0 { _a0.span, Tokens::as<Tokens::Identifier>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Path> push (std::make_unique<ASTNS::Path>(file, span, std::vector<Located<Tokens::Identifier>> {}));

                push->segments.push_back(a0);
                            std::unique_ptr<ASTNS::Path> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_48, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_48 });
                        }
                        break;
                }
                break;
            case 19:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ParamList> push (std::make_unique<ASTNS::ParamList>(file, span, std::vector<std::unique_ptr<ASTNS::ParamB>> {}));
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_65 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(32, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(33, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(35, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(34, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional function parameter list", "function declaration") } );
                        errored = done = true;
                }
                break;
            case 20:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                    case Tokens::index_of<Tokens::Bang>:
                    case Tokens::index_of<Tokens::BoolLit>:
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
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
                            stack.emplace_back(get_goto(NonTerminal::_71, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_71 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional line ending", "implementation") } );
                        errored = done = true;
                }
                break;
            case 21:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(file, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_68, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_68 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(41, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "optional implementation member list", Tokens::Newline::stringify()), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 22:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Indent>:
                        stack.emplace_back(45, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Indent::stringify(), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 23:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(46, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "symbol path") } );
                        errored = done = true;
                }
                break;
            case 24:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Star> a0 { _a0.span, Tokens::as<Tokens::Star>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PointerType> push (std::make_unique<ASTNS::PointerType>(file, span, false, std::move(a1)));
                            std::unique_ptr<ASTNS::PointerType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_18, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_18 });
                        }
                        break;
                }
                break;
            case 25:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(16, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(17, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "type specifier", "pointer type") } );
                        errored = done = true;
                }
                break;
            case 26:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(48, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "function declaration") } );
                        errored = done = true;
                }
                break;
            case 27:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_65, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_65 });
                        }
                        break;
                }
                break;
            case 28:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_51, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_51 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Comma>:
                        stack.emplace_back(49, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 29:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamB>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ParamList> push (std::make_unique<ASTNS::ParamList>(file, span, std::vector<std::unique_ptr<ASTNS::ParamB>> {}));

                push->params.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_52, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_52 });
                        }
                        break;
                }
                break;
            case 30:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Param>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_22, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_22 });
                        }
                        break;
                }
                break;
            case 31:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ThisParam>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_22, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_22 });
                        }
                        break;
                }
                break;
            case 32:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Colon>:
                        stack.emplace_back(51, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "required type annotation", "function parameter") } );
                        errored = done = true;
                }
                break;
            case 33:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(52, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "function parameter") } );
                        errored = done = true;
                }
                break;
            case 34:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::This> a0 { _a0.span, Tokens::as<Tokens::This>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(file, span, false, false));
                            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_23, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_23 });
                        }
                        break;
                }
                break;
            case 35:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(54, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(53, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", Tokens::This::stringify(), Tokens::Mut::stringify()), "'this' function parameter") } );
                        errored = done = true;
                }
                break;
            case 36:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Impl> a0 { _a0.span, Tokens::as<Tokens::Impl>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ImplDecl> push (std::make_unique<ASTNS::ImplDecl>(file, span, std::move(a1), std::move(a2->members)));
                            std::unique_ptr<ASTNS::Decl> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_3, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_3 });
                        }
                        break;
                }
                break;
            case 37:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_71, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_71 });
                        }
                        break;
                }
                break;
            case 38:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a0 { _a0.span, Tokens::as<Tokens::Newline>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(file, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_11 });
                        }
                        break;
                }
                break;
            case 39:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Semicolon> a0 { _a0.span, Tokens::as<Tokens::Semicolon>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(file, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_11 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(55, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 40:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                        stack.emplace_back(56, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CBrace::stringify(), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 41:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(file, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_68, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_68 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Indent>:
                        stack.emplace_back(58, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "optional implementation member list", Tokens::Indent::stringify()), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 42:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_68, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_68 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 43:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMember>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(file, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));

                push->members.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_64, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_64 });
                        }
                        break;
                }
                break;
            case 44:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::FunctionDecl>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FunctionImplMember> push (std::make_unique<ASTNS::FunctionImplMember>(file, span, std::move(a0)));
                            std::unique_ptr<ASTNS::ImplMember> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_5, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_5 });
                        }
                        break;
                }
                break;
            case 45:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(file, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_68, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_68 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional implementation member list", "implementation body") } );
                        errored = done = true;
                }
                break;
            case 46:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::DoubleColon> a1 { _a1.span, Tokens::as<Tokens::DoubleColon>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Path>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->segments.push_back(a2);
                            std::unique_ptr<ASTNS::Path> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_48, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_48 });
                        }
                        break;
                }
                break;
            case 47:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Mut> a1 { _a1.span, Tokens::as<Tokens::Mut>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Star> a0 { _a0.span, Tokens::as<Tokens::Star>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PointerType> push (std::make_unique<ASTNS::PointerType>(file, span, true, std::move(a2)));
                            std::unique_ptr<ASTNS::PointerType> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_18, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_18 });
                        }
                        break;
                }
                break;
            case 48:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Colon>:
                        stack.emplace_back(51, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "required type annotation", "function declaration") } );
                        errored = done = true;
                }
                break;
            case 49:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Comma> a1 { _a1.span, Tokens::as<Tokens::Comma>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_51, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_51 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(32, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(33, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(35, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(34, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 50:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a0 { _a0.span, Tokens::as<Tokens::Identifier>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Param> push (std::make_unique<ASTNS::Param>(file, span, std::move(a1), a0, false));
                            std::unique_ptr<ASTNS::Param> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_24, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_24 });
                        }
                        break;
                }
                break;
            case 51:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(16, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(17, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "type specifier", "required type annotation") } );
                        errored = done = true;
                }
                break;
            case 52:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Colon>:
                        stack.emplace_back(51, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "required type annotation", "function parameter") } );
                        errored = done = true;
                }
                break;
            case 53:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::This> a1 { _a1.span, Tokens::as<Tokens::This>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Star> a0 { _a0.span, Tokens::as<Tokens::Star>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(file, span, true, false));
                            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_23, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_23 });
                        }
                        break;
                }
                break;
            case 54:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(67, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::This::stringify(), "'this' function parameter") } );
                        errored = done = true;
                }
                break;
            case 55:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a1 { _a1.span, Tokens::as<Tokens::Newline>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Semicolon> a0 { _a0.span, Tokens::as<Tokens::Semicolon>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        WARN_EXTRA_SEMI(a0.span);std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(file, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_11 });
                        }
                        break;
                }
                break;
            case 56:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CBrace> a2 { _a2.span, Tokens::as<Tokens::CBrace>(_a2.value) };
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OBrace> a0 { _a0.span, Tokens::as<Tokens::OBrace>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_4 });
                        }
                        break;
                }
                break;
            case 57:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                        stack.emplace_back(68, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CBrace::stringify(), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 58:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ImplMemberList> push (std::make_unique<ASTNS::ImplMemberList>(file, span, std::vector<std::unique_ptr<ASTNS::ImplMember>> {}));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_68, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_68 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Fun>:
                        stack.emplace_back(6, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional implementation member list", "implementation body") } );
                        errored = done = true;
                }
                break;
            case 59:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMember>>>(stack).ast);
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->members.push_back(std::move(a1));
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_64, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_64 });
                        }
                        break;
                }
                break;
            case 60:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMember>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMember> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_63, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_63 });
                        }
                        break;
                }
                break;
            case 61:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Dedent>:
                        stack.emplace_back(70, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Dedent::stringify(), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 62:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(75, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "code block", "line ending"), "function declaration") } );
                        errored = done = true;
                }
                break;
            case 63:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamB>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Comma> a1 { _a1.span, Tokens::as<Tokens::Comma>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->params.push_back(std::move(a2));
                            std::unique_ptr<ASTNS::ParamList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_52, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_52 });
                        }
                        break;
                }
                break;
            case 64:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamB>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ParamB> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_50, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_50 });
                        }
                        break;
                }
                break;
            case 65:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Colon> a0 { _a0.span, Tokens::as<Tokens::Colon>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Type> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_16, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_16 });
                        }
                        break;
                }
                break;
            case 66:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a1 { _a1.span, Tokens::as<Tokens::Identifier>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Mut> a0 { _a0.span, Tokens::as<Tokens::Mut>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Param> push (std::make_unique<ASTNS::Param>(file, span, std::move(a2), a1, true));
                            std::unique_ptr<ASTNS::Param> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_24, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_24 });
                        }
                        break;
                }
                break;
            case 67:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::This> a2 { _a2.span, Tokens::as<Tokens::This>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Mut> a1 { _a1.span, Tokens::as<Tokens::Mut>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Star> a0 { _a0.span, Tokens::as<Tokens::Star>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ThisParam> push (std::make_unique<ASTNS::ThisParam>(file, span, true, true));
                            std::unique_ptr<ASTNS::ThisParam> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_23, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_23 });
                        }
                        break;
                }
                break;
            case 68:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CBrace> a3 { _a3.span, Tokens::as<Tokens::CBrace>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a1 { _a1.span, Tokens::as<Tokens::Newline>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OBrace> a0 { _a0.span, Tokens::as<Tokens::OBrace>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        WARN_BLOCK_NO_INDENT(a0.span, a3.span);                    std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a2);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_4 });
                        }
                        break;
                }
                break;
            case 69:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Dedent>:
                        stack.emplace_back(77, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Dedent::stringify(), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 70:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Dedent> a3 { _a3.span, Tokens::as<Tokens::Dedent>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Indent> a1 { _a1.span, Tokens::as<Tokens::Indent>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a0 { _a0.span, Tokens::as<Tokens::Newline>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a2);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_4 });
                        }
                        break;
                }
                break;
            case 71:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                    case Tokens::index_of<Tokens::Bang>:
                    case Tokens::index_of<Tokens::BoolLit>:
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
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
                            stack.emplace_back(get_goto(NonTerminal::_71, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_71 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional line ending", "function declaration") } );
                        errored = done = true;
                }
                break;
            case 72:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a6 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a5 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a4 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a4 { _a4.span, Tokens::as<Tokens::CParen>(_a4.value) };
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamList>>>(stack).ast);
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a2 { _a2.span, Tokens::as<Tokens::OParen>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a1 { _a1.span, Tokens::as<Tokens::Identifier>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Fun> a0 { _a0.span, Tokens::as<Tokens::Fun>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a6 && a6->span().has() ? Maybe<Location const>(a6->span().get().end) :
                                a5 && a5->span().has() ? Maybe<Location const>(a5->span().get().end) :
                                Maybe<Location const>(a4.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FunctionDecl> push (std::make_unique<ASTNS::FunctionDecl>(file, span, std::move(a5), a1, std::move(a3->params), nullptr));
                            std::unique_ptr<ASTNS::FunctionDecl> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_2, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_2 });
                        }
                        break;
                }
                break;
            case 73:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_13, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_13 });
                        }
                        break;
                }
                break;
            case 74:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_13, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_13 });
                        }
                        break;
                }
                break;
            case 75:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a0 { _a0.span, Tokens::as<Tokens::Newline>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PureLocation> push (std::make_unique<ASTNS::PureLocation>(file, span, 0));
                            std::unique_ptr<ASTNS::PureLocation> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_11, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_11 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Indent>:
                        stack.emplace_back(79, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 76:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(file, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_67 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(81, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Return>:
                        stack.emplace_back(90, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Var>:
                        stack.emplace_back(87, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "optional statement list", Tokens::Newline::stringify()), "braced code block") } );
                        errored = done = true;
                }
                break;
            case 77:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                        stack.emplace_back(127, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CBrace::stringify(), "implementation body") } );
                        errored = done = true;
                }
                break;
            case 78:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a7 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a6 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            auto a5 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a4 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a4 { _a4.span, Tokens::as<Tokens::CParen>(_a4.value) };
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::ParamList>>>(stack).ast);
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a2 { _a2.span, Tokens::as<Tokens::OParen>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a1 { _a1.span, Tokens::as<Tokens::Identifier>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Fun> a0 { _a0.span, Tokens::as<Tokens::Fun>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a5 && a5->span().has() ? Maybe<Location const>(a5->span().get().end) :
                                Maybe<Location const>(a4.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FunctionDecl> push (std::make_unique<ASTNS::FunctionDecl>(file, span, std::move(a5), a1, std::move(a3->params), std::move(a6)));
                            std::unique_ptr<ASTNS::FunctionDecl> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_2, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_2 });
                        }
                        break;
                }
                break;
            case 79:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(file, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_67 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Return>:
                        stack.emplace_back(90, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Var>:
                        stack.emplace_back(87, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional statement list", "indented code block") } );
                        errored = done = true;
                }
                break;
            case 80:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_72, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_72 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(131, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional implicit return value", "braced code block") } );
                        errored = done = true;
                }
                break;
            case 81:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(file, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_67 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Indent>:
                        stack.emplace_back(133, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Return>:
                        stack.emplace_back(90, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Var>:
                        stack.emplace_back(87, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "optional statement list", Tokens::Indent::stringify()), "braced code block") } );
                        errored = done = true;
                }
                break;
            case 82:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::StmtList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_67 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Return>:
                        stack.emplace_back(90, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Var>:
                        stack.emplace_back(87, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 83:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Stmt>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(file, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));

                push->stmts.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_60, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_60 });
                        }
                        break;
                }
                break;
            case 84:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmt>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_6, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_6 });
                        }
                        break;
                }
                break;
            case 85:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ExprStmt>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_6, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_6 });
                        }
                        break;
                }
                break;
            case 86:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::RetStmt>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_6, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_6 });
                        }
                        break;
                }
                break;
            case 87:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(139, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(140, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "variable binding list", "variable declaration") } );
                        errored = done = true;
                }
                break;
            case 88:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "line ending", "expression statement") } );
                        errored = done = true;
                }
                break;
            case 89:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                    case Tokens::index_of<Tokens::Bang>:
                    case Tokens::index_of<Tokens::BoolLit>:
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
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
                            stack.emplace_back(get_goto(NonTerminal::_71, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_71 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional line ending", "expression statement") } );
                        errored = done = true;
                }
                break;
            case 90:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "expression", "line ending"), "return statement") } );
                        errored = done = true;
                }
                break;
            case 91:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_27, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_27 });
                        }
                        break;
                }
                break;
            case 92:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::IfExpr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_26, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_26 });
                        }
                        break;
                }
                break;
            case 93:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::WhileExpr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_26, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_26 });
                        }
                        break;
                }
                break;
            case 94:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_26, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_26 });
                        }
                        break;
                }
                break;
            case 95:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_30, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_30 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoublePipe>:
                        stack.emplace_back(148, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Equal>:
                        stack.emplace_back(147, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 96:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "expression", "if expression") } );
                        errored = done = true;
                }
                break;
            case 97:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "expression", "while loop expression") } );
                        errored = done = true;
                }
                break;
            case 98:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_31, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_31 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleAmper>:
                        stack.emplace_back(151, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 99:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::BangEqual>:
                        stack.emplace_back(152, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_32, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_32 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleEqual>:
                        stack.emplace_back(153, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 100:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_33 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Greater>:
                        stack.emplace_back(155, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::GreaterEqual>:
                        stack.emplace_back(157, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Less>:
                        stack.emplace_back(154, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::LessEqual>:
                        stack.emplace_back(156, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 101:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_34 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(158, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 102:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_35, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_35 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Pipe>:
                        stack.emplace_back(159, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 103:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(160, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_36, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_36 });
                        }
                        break;
                }
                break;
            case 104:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_37, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_37 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleGreater>:
                        stack.emplace_back(161, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::DoubleLess>:
                        stack.emplace_back(162, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 105:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_38, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_38 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(164, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Plus>:
                        stack.emplace_back(163, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 106:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_39 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Percent>:
                        stack.emplace_back(167, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Slash>:
                        stack.emplace_back(166, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(165, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 107:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_40, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_40 });
                        }
                        break;
                    case Tokens::index_of<Tokens::RightArrow>:
                        stack.emplace_back(168, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 108:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_41 });
                        }
                        break;
                }
                break;
            case 109:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "unary expression") } );
                        errored = done = true;
                }
                break;
            case 110:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "unary expression") } );
                        errored = done = true;
                }
                break;
            case 111:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "unary expression") } );
                        errored = done = true;
                }
                break;
            case 112:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(173, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "unary expression", Tokens::Mut::stringify()), "unary expression") } );
                        errored = done = true;
                }
                break;
            case 113:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "unary expression") } );
                        errored = done = true;
                }
                break;
            case 114:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(175, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Period>:
                        stack.emplace_back(176, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 115:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Period>:
                        stack.emplace_back(177, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 116:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(178, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Period>:
                        stack.emplace_back(179, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 117:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_43, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_43 });
                        }
                        break;
                }
                break;
            case 118:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::BoolLit> a0 { _a0.span, Tokens::as<Tokens::BoolLit>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BoolLit> push (std::make_unique<ASTNS::BoolLit>(file, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 119:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::FloatLit> a0 { _a0.span, Tokens::as<Tokens::FloatLit>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FloatLit> push (std::make_unique<ASTNS::FloatLit>(file, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 120:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::IntLit> a0 { _a0.span, Tokens::as<Tokens::IntLit>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::IntLit> push (std::make_unique<ASTNS::IntLit>(file, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 121:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CharLit> a0 { _a0.span, Tokens::as<Tokens::CharLit>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::CharLit> push (std::make_unique<ASTNS::CharLit>(file, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 122:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::StringLit> a0 { _a0.span, Tokens::as<Tokens::StringLit>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::StringLit> push (std::make_unique<ASTNS::StringLit>(file, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 123:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::This> a0 { _a0.span, Tokens::as<Tokens::This>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ThisExpr> push (std::make_unique<ASTNS::ThisExpr>(file, span, a0));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 124:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "expression", "primary expression") } );
                        errored = done = true;
                }
                break;
            case 125:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 126:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Path>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::PathExpr> push (std::make_unique<ASTNS::PathExpr>(file, span, std::move(a0)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_47, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_47 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleColon>:
                        stack.emplace_back(23, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 127:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a5 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CBrace> a5 { _a5.span, Tokens::as<Tokens::CBrace>(_a5.value) };
                            auto _a4 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Dedent> a4 { _a4.span, Tokens::as<Tokens::Dedent>(_a4.value) };
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>>(stack).ast);
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Indent> a2 { _a2.span, Tokens::as<Tokens::Indent>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a1 { _a1.span, Tokens::as<Tokens::Newline>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OBrace> a0 { _a0.span, Tokens::as<Tokens::OBrace>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ImplMemberList> pushitem = std::move(a3);
                            stack.emplace_back(get_goto(NonTerminal::_4, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_4 });
                        }
                        break;
                }
                break;
            case 128:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_72, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_72 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(131, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional implicit return value", "indented code block") } );
                        errored = done = true;
                }
                break;
            case 129:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                        stack.emplace_back(182, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CBrace::stringify(), "braced code block") } );
                        errored = done = true;
                }
                break;
            case 130:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_72, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_72 });
                        }
                        break;
                }
                break;
            case 131:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "expression", "implicit return value") } );
                        errored = done = true;
                }
                break;
            case 132:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_72, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_72 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(131, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional implicit return value", "braced code block") } );
                        errored = done = true;
                }
                break;
            case 133:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::StmtList> push (std::make_unique<ASTNS::StmtList>(file, span, std::vector<std::unique_ptr<ASTNS::Stmt>> {}));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_67, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_67 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Return>:
                        stack.emplace_back(90, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Var>:
                        stack.emplace_back(87, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional statement list", "braced code block") } );
                        errored = done = true;
                }
                break;
            case 134:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Stmt>>>(stack).ast);
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::StmtList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->stmts.push_back(std::move(a1));
                            std::unique_ptr<ASTNS::StmtList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_60, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_60 });
                        }
                        break;
                }
                break;
            case 135:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Stmt>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Stmt> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_59, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_59 });
                        }
                        break;
                }
                break;
            case 136:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "line ending", "variable declaration") } );
                        errored = done = true;
                }
                break;
            case 137:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Comma>:
                        stack.emplace_back(187, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItemList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_57, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_57 });
                        }
                        break;
                }
                break;
            case 138:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItem>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::VarStmtItemList> push (std::make_unique<ASTNS::VarStmtItemList>(file, span, std::vector<std::unique_ptr<ASTNS::VarStmtItem>> {}));

                push->items.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_58, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_58 });
                        }
                        break;
                }
                break;
            case 139:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Colon>:
                        stack.emplace_back(51, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "required type annotation", "variable binding") } );
                        errored = done = true;
                }
                break;
            case 140:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(189, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "variable binding") } );
                        errored = done = true;
                }
                break;
            case 141:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(file, span, std::move(a0)));
                            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_8, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_8 });
                        }
                        break;
                }
                break;
            case 142:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ExprStmt> push (std::make_unique<ASTNS::ExprStmt>(file, span, std::move(a0)));
                            std::unique_ptr<ASTNS::ExprStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_8, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_8 });
                        }
                        break;
                }
                break;
            case 143:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "line ending", "return statement") } );
                        errored = done = true;
                }
                break;
            case 144:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Return> a0 { _a0.span, Tokens::as<Tokens::Return>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::RetStmt> push (std::make_unique<ASTNS::RetStmt>(file, span, nullptr));
                            std::unique_ptr<ASTNS::RetStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_9, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_9 });
                        }
                        break;
                }
                break;
            case 145:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_25, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_25 });
                        }
                        break;
                }
                break;
            case 146:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_25, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_25 });
                        }
                        break;
                }
                break;
            case 147:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "assignment expression", "assignment expression") } );
                        errored = done = true;
                }
                break;
            case 148:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "binary and expression", "binary or expression") } );
                        errored = done = true;
                }
                break;
            case 149:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(194, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "code block", "if expression") } );
                        errored = done = true;
                }
                break;
            case 150:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(194, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "code block", "while loop expression") } );
                        errored = done = true;
                }
                break;
            case 151:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "equality expression", "binary and expression") } );
                        errored = done = true;
                }
                break;
            case 152:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "comparison expression", "equality expression") } );
                        errored = done = true;
                }
                break;
            case 153:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "comparison expression", "equality expression") } );
                        errored = done = true;
                }
                break;
            case 154:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bitwise xor expression", "comparison expression") } );
                        errored = done = true;
                }
                break;
            case 155:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bitwise xor expression", "comparison expression") } );
                        errored = done = true;
                }
                break;
            case 156:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bitwise xor expression", "comparison expression") } );
                        errored = done = true;
                }
                break;
            case 157:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bitwise xor expression", "comparison expression") } );
                        errored = done = true;
                }
                break;
            case 158:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bitwise or expression", "bitwise xor expression") } );
                        errored = done = true;
                }
                break;
            case 159:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bitwise and expression", "bitwise or expression") } );
                        errored = done = true;
                }
                break;
            case 160:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "bit shift expression", "bitwise and expression") } );
                        errored = done = true;
                }
                break;
            case 161:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "addition expression", "bit shift expression") } );
                        errored = done = true;
                }
                break;
            case 162:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "addition expression", "bit shift expression") } );
                        errored = done = true;
                }
                break;
            case 163:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "multiplication expression", "addition expression") } );
                        errored = done = true;
                }
                break;
            case 164:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "multiplication expression", "addition expression") } );
                        errored = done = true;
                }
                break;
            case 165:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "multiplication expression") } );
                        errored = done = true;
                }
                break;
            case 166:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "multiplication expression") } );
                        errored = done = true;
                }
                break;
            case 167:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "multiplication expression") } );
                        errored = done = true;
                }
                break;
            case 168:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(16, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(17, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "type specifier", "type cast expression") } );
                        errored = done = true;
                }
                break;
            case 169:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Tilde> a0 { _a0.span, Tokens::as<Tokens::Tilde>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(file, span, Located<ASTNS::UnaryOperator> { a0.span, ASTNS::UnaryOperator::TILDE }, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                }
                break;
            case 170:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Minus> a0 { _a0.span, Tokens::as<Tokens::Minus>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(file, span, Located<ASTNS::UnaryOperator> { a0.span, ASTNS::UnaryOperator::MINUS }, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                }
                break;
            case 171:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Bang> a0 { _a0.span, Tokens::as<Tokens::Bang>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::UnaryExpr> push (std::make_unique<ASTNS::UnaryExpr>(file, span, Located<ASTNS::UnaryOperator> { a0.span, ASTNS::UnaryOperator::BANG }, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                }
                break;
            case 172:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Amper> a0 { _a0.span, Tokens::as<Tokens::Amper>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::AddrofExpr> push (std::make_unique<ASTNS::AddrofExpr>(file, span, a0, std::move(a1), false));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                }
                break;
            case 173:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "unary expression", "unary expression") } );
                        errored = done = true;
                }
                break;
            case 174:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Star> a0 { _a0.span, Tokens::as<Tokens::Star>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::DerefExpr> push (std::make_unique<ASTNS::DerefExpr>(file, span, a0, std::move(a1)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                }
                break;
            case 175:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CParen>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(file, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_66 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional argument list", "function call expression") } );
                        errored = done = true;
                }
                break;
            case 176:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(220, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "field access expression"), format("expected {} of {}", Tokens::Identifier::stringify(), "method call expression") } );
                        errored = done = true;
                }
                break;
            case 177:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(221, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "field access expression"), format("expected {} of {}", Tokens::Identifier::stringify(), "method call expression") } );
                        errored = done = true;
                }
                break;
            case 178:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CParen>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(file, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_66 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional argument list", "function call expression") } );
                        errored = done = true;
                }
                break;
            case 179:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(223, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Identifier::stringify(), "field access expression"), format("expected {} of {}", Tokens::Identifier::stringify(), "method call expression") } );
                        errored = done = true;
                }
                break;
            case 180:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(224, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "primary expression") } );
                        errored = done = true;
                }
                break;
            case 181:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Dedent>:
                        stack.emplace_back(225, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Dedent::stringify(), "indented code block") } );
                        errored = done = true;
                }
                break;
            case 182:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CBrace> a3 { _a3.span, Tokens::as<Tokens::CBrace>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::StmtList>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OBrace> a0 { _a0.span, Tokens::as<Tokens::OBrace>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(file, span, std::move(a1->stmts), std::move(a2)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_14, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_14 });
                        }
                        break;
                }
                break;
            case 183:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                    case Tokens::index_of<Tokens::Bang>:
                    case Tokens::index_of<Tokens::BoolLit>:
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Caret>:
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
                            stack.emplace_back(get_goto(NonTerminal::_71, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_71 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(38, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Semicolon>:
                        stack.emplace_back(39, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional line ending", "implicit return value") } );
                        errored = done = true;
                }
                break;
            case 184:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                        stack.emplace_back(227, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CBrace::stringify(), "braced code block") } );
                        errored = done = true;
                }
                break;
            case 185:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                    case Tokens::index_of<Tokens::Dedent>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = nullptr;
                            stack.emplace_back(get_goto(NonTerminal::_72, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_72 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(131, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional implicit return value", "braced code block") } );
                        errored = done = true;
                }
                break;
            case 186:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItemList>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Var> a0 { _a0.span, Tokens::as<Tokens::Var>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::VarStmt> push (std::make_unique<ASTNS::VarStmt>(file, span, std::move(a1->items)));
                            std::unique_ptr<ASTNS::VarStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_7, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_7 });
                        }
                        break;
                }
                break;
            case 187:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(139, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Mut>:
                        stack.emplace_back(140, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Comma> a1 { _a1.span, Tokens::as<Tokens::Comma>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItemList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_57, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_57 });
                        }
                        break;
                }
                break;
            case 188:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a0 { _a0.span, Tokens::as<Tokens::Identifier>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(file, span, std::move(a1), false, a0, Maybe<Located<Tokens::Equal>>(), nullptr));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_10 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Equal>:
                        stack.emplace_back(231, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 189:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Colon>:
                        stack.emplace_back(51, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "required type annotation", "variable binding") } );
                        errored = done = true;
                }
                break;
            case 190:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Return> a0 { _a0.span, Tokens::as<Tokens::Return>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::RetStmt> push (std::make_unique<ASTNS::RetStmt>(file, span, std::move(a1)));
                            std::unique_ptr<ASTNS::RetStmt> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_9, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_9 });
                        }
                        break;
                }
                break;
            case 191:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Equal> a1 { _a1.span, Tokens::as<Tokens::Equal>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::AssignmentExpr> push (std::make_unique<ASTNS::AssignmentExpr>(file, span, std::move(a0), Located<ASTNS::AssignOperator> { a1.span, ASTNS::AssignOperator::EQUAL }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_30, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_30 });
                        }
                        break;
                }
                break;
            case 192:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::DoublePipe> a1 { _a1.span, Tokens::as<Tokens::DoublePipe>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ShortCircuitExpr> push (std::make_unique<ASTNS::ShortCircuitExpr>(file, span, std::move(a0), Located<ASTNS::ShortCircuitOperator> { a1.span, ASTNS::ShortCircuitOperator::DOUBLEPIPE }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_31, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_31 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleAmper>:
                        stack.emplace_back(151, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 193:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::If> a0 { _a0.span, Tokens::as<Tokens::If>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(file, span, a0, Maybe<Located<Tokens::Else>>(), std::move(a1), std::move(a2), nullptr));
                            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_28, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_28 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Else>:
                        stack.emplace_back(233, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 194:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Indent>:
                        stack.emplace_back(79, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Indent::stringify(), "indented code block") } );
                        errored = done = true;
                }
                break;
            case 195:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::While> a0 { _a0.span, Tokens::as<Tokens::While>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::WhileExpr> push (std::make_unique<ASTNS::WhileExpr>(file, span, std::move(a1), std::move(a2)));
                            std::unique_ptr<ASTNS::WhileExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_29, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_29 });
                        }
                        break;
                }
                break;
            case 196:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::BangEqual>:
                        stack.emplace_back(152, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::DoubleAmper> a1 { _a1.span, Tokens::as<Tokens::DoubleAmper>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ShortCircuitExpr> push (std::make_unique<ASTNS::ShortCircuitExpr>(file, span, std::move(a0), Located<ASTNS::ShortCircuitOperator> { a1.span, ASTNS::ShortCircuitOperator::DOUBLEAMPER }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_32, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_32 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleEqual>:
                        stack.emplace_back(153, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 197:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::BangEqual> a1 { _a1.span, Tokens::as<Tokens::BangEqual>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::BANGEQUAL }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_33 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Greater>:
                        stack.emplace_back(155, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::GreaterEqual>:
                        stack.emplace_back(157, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Less>:
                        stack.emplace_back(154, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::LessEqual>:
                        stack.emplace_back(156, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 198:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::DoubleEqual> a1 { _a1.span, Tokens::as<Tokens::DoubleEqual>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::DOUBLEEQUAL }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_33, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_33 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Greater>:
                        stack.emplace_back(155, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::GreaterEqual>:
                        stack.emplace_back(157, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Less>:
                        stack.emplace_back(154, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::LessEqual>:
                        stack.emplace_back(156, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 199:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Less> a1 { _a1.span, Tokens::as<Tokens::Less>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::LESS }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_34 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(158, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 200:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Greater> a1 { _a1.span, Tokens::as<Tokens::Greater>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::GREATER }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_34 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(158, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 201:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::LessEqual> a1 { _a1.span, Tokens::as<Tokens::LessEqual>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::LESSEQUAL }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_34 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(158, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 202:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::GreaterEqual> a1 { _a1.span, Tokens::as<Tokens::GreaterEqual>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::GREATEREQUAL }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_34, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_34 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Caret>:
                        stack.emplace_back(158, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 203:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Caret> a1 { _a1.span, Tokens::as<Tokens::Caret>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::CARET }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_35, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_35 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Pipe>:
                        stack.emplace_back(159, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 204:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(160, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Pipe> a1 { _a1.span, Tokens::as<Tokens::Pipe>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::PIPE }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_36, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_36 });
                        }
                        break;
                }
                break;
            case 205:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Amper> a1 { _a1.span, Tokens::as<Tokens::Amper>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::AMPER }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_37, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_37 });
                        }
                        break;
                    case Tokens::index_of<Tokens::DoubleGreater>:
                        stack.emplace_back(161, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::DoubleLess>:
                        stack.emplace_back(162, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 206:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::DoubleGreater> a1 { _a1.span, Tokens::as<Tokens::DoubleGreater>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::DOUBLEGREATER }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_38, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_38 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(164, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Plus>:
                        stack.emplace_back(163, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 207:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::DoubleLess> a1 { _a1.span, Tokens::as<Tokens::DoubleLess>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::DOUBLELESS }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_38, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_38 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(164, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Plus>:
                        stack.emplace_back(163, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 208:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Plus> a1 { _a1.span, Tokens::as<Tokens::Plus>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::PLUS }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_39 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Percent>:
                        stack.emplace_back(167, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Slash>:
                        stack.emplace_back(166, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(165, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 209:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Minus> a1 { _a1.span, Tokens::as<Tokens::Minus>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::MINUS }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_39, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_39 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Percent>:
                        stack.emplace_back(167, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Slash>:
                        stack.emplace_back(166, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(165, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 210:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Star> a1 { _a1.span, Tokens::as<Tokens::Star>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::STAR }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_40, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_40 });
                        }
                        break;
                }
                break;
            case 211:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Slash> a1 { _a1.span, Tokens::as<Tokens::Slash>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::SLASH }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_40, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_40 });
                        }
                        break;
                }
                break;
            case 212:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Percent> a1 { _a1.span, Tokens::as<Tokens::Percent>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::BinaryExpr> push (std::make_unique<ASTNS::BinaryExpr>(file, span, std::move(a0), Located<ASTNS::BinaryOperator> { a1.span, ASTNS::BinaryOperator::PERCENT }, std::move(a2)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_40, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_40 });
                        }
                        break;
                }
                break;
            case 213:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::RightArrow> a1 { _a1.span, Tokens::as<Tokens::RightArrow>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::CastExpr> push (std::make_unique<ASTNS::CastExpr>(file, span, std::move(a2), std::move(a0)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_41, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_41 });
                        }
                        break;
                }
                break;
            case 214:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Mut> a1 { _a1.span, Tokens::as<Tokens::Mut>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Amper> a0 { _a0.span, Tokens::as<Tokens::Amper>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::AddrofExpr> push (std::make_unique<ASTNS::AddrofExpr>(file, span, a0, std::move(a2), true));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_42, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_42 });
                        }
                        break;
                }
                break;
            case 215:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(234, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "function call expression") } );
                        errored = done = true;
                }
                break;
            case 216:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_66 });
                        }
                        break;
                }
                break;
            case 217:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_54, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_54 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Comma>:
                        stack.emplace_back(235, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 218:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Arg>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(file, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));

                push->args.push_back(std::move(a0));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_55, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_55 });
                        }
                        break;
                }
                break;
            case 219:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Arg> push (std::make_unique<ASTNS::Arg>(file, span, std::move(a0)));
                            std::unique_ptr<ASTNS::Arg> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_21, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_21 });
                        }
                        break;
                }
                break;
            case 220:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Period> a1 { _a1.span, Tokens::as<Tokens::Period>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(file, span, std::move(a0), a1, a2));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_44, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_44 });
                        }
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(236, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 221:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Period> a1 { _a1.span, Tokens::as<Tokens::Period>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(file, span, std::move(a0), a1, a2));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_44, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_44 });
                        }
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(237, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 222:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(238, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "function call expression") } );
                        errored = done = true;
                }
                break;
            case 223:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Period> a1 { _a1.span, Tokens::as<Tokens::Period>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::FieldAccessExpr> push (std::make_unique<ASTNS::FieldAccessExpr>(file, span, std::move(a0), a1, a2));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_44, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_44 });
                        }
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(239, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 224:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a2 { _a2.span, Tokens::as<Tokens::CParen>(_a2.value) };
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a0 { _a0.span, Tokens::as<Tokens::OParen>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_46, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_46 });
                        }
                        break;
                }
                break;
            case 225:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a4 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Dedent> a4 { _a4.span, Tokens::as<Tokens::Dedent>(_a4.value) };
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::StmtList>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Indent> a1 { _a1.span, Tokens::as<Tokens::Indent>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a0 { _a0.span, Tokens::as<Tokens::Newline>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a4.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(file, span, std::move(a2->stmts), std::move(a3)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_15, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_15 });
                        }
                        break;
                }
                break;
            case 226:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::PureLocation>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Caret> a0 { _a0.span, Tokens::as<Tokens::Caret>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                a1 && a1->span().has() ? Maybe<Location const>(a1->span().get().end) :
                                Maybe<Location const>(a0.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(a1);
                            stack.emplace_back(get_goto(NonTerminal::_12, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_12 });
                        }
                        break;
                }
                break;
            case 227:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a4 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CBrace> a4 { _a4.span, Tokens::as<Tokens::CBrace>(_a4.value) };
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::StmtList>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a1 { _a1.span, Tokens::as<Tokens::Newline>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OBrace> a0 { _a0.span, Tokens::as<Tokens::OBrace>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a4.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        WARN_BLOCK_NO_INDENT(a0.span, a4.span);std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(file, span, std::move(a2->stmts), std::move(a3)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_14, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_14 });
                        }
                        break;
                }
                break;
            case 228:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Dedent>:
                        stack.emplace_back(240, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::Dedent::stringify(), "braced code block") } );
                        errored = done = true;
                }
                break;
            case 229:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItem>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Comma> a1 { _a1.span, Tokens::as<Tokens::Comma>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItemList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->items.push_back(std::move(a2));
                            std::unique_ptr<ASTNS::VarStmtItemList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_58, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_58 });
                        }
                        break;
                }
                break;
            case 230:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::VarStmtItem>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_56, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_56 });
                        }
                        break;
                }
                break;
            case 231:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "expression", "variable binding") } );
                        errored = done = true;
                }
                break;
            case 232:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a1 { _a1.span, Tokens::as<Tokens::Identifier>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Mut> a0 { _a0.span, Tokens::as<Tokens::Mut>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(file, span, std::move(a2), true, a1, Maybe<Located<Tokens::Equal>>(), nullptr));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_10 });
                        }
                        break;
                    case Tokens::index_of<Tokens::Equal>:
                        stack.emplace_back(242, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 233:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Newline>:
                        stack.emplace_back(194, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", format("either {} or {}", "code block", "if expression"), "if expression") } );
                        errored = done = true;
                }
                break;
            case 234:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a3 { _a3.span, Tokens::as<Tokens::CParen>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a1 { _a1.span, Tokens::as<Tokens::OParen>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::CallExpr> push (std::make_unique<ASTNS::CallExpr>(file, span, std::move(a0), a1, std::move(a2->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_43, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_43 });
                        }
                        break;
                }
                break;
            case 235:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        {
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Comma> a1 { _a1.span, Tokens::as<Tokens::Comma>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_54, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_54 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                }
                break;
            case 236:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CParen>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(file, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_66 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional argument list", "method call expression") } );
                        errored = done = true;
                }
                break;
            case 237:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CParen>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(file, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_66 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional argument list", "method call expression") } );
                        errored = done = true;
                }
                break;
            case 238:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a3 { _a3.span, Tokens::as<Tokens::CParen>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a1 { _a1.span, Tokens::as<Tokens::OParen>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::CallExpr> push (std::make_unique<ASTNS::CallExpr>(file, span, std::move(a0), a1, std::move(a2->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_43, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_43 });
                        }
                        break;
                }
                break;
            case 239:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CParen>:
                        {
                            Maybe<Location const> start, end;
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::ArgList> push (std::make_unique<ASTNS::ArgList>(file, span, std::vector<std::unique_ptr<ASTNS::Arg>> {}));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_66, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_66 });
                        }
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "optional argument list", "method call expression") } );
                        errored = done = true;
                }
                break;
            case 240:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CBrace>:
                        stack.emplace_back(250, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CBrace::stringify(), "braced code block") } );
                        errored = done = true;
                }
                break;
            case 241:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Equal> a2 { _a2.span, Tokens::as<Tokens::Equal>(_a2.value) };
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a0 { _a0.span, Tokens::as<Tokens::Identifier>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a3 && a3->span().has() ? Maybe<Location const>(a3->span().get().end) :
                                Maybe<Location const>(a2.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(file, span, std::move(a1), false, a0, a2, std::move(a3)));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_10 });
                        }
                        break;
                }
                break;
            case 242:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::Amper>:
                        stack.emplace_back(112, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Bang>:
                        stack.emplace_back(111, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::BoolLit>:
                        stack.emplace_back(118, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::CharLit>:
                        stack.emplace_back(121, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::FloatLit>:
                        stack.emplace_back(119, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Identifier>:
                        stack.emplace_back(18, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::If>:
                        stack.emplace_back(96, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::IntLit>:
                        stack.emplace_back(120, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Minus>:
                        stack.emplace_back(110, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OBrace>:
                        stack.emplace_back(76, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::OParen>:
                        stack.emplace_back(124, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Star>:
                        stack.emplace_back(113, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::StringLit>:
                        stack.emplace_back(122, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::This>:
                        stack.emplace_back(123, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::Tilde>:
                        stack.emplace_back(109, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    case Tokens::index_of<Tokens::While>:
                        stack.emplace_back(97, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", "expression", "variable binding") } );
                        errored = done = true;
                }
                break;
            case 243:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Else> a3 { _a3.span, Tokens::as<Tokens::Else>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::If> a0 { _a0.span, Tokens::as<Tokens::If>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(file, span, a0, a3, std::move(a1), std::move(a2), std::move(a4)));
                            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_28, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_28 });
                        }
                        break;
                }
                break;
            case 244:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::IfExpr>>>(stack).ast);
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Else> a3 { _a3.span, Tokens::as<Tokens::Else>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Block>>>(stack).ast);
                            auto a1 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::If> a0 { _a0.span, Tokens::as<Tokens::If>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::IfExpr> push (std::make_unique<ASTNS::IfExpr>(file, span, a0, a3, std::move(a1), std::move(a2), std::move(a4)));
                            std::unique_ptr<ASTNS::IfExpr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_28, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_28 });
                        }
                        break;
                }
                break;
            case 245:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Arg>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Comma> a1 { _a1.span, Tokens::as<Tokens::Comma>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                a2 && a2->span().has() ? Maybe<Location const>(a2->span().get().end) :
                                Maybe<Location const>(a1.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        a0->args.push_back(std::move(a2));
                            std::unique_ptr<ASTNS::ArgList> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_55, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_55 });
                        }
                        break;
                }
                break;
            case 246:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Arg>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) : Maybe<Location const>();
                            Maybe<Location const> end =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().end) : Maybe<Location const>();
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
                            std::unique_ptr<ASTNS::Arg> pushitem = std::move(a0);
                            stack.emplace_back(get_goto(NonTerminal::_53, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_53 });
                        }
                        break;
                }
                break;
            case 247:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(252, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "method call expression") } );
                        errored = done = true;
                }
                break;
            case 248:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(253, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "method call expression") } );
                        errored = done = true;
                }
                break;
            case 249:
                switch (tsv.next().value.index()) {
                    case Tokens::index_of<Tokens::CParen>:
                        stack.emplace_back(254, TokenItem { tsv.next() });
                        tsv.advance();
                        break;
                    default:
                        ERR_UNRECOVERABLE_INVALID_SYNTAX(tsv.next().span, Tokens::stringify_type(tsv.next().value), tsv.prev().span, { format("expected {} of {}", Tokens::CParen::stringify(), "method call expression") } );
                        errored = done = true;
                }
                break;
            case 250:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a6 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CBrace> a6 { _a6.span, Tokens::as<Tokens::CBrace>(_a6.value) };
                            auto _a5 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Dedent> a5 { _a5.span, Tokens::as<Tokens::Dedent>(_a5.value) };
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto a3 (pop_as<ASTItem<std::unique_ptr<ASTNS::StmtList>>>(stack).ast);
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Indent> a2 { _a2.span, Tokens::as<Tokens::Indent>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Newline> a1 { _a1.span, Tokens::as<Tokens::Newline>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OBrace> a0 { _a0.span, Tokens::as<Tokens::OBrace>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a6.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::Block> push (std::make_unique<ASTNS::Block>(file, span, std::move(a3->stmts), std::move(a4)));
                            std::unique_ptr<ASTNS::Block> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_14, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_14 });
                        }
                        break;
                }
                break;
            case 251:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Equal> a3 { _a3.span, Tokens::as<Tokens::Equal>(_a3.value) };
                            auto a2 (pop_as<ASTItem<std::unique_ptr<ASTNS::Type>>>(stack).ast);
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a1 { _a1.span, Tokens::as<Tokens::Identifier>(_a1.value) };
                            auto _a0 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Mut> a0 { _a0.span, Tokens::as<Tokens::Mut>(_a0.value) };
                            Maybe<Location const> start =
                                Maybe<Location const>(a0.span.start);
                            Maybe<Location const> end =
                                a4 && a4->span().has() ? Maybe<Location const>(a4->span().get().end) :
                                Maybe<Location const>(a3.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::VarStmtItem> push (std::make_unique<ASTNS::VarStmtItem>(file, span, std::move(a2), true, a1, a3, std::move(a4)));
                            std::unique_ptr<ASTNS::VarStmtItem> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_10, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_10 });
                        }
                        break;
                }
                break;
            case 252:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a5 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a5 { _a5.span, Tokens::as<Tokens::CParen>(_a5.value) };
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a3 { _a3.span, Tokens::as<Tokens::OParen>(_a3.value) };
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Period> a1 { _a1.span, Tokens::as<Tokens::Period>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(file, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_45 });
                        }
                        break;
                }
                break;
            case 253:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a5 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a5 { _a5.span, Tokens::as<Tokens::CParen>(_a5.value) };
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a3 { _a3.span, Tokens::as<Tokens::OParen>(_a3.value) };
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Period> a1 { _a1.span, Tokens::as<Tokens::Period>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(file, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_45 });
                        }
                        break;
                }
                break;
            case 254:
                switch (tsv.next().value.index()) {
                    default:
                        {
                            auto _a5 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::CParen> a5 { _a5.span, Tokens::as<Tokens::CParen>(_a5.value) };
                            auto a4 (pop_as<ASTItem<std::unique_ptr<ASTNS::ArgList>>>(stack).ast);
                            auto _a3 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::OParen> a3 { _a3.span, Tokens::as<Tokens::OParen>(_a3.value) };
                            auto _a2 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Identifier> a2 { _a2.span, Tokens::as<Tokens::Identifier>(_a2.value) };
                            auto _a1 (pop_as<TokenItem>(stack).tok);
                            Located<Tokens::Period> a1 { _a1.span, Tokens::as<Tokens::Period>(_a1.value) };
                            auto a0 (pop_as<ASTItem<std::unique_ptr<ASTNS::Expr>>>(stack).ast);
                            Maybe<Location const> start =
                                a0 && a0->span().has() ? Maybe<Location const>(a0->span().get().start) :
                                Maybe<Location const>(a1.span.start);
                            Maybe<Location const> end =
                                Maybe<Location const>(a5.span.end);
                            Maybe<Span const> span = start.has() && end.has() ? Span(start.get(), end.get()) : Maybe<Span const>();
        std::unique_ptr<ASTNS::MethodCallExpr> push (std::make_unique<ASTNS::MethodCallExpr>(file, span, std::move(a0), a1, a2, a3, std::move(a4->args)));
                            std::unique_ptr<ASTNS::Expr> pushitem = std::move(push);
                            stack.emplace_back(get_goto(NonTerminal::_45, stack.back().state), ASTItem<decltype(pushitem)>{ std::move(pushitem), NonTerminal::_45 });
                        }
                        break;
                }
                break;
            default:
                report_abort_noh(format("parser reached invalid state {}", stack.back().state));
        }
        // PARSERLOOP END }}}
    }

    template <typename T>
    static T pop_as(std::vector<StackItem> &stack) {
        StackItem item = std::move(stack.back());
        stack.pop_back();

        ASSERT(std::holds_alternative<T>(item.item));
        T &as_t = std::get<T>(item.item);

        return std::move(as_t);
    }

private:
    TokenStreamView &tsv;
    File &file;
    bool done, errored;

    std::vector<StackItem> stack;
};
// }}}
// Parser {{{
Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile) {}

std::unique_ptr<ASTNS::CUB> Parser::parse() {
    TokenStream stream (lexer);
    TokenStreamView stream_view (stream);

    _Parser p (stream_view, sourcefile);

    std::unique_ptr<ASTNS::CUB> ret (p.parse());

    if (stream.errored)
        return nullptr;

    return ret;
}
// }}}
