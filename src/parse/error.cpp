#include "parsestack.h"
#include "message/errmsgs.h"

#include "utils/format.h"

bool error_recovery(errorstate const &e, std::vector<std::string> const &expectations) {
    if (/*single_tok(e, expectations) || */ panic_mode(e, expectations))
        return true;
    else {
        ERR_UNRECOVERABLE_INVALID_SYNTAX(e.olh, e.lasttok, expectations);
        return false;
    }
}
// simple invalid syntax {{{
/* 
static std::vector<stackitem> copy_stack(std::vector<stackitem> const &stack) {
    std::vector<stackitem> tempstack;
    for (auto const &si : stack) {
        tempstack.emplace_back(si.state);
    }
    return tempstack;
}

static bool try_insert(TokenType ty, Parser const &p, Token const &lookahead, std::vector<stackitem> const &stack) {
    // TODO: fix these b/c with new indentation you cant reset the lexer to a previous token
    // Lexer temp_l (lookahead);
    // Parser temp_p (temp_l, p.sourcefile);
    // Token temp_lookahead (lookahead);
    // temp_lookahead.type = ty;

    // auto tempstack (copy_stack(stack));

    // std::unique_ptr<ASTNS::CUB> temp_c (nullptr);

    // return _parse(temp_p, tempstack, true, temp_c, temp_lookahead);
    return false;
}

static bool try_del(Parser const &p, std::vector<stackitem> const &stack) {
    // Lexer temp_l (p.lexer);
    // Parser temp_p (temp_l, p.sourcefile);
    // Token temp_lookahead (temp_p.consume());

    // auto tempstack (copy_stack(stack));

    // std::unique_ptr<ASTNS::CUB> temp_c (nullptr);

    // return _parse(temp_p, tempstack, true, temp_c, temp_lookahead);
    return false;
}

static bool try_sub(TokenType ty, Parser const &p, Token const &lookahead, std::vector<stackitem> const &stack) {
    // Lexer temp_l (p.lexer);
    // Parser temp_p (temp_l, p.sourcefile);
    // Token temp_lookahead (lookahead);
    // temp_lookahead.type = ty;

    // auto tempstack (copy_stack(stack));

    // std::unique_ptr<ASTNS::CUB> temp_c (nullptr);

    // return _parse(temp_p, tempstack, true, temp_c, temp_lookahead);
    return false;
}
struct fix {
    enum class fixtype {
        INSERT,
        REMOVE,
        SUBSTITUTE,
        NOFIX,
    } type;
    TokenType ttype;

    std::string stringify() const {
        switch (type) {
            case fix::fixtype::REMOVE:
                return "implicitly removed token";

            case fix::fixtype::SUBSTITUTE:
                return format("implicitly replaced token with {}", stringify_token_type(ttype));

            case fix::fixtype::INSERT:
                return format("implicitly inserted {} before token", stringify_token_type(ttype));

            default:
                report_abort_noh("invalid fix type");
        }
    }
};

static void apply_fix(fix const &f, Parser &p, Token &lookahead) {
    switch (f.type) {
        case fix::fixtype::REMOVE:
            lookahead = p.consume();
            break;

        case fix::fixtype::SUBSTITUTE:
            lookahead.type = f.ttype;
            break;

        case fix::fixtype::INSERT:
            // TODO: fix
            // p.lexer.reset_to_tok(lookahead);
            lookahead.type = f.ttype;
            break;

        default:
            report_abort_noh("attempt to apply invalid fix type");
    }
}

bool single_tok(errorstate const &e, std::vector<std::string> const &expectations) {
    fix bestfix = fix {fix::fixtype::NOFIX, static_cast<TokenType>(-1)};
    auto score = [](fix const &f) {
        switch (f.type) {
            case fix::fixtype::REMOVE:
                return 0;
            case fix::fixtype::SUBSTITUTE:
                return 1;
            case fix::fixtype::INSERT:
                return 2;
            case fix::fixtype::NOFIX:
                return -1;
            default:
                report_abort_noh("invalid fix type");
        }
    };

    // error recovery {{{
    // SINGLETOK START
    #define TRYINSERT(ty) if (try_insert(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::INSERT, ty}; if (score(f) > score(bestfix)) bestfix = f;}
    #define TRYSUB(ty) if (try_sub(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::SUBSTITUTE, ty}; if (score(f) > score(bestfix)) bestfix = f;}
    #define TRYTOKTY(ty) TRYINSERT(ty); TRYSUB(ty);
    TRYTOKTY(Tokens::_EOF)
    TRYTOKTY(Tokens::Comma)
    TRYTOKTY(Tokens::Fun)
    TRYTOKTY(Tokens::Identifier)
    TRYTOKTY(Tokens::OParen)
    TRYTOKTY(Tokens::CParen)
    TRYTOKTY(Tokens::Impl)
    TRYTOKTY(Tokens::OBrace)
    TRYTOKTY(Tokens::CBrace)
    TRYTOKTY(Tokens::Newline)
    TRYTOKTY(Tokens::Indent)
    TRYTOKTY(Tokens::Dedent)
    TRYTOKTY(Tokens::Var)
    TRYTOKTY(Tokens::Dollar)
    TRYTOKTY(Tokens::Return)
    TRYTOKTY(Tokens::Equal)
    TRYTOKTY(Tokens::Mut)
    TRYTOKTY(Tokens::Semicolon)
    TRYTOKTY(Tokens::Star)
    TRYTOKTY(Tokens::This)
    TRYTOKTY(Tokens::Colon)
    TRYTOKTY(Tokens::If)
    TRYTOKTY(Tokens::Else)
    TRYTOKTY(Tokens::While)
    TRYTOKTY(Tokens::DoublePipe)
    TRYTOKTY(Tokens::DoubleAmper)
    TRYTOKTY(Tokens::BangEqual)
    TRYTOKTY(Tokens::DoubleEqual)
    TRYTOKTY(Tokens::Less)
    TRYTOKTY(Tokens::Greater)
    TRYTOKTY(Tokens::LessEqual)
    TRYTOKTY(Tokens::GreaterEqual)
    TRYTOKTY(Tokens::Caret)
    TRYTOKTY(Tokens::Pipe)
    TRYTOKTY(Tokens::Amper)
    TRYTOKTY(Tokens::DoubleGreater)
    TRYTOKTY(Tokens::DoubleLess)
    TRYTOKTY(Tokens::Plus)
    TRYTOKTY(Tokens::Minus)
    TRYTOKTY(Tokens::Slash)
    TRYTOKTY(Tokens::Percent)
    TRYTOKTY(Tokens::RightArrow)
    TRYTOKTY(Tokens::Tilde)
    TRYTOKTY(Tokens::Bang)
    TRYTOKTY(Tokens::Period)
    TRYTOKTY(Tokens::BoolLit)
    TRYTOKTY(Tokens::FloatLit)
    TRYTOKTY(Tokens::IntLit)
    TRYTOKTY(Tokens::CharLit)
    TRYTOKTY(Tokens::StringLit)
    TRYTOKTY(Tokens::DoubleColon)
    if (try_del(e.p, e.stack)) {fix f = fix {fix::fixtype::REMOVE, static_cast<TokenType>(-1)}; if (score(f) > score(bestfix)) bestfix = f;};
    // SINGLETOK END
    // }}}

    if (bestfix.type != fix::fixtype::NOFIX) {
        apply_fix(bestfix, e.p, e.lookahead);
        ERR_SIMPLE_INVALID_SYNTAX(e.olh, e.lasttok, bestfix.stringify(), expectations);
        return true;
    }

    return false;
}
}}} */

bool panic_mode(errorstate const &e, std::vector<std::string> const &expectations) {
    // error recovery {{{
    // PANIC MODE START
    #define CHECKASI(ty)\
        if (nterm == NonTerminal::ty) {\
            switch (e.lookahead.index()) {
    #define FINISHCHECKASI()\
            }\
        }
    #define RECOVERANDDEFBREAK()\
            valid = true;\
            delto = i;\
            break;\
        default:\
            break;
    bool valid = false;
    e.lookahead = e.p.consume(); // prevent infinite panicking loops
    std::vector<stackitem>::reverse_iterator delto;
    while (!valid) {
        for (auto i = e.stack.rbegin(); i != e.stack.rend() && !valid; ++i) {
            if (std::holds_alternative<astitem>(i->item)) {
                NonTerminal nterm = std::get<astitem>(i->item).nt;
                CHECKASI(_49)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_21)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_51)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_50)
                        case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_52)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_20)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_54)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_53)
                        case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_55)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_57)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_56)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_58)
                        case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_6)
                        case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_59)
                        case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_60)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::_EOF>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_1)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::_EOF>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_61)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::_EOF>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_62)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_5)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_63)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_7)
                        case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_2)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::_EOF>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_3)
                        case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::_EOF>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_12)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>: case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::Dollar>: case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::Else>: case Token::index_of<Tokens::_EOF>: case Token::index_of<Tokens::CParen>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_4)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>: case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::_EOF>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_8)
                        case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_9)
                        case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_13)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>: case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::Dollar>: case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::Else>: case Token::index_of<Tokens::_EOF>: case Token::index_of<Tokens::CParen>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_14)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>: case Token::index_of<Tokens::Fun>: case Token::index_of<Tokens::Impl>: case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::Dollar>: case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::Else>: case Token::index_of<Tokens::_EOF>: case Token::index_of<Tokens::CParen>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_23)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_22)
                        case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_27)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>: case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::Dollar>: case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(_28)
                        case Token::index_of<Tokens::Newline>: case Token::index_of<Tokens::Semicolon>: case Token::index_of<Tokens::Var>: case Token::index_of<Tokens::Return>: case Token::index_of<Tokens::OBrace>: case Token::index_of<Tokens::If>: case Token::index_of<Tokens::While>: case Token::index_of<Tokens::Tilde>: case Token::index_of<Tokens::Minus>: case Token::index_of<Tokens::Bang>: case Token::index_of<Tokens::Amper>: case Token::index_of<Tokens::Star>: case Token::index_of<Tokens::BoolLit>: case Token::index_of<Tokens::FloatLit>: case Token::index_of<Tokens::IntLit>: case Token::index_of<Tokens::CharLit>: case Token::index_of<Tokens::StringLit>: case Token::index_of<Tokens::This>: case Token::index_of<Tokens::OParen>: case Token::index_of<Tokens::Identifier>: case Token::index_of<Tokens::Dollar>: case Token::index_of<Tokens::Comma>: case Token::index_of<Tokens::CParen>: case Token::index_of<Tokens::CBrace>: case Token::index_of<Tokens::Dedent>:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
            }
        }
        if (!valid)
            e.lookahead = e.p.consume();
        if (e.lookahead.is<Tokens::_EOF>())
            return false;
    }
    e.stack.erase(delto.base(), e.stack.end());
    #undef CHECKASI
    #undef FINISHCHECKASI
    #undef RECOVERANDDEFBREAK
    ERR_PANICKING_INVALID_SYNTAX(e.olh, e.lasttok, e.lookahead, expectations);
    return true;
    // PANIC MODE END
    // }}}
}
