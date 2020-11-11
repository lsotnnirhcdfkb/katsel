#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>
#include <stack>

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile) {}

// get goto {{{
// GETGOTO START

// The following code was autogenerated - see the utils/ directory
template <> size_t Parser::getGoto<ASTNS::New_stmt>(size_t state)
{
    switch (state)
    {
        case 0:
             return 1;
        default:
            reportAbortNoh("retrieve goto of nonterminal new_stmt in state 22");
    }
}
template <> size_t Parser::getGoto<ASTNS::New_expr>(size_t state)
{
    switch (state)
    {
        case 0:
             return 2;
        case 10:
             return 17;
        default:
            reportAbortNoh("retrieve goto of nonterminal new_expr in state 22");
    }
}
template <> size_t Parser::getGoto<ASTNS::Add>(size_t state)
{
    switch (state)
    {
        case 0:
             return 3;
        case 10:
             return 3;
        default:
            reportAbortNoh("retrieve goto of nonterminal add in state 22");
    }
}
template <> size_t Parser::getGoto<ASTNS::Mult>(size_t state)
{
    switch (state)
    {
        case 0:
             return 4;
        case 10:
             return 4;
        case 11:
             return 18;
        case 12:
             return 19;
        default:
            reportAbortNoh("retrieve goto of nonterminal mult in state 22");
    }
}
template <> size_t Parser::getGoto<ASTNS::Unary>(size_t state)
{
    switch (state)
    {
        case 0:
             return 5;
        case 6:
             return 15;
        case 7:
             return 16;
        case 10:
             return 5;
        case 11:
             return 5;
        case 12:
             return 5;
        case 13:
             return 20;
        case 14:
             return 21;
        default:
            reportAbortNoh("retrieve goto of nonterminal unary in state 22");
    }
}
template <> size_t Parser::getGoto<ASTNS::Primary>(size_t state)
{
    switch (state)
    {
        case 0:
             return 8;
        case 6:
             return 8;
        case 7:
             return 8;
        case 10:
             return 8;
        case 11:
             return 8;
        case 12:
             return 8;
        case 13:
             return 8;
        case 14:
             return 8;
        default:
            reportAbortNoh("retrieve goto of nonterminal primary in state 22");
    }
}
// This code was autogenerated - see the utils/ directory

// GETGOTO END
// }}}

std::unique_ptr<ASTNS::NewBaseAST> Parser::parse()
{
    struct stackitem
    {
        int state;
        stackitem(size_t state): state(state) {}
        virtual void dummy() {}
    };

    struct tokstackitem : public stackitem
    {
        Token tok;
        tokstackitem(size_t state, Token tok): stackitem(state), tok(tok) {}
    };
    
    struct aststackitem : public stackitem
    {
        std::unique_ptr<ASTNS::NewBaseAST> ast;
        aststackitem(size_t state, std::unique_ptr<ASTNS::NewBaseAST> ast): stackitem(state), ast(std::move(ast)) {}
    };

    // parser loop {{{
    // PARSERLOOP START

// The following code was autogenerated - see the utils/ directory
#define SHIFT(newstate) \
    lasttok = lookahead;\
    stack.push(std::make_unique<tokstackitem>(newstate, lasttok));\
    lookahead = consume();
#define REDUCET(n) \
    std::unique_ptr<stackitem> _a ## n = std::move(stack.top()); stack.pop();\
    tokstackitem *si ## n = dynamic_cast<tokstackitem*>(_a ## n .get());\
    Token a ## n (si ## n ->tok);
#define REDUCEA(n) \
    std::unique_ptr<stackitem> _a ## n = std::move(stack.top()); stack.pop();\
    aststackitem *si ## n = dynamic_cast<aststackitem*>(_a ## n .get());\
    std::unique_ptr<ASTNS::NewBaseAST> a ## n (std::move(si ## n ->ast));
#define SHIFTON(ty, n) \
    case ty: \
        {SHIFT(n)} break;
#define DEFAULTINVALID2(justparsed, expected) \
    default: \
        invalidSyntax(justparsed, expected, lookahead, lasttok);\
        done = true;\
        break;
#define DEFAULTINVALID3(justparsed, expected, whileparsing) \
    default: \
        invalidSyntax(justparsed, expected, whileparsing, lookahead, lasttok);\
        done = true;\
        break;
#define REDUCESKIP(cl) \
    {\
        std::unique_ptr<stackitem> popped (std::move(stack.top())); stack.pop();\
        aststackitem *asi = dynamic_cast<aststackitem*>(popped.get());\
        size_t newstate = getGoto<ASTNS::cl>(stack.top()->state);\
        stack.push(std::make_unique<aststackitem>(newstate, std::move(asi->ast)));\
    }
    bool done = false;
    Token lookahead (consume());
    Token lasttok;
    std::stack<std::unique_ptr<stackitem>> stack;
    stack.push(std::make_unique<stackitem>(0));
    while (!done)
    {
        switch (stack.top()->state)
        {
            case 0:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("beginning", "statement", "entire thing")
                }
                break;
            case 1:
               switch (lookahead.type)
               {
                    case TokenType::EOF_:
                            done = true;
                        break;
                    DEFAULTINVALID3("statement", "nothing", "entire thing")
                }
                break;
            case 2:
               switch (lookahead.type)
               {
                    case TokenType::EOF_:
                        {
                            REDUCEA(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::New_stmt>(std::move(a0));
                            size_t newstate = getGoto<ASTNS::New_stmt>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("expression", "nothing", "statement")
                }
                break;
            case 3:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                        {
                            REDUCEA(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::New_expr>(std::move(a0));
                            size_t newstate = getGoto<ASTNS::New_expr>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    SHIFTON(TokenType::MINUS, 12)
                    SHIFTON(TokenType::PLUS, 11)
                    DEFAULTINVALID2("addition expression", "either TokenType::MINUS or TokenType::PLUS")
                }
                break;
            case 4:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                        REDUCESKIP(Add);
                        break;
                    SHIFTON(TokenType::SLASH, 14)
                    SHIFTON(TokenType::STAR, 13)
                    DEFAULTINVALID2("multiplication expression", "either TokenType::SLASH or TokenType::STAR")
                }
                break;
            case 5:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        REDUCESKIP(Mult);
                        break;
                    DEFAULTINVALID3("unary expression", "nothing", "multiplication expression")
                }
                break;
            case 6:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::MINUS", "unary expression", "unary expression")
                }
                break;
            case 7:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::TILDE", "unary expression", "unary expression")
                }
                break;
            case 8:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        REDUCESKIP(Unary);
                        break;
                    DEFAULTINVALID3("primary expression", "nothing", "unary expression")
                }
                break;
            case 9:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        {
                            REDUCET(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Primary>(std::move(a0));
                            size_t newstate = getGoto<ASTNS::Primary>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("TokenType::DECINTLIT", "nothing", "primary expression")
                }
                break;
            case 10:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::OPARN", "expression", "primary expression")
                }
                break;
            case 11:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::PLUS", "multiplication expression", "addition expression")
                }
                break;
            case 12:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::MINUS", "multiplication expression", "addition expression")
                }
                break;
            case 13:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::STAR", "unary expression", "multiplication expression")
                }
                break;
            case 14:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::DECINTLIT, 9)
                    SHIFTON(TokenType::MINUS, 6)
                    SHIFTON(TokenType::OPARN, 10)
                    SHIFTON(TokenType::TILDE, 7)
                    DEFAULTINVALID3("TokenType::SLASH", "unary expression", "multiplication expression")
                }
                break;
            case 15:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        {
                            REDUCEA(1)
                            REDUCET(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Unary>(std::move(a0), std::move(a1));
                            size_t newstate = getGoto<ASTNS::Unary>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("unary expression", "nothing", "unary expression")
                }
                break;
            case 16:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        {
                            REDUCEA(1)
                            REDUCET(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Unary>(std::move(a0), std::move(a1));
                            size_t newstate = getGoto<ASTNS::Unary>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("unary expression", "nothing", "unary expression")
                }
                break;
            case 17:
               switch (lookahead.type)
               {
                    SHIFTON(TokenType::CPARN, 22)
                    DEFAULTINVALID3("expression", "TokenType::CPARN", "primary expression")
                }
                break;
            case 18:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                        {
                            REDUCEA(2)
                            REDUCET(1)
                            REDUCEA(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Add>(std::move(a0), std::move(a1), std::move(a2));
                            size_t newstate = getGoto<ASTNS::Add>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    SHIFTON(TokenType::SLASH, 14)
                    SHIFTON(TokenType::STAR, 13)
                    DEFAULTINVALID2("multiplication expression", "either TokenType::SLASH or TokenType::STAR")
                }
                break;
            case 19:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                        {
                            REDUCEA(2)
                            REDUCET(1)
                            REDUCEA(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Add>(std::move(a0), std::move(a1), std::move(a2));
                            size_t newstate = getGoto<ASTNS::Add>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    SHIFTON(TokenType::SLASH, 14)
                    SHIFTON(TokenType::STAR, 13)
                    DEFAULTINVALID2("multiplication expression", "either TokenType::SLASH or TokenType::STAR")
                }
                break;
            case 20:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        {
                            REDUCEA(2)
                            REDUCET(1)
                            REDUCEA(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Mult>(std::move(a0), std::move(a1), std::move(a2));
                            size_t newstate = getGoto<ASTNS::Mult>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("unary expression", "nothing", "multiplication expression")
                }
                break;
            case 21:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        {
                            REDUCEA(2)
                            REDUCET(1)
                            REDUCEA(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Mult>(std::move(a0), std::move(a1), std::move(a2));
                            size_t newstate = getGoto<ASTNS::Mult>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("unary expression", "nothing", "multiplication expression")
                }
                break;
            case 22:
               switch (lookahead.type)
               {
                    case TokenType::CPARN:
                    case TokenType::EOF_:
                    case TokenType::MINUS:
                    case TokenType::PLUS:
                    case TokenType::SLASH:
                    case TokenType::STAR:
                        {
                            REDUCET(2)
                            REDUCEA(1)
                            REDUCET(0)
                            std::unique_ptr<ASTNS::NewBaseAST> push = std::make_unique<ASTNS::Primary>(std::move(a0), std::move(a1), std::move(a2));
                            size_t newstate = getGoto<ASTNS::Primary>(stack.top()->state);
                            stack.push(std::make_unique<aststackitem>(newstate, std::move(push)));
                        }
                        break;
                    DEFAULTINVALID3("TokenType::CPARN", "nothing", "primary expression")
                }
                break;
            default:
                Error(Error::MsgType::INTERR, lookahead, "Parser reached invalid state")
                    .primary(Error::Primary(lookahead)
                        .error(static_cast<std::stringstream&>(std::stringstream() << "Parser reached invalid state: " << stack.top()->state).str()))
                    .reportAbort();
        }
#undef SHIFT
#undef REDUCET
#undef REDUCEA
#undef REDUCESKIP
    }
// This code was autogenerated - see the utils/ directory

    // PARSERLOOP END
    // }}}

    aststackitem *asir (dynamic_cast<aststackitem*>(stack.top().get()));
    if (!asir)
        return nullptr;

    return std::move(asir->ast);
}

Token Parser::consume()
{
    Token cur;
    while (true)
    {
        cur = lexer.nextToken();
        if (cur.type != TokenType::ERROR) return cur;

        Error(Error::MsgType::ERROR, cur, cur.message)
            .primary(Error::Primary(cur)
                .error(cur.message))
            .report();
    }

    return cur;
}

void Parser::invalidSyntax(const char *justparsed, const char *expected, const char *whileparsing, Token const &lookahead, Token const &last)
{
    std::stringstream ssl;
    std::stringstream sss;
    ssl << "expected " << expected << " after " << justparsed << " of " << whileparsing << ", but got " << stringifyTokenType(lookahead.type) << " instead";
    sss << "expected " << expected;
    Error(Error::MsgType::ERROR, lookahead, ssl.str())
        .primary(Error::Primary(last)
            .error(sss.str()))
        .primary(Error::Primary(lookahead)
            .note("unexpected token here"))
        .report();
}
void Parser::invalidSyntax(const char *justparsed, const char *expected, Token const &lookahead, Token const &last)
{
    std::stringstream ssl;
    std::stringstream sss;
    ssl << "expected " << expected << " after " << justparsed << ", but got " << stringifyTokenType(lookahead.type) << " instead";
    sss << "expected " << expected;
    Error(Error::MsgType::ERROR, lookahead, ssl.str())
        .primary(Error::Primary(last)
            .error(sss.str()))
        .primary(Error::Primary(lookahead)
            .note("unexpected token here"))
        .report();
}
