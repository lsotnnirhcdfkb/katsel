#pragma once
#include <vector>
#include <memory>
#include "lex/token.h"
#include "parse/parser.h"

struct stackitem
{
    inline stackitem(size_t state)                                 : type(type::INITIAL), state(state) {}
    inline stackitem(size_t state, Token const &t)                 : type(type::TOKEN)  , state(state), as(t) {}
    inline stackitem(size_t state, std::unique_ptr<ASTNS::AST> ast): type(type::AST)    , state(state), as(std::move(ast)) {}
    inline stackitem(stackitem &other)                             : type(other.type)
    {
        switch (other.type)
        {
            case type::INITIAL: new (&as) data();               break;
            case type::TOKEN:   new (&as) data(other.as.token); break;
            case type::AST:     new (&as) data(other.as.ast);   break;
        }
    }
    inline stackitem(stackitem &&other)                            : stackitem(other) {}
    inline ~stackitem()
    {
        as.free(type);
    }

    enum class type
    {
        INITIAL,
        TOKEN,
        AST,
    } type;

    int state;

    union data
    {
        struct asTok
        {
            Token tok;

            asTok(Token const &tok): tok(tok) {}
            asTok(asTok const &other): tok(other.tok) {}
            ~asTok() = default;
        } token;
        struct asAST
        {
            std::unique_ptr<ASTNS::AST> ast;

            asAST(std::unique_ptr<ASTNS::AST> ast): ast(std::move(ast)) {}
            asAST(asAST &other): ast(std::move(other.ast)) {}
            asAST(asAST &&other): ast(std::move(other.ast)) {}
            ~asAST() = default;
        } ast;
        struct asInitial {} initial;

        data(): initial() {}
        data(Token const &tok): token(tok) {}
        data(std::unique_ptr<ASTNS::AST> ast): ast(std::move(ast)) {}

        data(asTok const &othertok): token(othertok) {}
        data(asAST &otherast): ast(otherast) {}

        ~data() {}
        void free(enum type type)
        {
            if (type == type::AST)
                ast.~asAST();
        }
    } as;
};

struct errorstate
{
    Parser &p;
    std::vector<stackitem> &stack;
    Token const &lasttok;
    Token &lookahead;
    Token const olh;

    inline errorstate(Parser &p, std::vector<stackitem> &stack, Token &lasttok, Token &lookahead) : p(p), stack(stack), lasttok(lasttok), lookahead(lookahead), olh(lookahead) {}
};

bool errorRecovery(errorstate const &e, std::vector<std::string> const &expectations);
bool singleTok(errorstate const &e, std::vector<std::string> const &expectations);
bool panicMode(errorstate const &e, std::vector<std::string> const &expectations);

template <typename AST>
size_t getGoto(size_t state);
bool _parse(Parser &p, std::vector<stackitem> &stack, bool istrial, std::unique_ptr<ASTNS::CUB> &out, Token const &_lookahead);
