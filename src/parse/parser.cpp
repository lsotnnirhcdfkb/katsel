#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile) {}

std::unique_ptr<ASTNS::AST> Parser::parse()
{
    struct stackitem
    {
        int state;
        bool isTok;
        union
        {
            Token t;
            std::unique_ptr<ASTNS::AST> ast;
        } data;
    };

    Token first (consume());
}

Token Parser::consume()
{
    while (true)
    {
        Token cur (lexer.nextToken());

        if (cur.type != TokenType::ERROR) return cur;

        Error(Error::MsgType::ERROR, cur, cur.message)
            .primary(Error::Primary(cur)
                .error(cur.message))
            .report();
    }
}

Parser::Action Parser::getAction(size_t state, Token lookahead)
{
    // GETACTION START

// The following code was autogenerated - see the utils/ directory
    switch (state)
    {
        case 0:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                case TokenType::DECINTLIT:
                    return Parser::Action();
                case TokenType::OPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 1:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 2:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 3:
            switch (lookahead.type)
            {
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 4:
            switch (lookahead.type)
            {
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 5:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 6:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 7:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 8:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 9:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 10:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                case TokenType::DECINTLIT:
                    return Parser::Action();
                case TokenType::OPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 11:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                case TokenType::DECINTLIT:
                    return Parser::Action();
                case TokenType::OPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 12:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                case TokenType::DECINTLIT:
                    return Parser::Action();
                case TokenType::OPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 13:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                case TokenType::DECINTLIT:
                    return Parser::Action();
                case TokenType::OPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 14:
            switch (lookahead.type)
            {
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::TILDE:
                    return Parser::Action();
                case TokenType::DECINTLIT:
                    return Parser::Action();
                case TokenType::OPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 15:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 16:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 17:
            switch (lookahead.type)
            {
                case TokenType::CPARN:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 18:
            switch (lookahead.type)
            {
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 19:
            switch (lookahead.type)
            {
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 20:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 21:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        case 22:
            switch (lookahead.type)
            {
                case TokenType::EOF_:
                    return Parser::Action();
                case TokenType::CPARN:
                    return Parser::Action();
                case TokenType::PLUS:
                    return Parser::Action();
                case TokenType::MINUS:
                    return Parser::Action();
                case TokenType::STAR:
                    return Parser::Action();
                case TokenType::SLASH:
                    return Parser::Action();
                default:
                    return Parser::Action();
            }
            break;
        default:
            Error(Error::MsgType::INTERR, lookahead, "Parser reached invalid state")                                                     
                .primary(Error::Primary(lookahead)                                                                                       
                    .error(static_cast<std::stringstream&>(std::stringstream() << "Parser reached invalid state: " << state).str()))     
                .reportAbort();
    }
// This code was autogenerated - see the utils/ directory

    // GETACTION END
}
