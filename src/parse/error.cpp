#include "parsestack.h"
#include "message/errmsgs.h"

#include "utils/format.h"

bool errorRecovery(errorstate const &e, std::vector<std::string> const &expectations)
{
    if (singleTok(e, expectations) || panicMode(e, expectations))
        return true;
    else
    {
        ERR_UNRECOVERABLE_INVALID_SYNTAX(e.lasttok, e.olh, expectations);
        return false;
    }
}

// helper functions {{{
static std::vector<stackitem> copyStack(std::vector<stackitem> const &stack)
{
    std::vector<stackitem> tempstack;
    for (auto const &si : stack)
    {
        tempstack.emplace_back(si.state);
    }
    return tempstack;
}

static bool tryInsert(TokenType ty, Parser const &p, Token const &lookahead, std::vector<stackitem> const &stack)
{
    // TODO: fix these b/c with new indentation you cant reset the lexer to a previous token
    // Lexer tempL (lookahead);
    // Parser tempP (tempL, p.sourcefile);
    // Token tempLookahead (lookahead);
    // tempLookahead.type = ty;

    // auto tempstack (copyStack(stack));

    // std::unique_ptr<ASTNS::CUB> tempC (nullptr);

    // return _parse(tempP, tempstack, true, tempC, tempLookahead);
    return false;
}

static bool tryDel(Parser const &p, std::vector<stackitem> const &stack)
{
    // Lexer tempL (p.lexer);
    // Parser tempP (tempL, p.sourcefile);
    // Token tempLookahead (tempP.consume());

    // auto tempstack (copyStack(stack));

    // std::unique_ptr<ASTNS::CUB> tempC (nullptr);

    // return _parse(tempP, tempstack, true, tempC, tempLookahead);
    return false;
}

static bool trySub(TokenType ty, Parser const &p, Token const &lookahead, std::vector<stackitem> const &stack)
{
    // Lexer tempL (p.lexer);
    // Parser tempP (tempL, p.sourcefile);
    // Token tempLookahead (lookahead);
    // tempLookahead.type = ty;

    // auto tempstack (copyStack(stack));

    // std::unique_ptr<ASTNS::CUB> tempC (nullptr);

    // return _parse(tempP, tempstack, true, tempC, tempLookahead);
    return false;
}
// }}}
struct fix
{
    enum class fixtype
    {
        INSERT,
        REMOVE,
        SUBSTITUTE,
        NOFIX,
    } type;
    TokenType ttype;

    std::string stringify() const
    {
        switch (type)
        {
            case fix::fixtype::REMOVE:
                return "implicitly removed token";

            case fix::fixtype::SUBSTITUTE:
                return format("implicitly replaced token with %", stringifyTokenType(ttype));

            case fix::fixtype::INSERT:
                return format("implicitly inserted % before token", stringifyTokenType(ttype));

            default:
                reportAbortNoh("invalid fix type");
        }
    }
};

static void applyFix(fix const &f, Parser &p, Token &lookahead)
{
    switch (f.type)
    {
        case fix::fixtype::REMOVE:
            lookahead = p.consume();
            break;

        case fix::fixtype::SUBSTITUTE:
            lookahead.type = f.ttype;
            break;

        case fix::fixtype::INSERT:
            p.lexer.resetToTok(lookahead);
            lookahead.type = f.ttype;
            break;

        default:
            reportAbortNoh("attempt to apply invalid fix type");
    }
}

bool singleTok(errorstate const &e, std::vector<std::string> const &expectations)
{
    fix bestfix = fix {fix::fixtype::NOFIX, static_cast<TokenType>(-1)};
    auto score = [](fix const &f)
    {
        switch (f.type)
        {
            case fix::fixtype::REMOVE:
                return 0;
            case fix::fixtype::SUBSTITUTE:
                return 1;
            case fix::fixtype::INSERT:
                return 2;
            case fix::fixtype::NOFIX:
                return -1;
            default:
                reportAbortNoh("invalid fix type");
        }
    };

    // error recovery {{{
    // SINGLETOK START

// The following code was autogenerated - see the utils/ directory
#define TRYINSERT(ty) if (tryInsert(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::INSERT, ty}; if (score(f) > score(bestfix)) bestfix = f;}
#define TRYSUB(ty) if (trySub(ty, e.p, e.lookahead, e.stack)) {fix f = fix {fix::fixtype::SUBSTITUTE, ty}; if (score(f) > score(bestfix)) bestfix = f;}
#define TRYTOKTY(ty) TRYINSERT(ty); TRYSUB(ty);
    TRYTOKTY(TokenType::EOF_)
    TRYTOKTY(TokenType::COMMA)
    TRYTOKTY(TokenType::FUN)
    TRYTOKTY(TokenType::IDENTIFIER)
    TRYTOKTY(TokenType::OPARN)
    TRYTOKTY(TokenType::CPARN)
    TRYTOKTY(TokenType::NEWLINE)
    TRYTOKTY(TokenType::VAR)
    TRYTOKTY(TokenType::RETURN)
    TRYTOKTY(TokenType::EQUAL)
    TRYTOKTY(TokenType::OCURB)
    TRYTOKTY(TokenType::CCURB)
    TRYTOKTY(TokenType::INDENT)
    TRYTOKTY(TokenType::DEDENT)
    TRYTOKTY(TokenType::SEMICOLON)
    TRYTOKTY(TokenType::VOID)
    TRYTOKTY(TokenType::UINT8)
    TRYTOKTY(TokenType::UINT16)
    TRYTOKTY(TokenType::UINT32)
    TRYTOKTY(TokenType::UINT64)
    TRYTOKTY(TokenType::SINT8)
    TRYTOKTY(TokenType::SINT16)
    TRYTOKTY(TokenType::SINT32)
    TRYTOKTY(TokenType::SINT64)
    TRYTOKTY(TokenType::FLOAT)
    TRYTOKTY(TokenType::BOOL)
    TRYTOKTY(TokenType::DOUBLE)
    TRYTOKTY(TokenType::CHAR)
    TRYTOKTY(TokenType::QUESTION)
    TRYTOKTY(TokenType::COLON)
    TRYTOKTY(TokenType::DOUBLEPIPE)
    TRYTOKTY(TokenType::DOUBLEAMPER)
    TRYTOKTY(TokenType::BANGEQUAL)
    TRYTOKTY(TokenType::DOUBLEEQUAL)
    TRYTOKTY(TokenType::LESS)
    TRYTOKTY(TokenType::GREATER)
    TRYTOKTY(TokenType::LESSEQUAL)
    TRYTOKTY(TokenType::GREATEREQUAL)
    TRYTOKTY(TokenType::CARET)
    TRYTOKTY(TokenType::PIPE)
    TRYTOKTY(TokenType::AMPER)
    TRYTOKTY(TokenType::DOUBLEGREATER)
    TRYTOKTY(TokenType::DOUBLELESS)
    TRYTOKTY(TokenType::PLUS)
    TRYTOKTY(TokenType::MINUS)
    TRYTOKTY(TokenType::STAR)
    TRYTOKTY(TokenType::SLASH)
    TRYTOKTY(TokenType::PERCENT)
    TRYTOKTY(TokenType::TILDE)
    TRYTOKTY(TokenType::BANG)
    TRYTOKTY(TokenType::TRUELIT)
    TRYTOKTY(TokenType::FALSELIT)
    TRYTOKTY(TokenType::FLOATLIT)
    TRYTOKTY(TokenType::NULLPTRLIT)
    TRYTOKTY(TokenType::DECINTLIT)
    TRYTOKTY(TokenType::OCTINTLIT)
    TRYTOKTY(TokenType::BININTLIT)
    TRYTOKTY(TokenType::HEXINTLIT)
    TRYTOKTY(TokenType::CHARLIT)
    TRYTOKTY(TokenType::STRINGLIT)
    if (tryDel(e.p, e.stack)) {fix f = fix {fix::fixtype::REMOVE, static_cast<TokenType>(-1)}; if (score(f) > score(bestfix)) bestfix = f;};
// This code was autogenerated - see the utils/ directory

    // SINGLETOK END
    // }}}

    if (bestfix.type != fix::fixtype::NOFIX)
    {
        applyFix(bestfix, e.p, e.lookahead);
        ERR_SIMPLE_INVALID_SYNTAX(e.lasttok, e.olh, bestfix.stringify(), expectations);
        return true;
    }

    return false;
}

bool panicMode(errorstate const &e, std::vector<std::string> const &expectations)
{
    // error recovery {{{
    // PANIC MODE START

// The following code was autogenerated - see the utils/ directory
#define CHECKASI(ty)\
    ASTNS::ty *ast##ty (dynamic_cast<ASTNS::ty*>(ast));\
    if (ast##ty)\
    {\
        switch (e.lookahead.type)\
        {
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
    while (!valid)
    {
        for (auto i = e.stack.rbegin(); i != e.stack.rend() && !valid; ++i)
        {
            if (!i->istok && !i->isinitial)
            {
                ASTNS::AST *ast = i->ast.get();
                CHECKASI(AnotherParam)
                        case TokenType::COMMA: case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(Param)
                        case TokenType::COMMA: case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(ParamSegment)
                        case TokenType::COMMA: case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(ParamList)
                        case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(AnotherArg)
                        case TokenType::COMMA: case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(Arg)
                        case TokenType::COMMA: case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(ArgSegment)
                        case TokenType::COMMA: case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(ArgList)
                        case TokenType::CPARN:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(AnotherVarStmtItem)
                        case TokenType::COMMA: case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(VarStmtItemSegment)
                        case TokenType::COMMA: case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(VarStmtItemList)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(AnotherStmt)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(Stmt)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(StmtSegment)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(StmtList)
                        case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(AnotherDecl)
                        case TokenType::FUN: case TokenType::EOF_:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(Decl)
                        case TokenType::FUN: case TokenType::EOF_:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(DeclList)
                        case TokenType::FUN: case TokenType::EOF_:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(FunctionDecl)
                        case TokenType::FUN: case TokenType::EOF_:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(Block)
                        case TokenType::FUN: case TokenType::EOF_:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(VarStmt)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(ExprStmt)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(RetExpr)
                        case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::COMMA: case TokenType::COLON: case TokenType::CPARN: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(BracedBlock)
                        case TokenType::FUN: case TokenType::NEWLINE: case TokenType::SEMICOLON: case TokenType::COMMA: case TokenType::COLON: case TokenType::CPARN: case TokenType::EOF_: case TokenType::CCURB: case TokenType::DEDENT:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
                CHECKASI(IndentedBlock)
                        case TokenType::FUN: case TokenType::EOF_:
                            RECOVERANDDEFBREAK()
                FINISHCHECKASI()
            }
        }
        if (!valid)
            e.lookahead = e.p.consume();
        if (e.lookahead.type == TokenType::EOF_)
            return false;
    }
    e.stack.erase(delto.base(), e.stack.end());
#undef CHECKASI
#undef FINISHCHECKASI
#undef RECOVERANDDEFBREAK
    ERR_PANICKING_INVALID_SYNTAX(e.lasttok, e.olh, e.lookahead, expectations);
    return true;
// This code was autogenerated - see the utils/ directory

    // PANIC MODE END
    // }}}
}
