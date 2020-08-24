#include "parser.h"

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile), PANICK(false) 
{
    advance(); // get first token
    prevToken.type = TokenType::SOF;
}

// {{{ parser parsing methods
std::unique_ptr<AST> Parser::parse()
{
    std::vector<std::unique_ptr<AST>> programV;

    while (!atEnd()) // if there is no expression
    {
        if (PANICK)
        {
            calmDown();
            syncTokens();
        }

        if (atEnd()) // if syncTokens reached the end
            break;

        std::unique_ptr<AST> declast = decl();

        // if panicing then this ast
        // has an error and something
        // could be nullptr or the ast
        // is malformed so dont add it
        if (declast && !PANICK) programV.push_back(std::move(declast));
    }

    consume(TokenType::EOF_, "Expected EOF token at end of file (internal compiling error)");

    std::unique_ptr<ProgramAST> program = std::make_unique<ProgramAST>(programV);
    return program;
}
// {{{ declarations
std::unique_ptr<AST> Parser::decl()
{
    return function();
}

std::unique_ptr<AST> Parser::function()
{
    std::unique_ptr<AST> rettype = type();
    Token name = consume(TokenType::IDENTIFIER, "Expected identifier to denote function name after return type");

    consume(TokenType::OPARN, "Expected '(' after function name");
    std::unique_ptr<AST> fparamlist;
    if (!check(TokenType::CPARN))
        fparamlist = paramlist();
    else
        fparamlist = nullptr;

    consume(TokenType::CPARN, "Expected ')' after paramument list");

    std::unique_ptr<AST> fblock = block();

    std::unique_ptr<FunctionAST> func = std::make_unique<FunctionAST>(std::move(rettype), name, std::move(fparamlist), std::move(fblock));
    return func;
}
// }}}
// {{{ statements
std::unique_ptr<AST> Parser::statement()
{
    std::unique_ptr<AST> statementast = nullptr;
    switch (peek().type)
    {
        case TokenType::VAR:
            statementast = varstatement();
            break;

        case TokenType::OCURB:
            statementast = block();
            break;

        case TokenType::RETURN:
            statementast = retstatement();
            break;

        default:
            statementast = exprstatement();

    }
    return statementast;
}
std::unique_ptr<AST> Parser::varstatement()
{
    advance(); // consume the var keyword
    std::unique_ptr<AST> typeast = type();

    Token name = consume(TokenType::IDENTIFIER, "Expected identifier as variable name");

    std::unique_ptr<AST> expressionast = nullptr;
    if (match(TokenType::EQUAL))
    {
        expressionast = expression();
    }

    consume(TokenType::SEMICOLON, "Expected semicolon after var statement");

    std::unique_ptr<VarStmtAST> stmtast = std::make_unique<VarStmtAST>(std::move(typeast), name, std::move(expressionast));
    return stmtast;
}
std::unique_ptr<AST> Parser::exprstatement()
{
    std::unique_ptr<AST> expr = expression();
    consume(TokenType::SEMICOLON, "Expected semicolon after expression statement");
    return std::make_unique<ExprStmtAST>(std::move(expr));
}
std::unique_ptr<AST> Parser::retstatement()
{
    advance(); // consume the return keyword
    std::unique_ptr<AST> expr;
    if (!check(TokenType::SEMICOLON))
        expr = expression();
    consume(TokenType::SEMICOLON, "Expected semicolon after return statement");

    return std::make_unique<ReturnStmtAST>(std::move(expr));
}
// }}}
// {{{ expression
std::unique_ptr<AST> Parser::expression()
{
    return assignmentexpr();
}

std::unique_ptr<AST> Parser::assignmentexpr()
{
    std::unique_ptr<AST> lhs = ternaryexpr(); // should be VariableRefAST if it is a valid lhs
    
    if (match(TokenType::EQUAL))
    {
        if (!(dynamic_cast<VariableRefAST*>(&*lhs)))
            error("Invalid assignment tparamet");

        Token equalSign = prev();
        std::unique_ptr<AST> rhs = assignmentexpr();
        std::unique_ptr<AssignAST> assignast = std::make_unique<AssignAST>(std::move(lhs), std::move(rhs), equalSign);

        return assignast;
    }

    return lhs;
}
// {{{ ternary expr
std::unique_ptr<AST> Parser::ternaryexpr()
{
    std::unique_ptr<AST> binexpr = binorexpr();

    if (match(TokenType::QUESTION))
    {
        std::unique_ptr<AST> trueexpr = binorexpr();
        consume(TokenType::COLON, "Expect colon after first expression");
        std::unique_ptr<AST> falseexpr = ternaryexpr();

        std::unique_ptr<TernaryOpAST> ternast = std::make_unique<TernaryOpAST>(std::move(binexpr), std::move(trueexpr), std::move(falseexpr));

        return ternast;
    }

    return binexpr;
}
// }}}
// {{{ binary and or not
std::unique_ptr<AST> Parser::binorexpr()
{
    std::unique_ptr<AST> lnode = binandexpr();
    while (match(TokenType::DOUBLEPIPE) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = binandexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::binandexpr()
{
    std::unique_ptr<AST> lnode = binnotexpr();
    while (match(TokenType::DOUBLEAMPER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = binnotexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::binnotexpr()
{
    if (match(TokenType::BANG))
    {
        std::unique_ptr<AST> binnot = binnotexpr();
        std::unique_ptr<UnaryAST> node = std::make_unique<UnaryAST>(prev(), std::move(binnot));
        return node;
    }

    return compeqexpr();
}
// }}}
// {{{ comparison
std::unique_ptr<AST> Parser::compeqexpr()
{
    std::unique_ptr<AST> lnode = complgtexpr();
    while (match(TokenType::DOUBLEEQUAL) || match(TokenType::BANGEQUAL) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = complgtexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::complgtexpr()
{
    std::unique_ptr<AST> lnode = bitxorexpr();
    while (match(TokenType::LESS) || match(TokenType::GREATER) || match(TokenType::LESSEQUAL) || match(TokenType::GREATEREQUAL) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitxorexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}
// }}}
// {{{ bit xor or and shift
std::unique_ptr<AST> Parser::bitxorexpr()
{
    std::unique_ptr<AST> lnode = bitorexpr();
    while (match(TokenType::CARET) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitorexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitorexpr()
{
    std::unique_ptr<AST> lnode = bitandexpr();
    while (match(TokenType::PIPE) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitandexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitandexpr()
{
    std::unique_ptr<AST> lnode = bitshiftexpr();
    while (match(TokenType::AMPER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = bitshiftexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::bitshiftexpr()
{
    std::unique_ptr<AST> lnode = additionexpr();
    while (match(TokenType::DOUBLELESS) || match(TokenType::DOUBLEGREATER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = additionexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}
// }}}
// {{{ add mult unary
std::unique_ptr<AST> Parser::additionexpr()
{
    std::unique_ptr<AST> lnode = multexpr();

    while (match(TokenType::PLUS) || match(TokenType::MINUS) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = multexpr();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::multexpr()
{
    std::unique_ptr<AST> lnode = unary();

    while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<AST> rnode = unary();

        std::unique_ptr<BinaryAST> pnode = std::make_unique<BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<AST> Parser::unary()
{
    if (match(TokenType::TILDE) || match(TokenType::MINUS))
    {
        Token op = prev();
        std::unique_ptr<AST> unaryno = unary();

        std::unique_ptr<UnaryAST> node = std::make_unique<UnaryAST>(op, std::move(unaryno));

        return node;
    }

    return primary();
}
// }}}
// {{{ primary
std::unique_ptr<AST> Parser::primary()
{
    if (match(TokenType::TRUELIT) || match(TokenType::FALSELIT) ||
            match(TokenType::FLOATLIT) ||
            match(TokenType::DECINTLIT) || match(TokenType::OCTINTLIT) || match(TokenType::BININTLIT) || match(TokenType::HEXINTLIT) ||
            match(TokenType::CHARLIT) || match(TokenType::STRINGLIT))
    {
        return std::make_unique<PrimaryAST>(prev());
    }

    if (match(TokenType::OPARN))
    {
        std::unique_ptr<AST> expr = expression();
        consume(TokenType::CPARN, "Expect ')' after expression");
        return expr;
    }

    if (match(TokenType::IDENTIFIER))
    {
        return varref();
    }

    error("Expected expression", true);
    return nullptr;
}
// }}}
// }}}
// {{{ parsing helping rules
std::unique_ptr<AST> Parser::paramlist()
{
    std::vector<std::unique_ptr<AST>> params;
    std::unique_ptr<AST> paramtype = type();
    Token paramname = consume(TokenType::IDENTIFIER, "Expected paramuemnt name");

    std::unique_ptr<AST> param = std::make_unique<ParamAST>(std::move(paramtype), paramname);

    params.push_back(std::move(param));

    while (match(TokenType::COMMA) && !atEnd())
    {
        std::unique_ptr<AST> cparamtype = type();
        Token cparamname = consume(TokenType::IDENTIFIER, "Expected paramuemnt name");

        std::unique_ptr<AST> cparam = std::make_unique<ParamAST>(std::move(cparamtype), cparamname);

        params.push_back(std::move(cparam));
    }

    std::unique_ptr<AST> paramsast = std::make_unique<ParamsAST>(params);
    return paramsast;
}
std::unique_ptr<AST> Parser::arglist()
{
    std::vector<std::unique_ptr<AST>> args;

    std::unique_ptr<AST> arg = std::make_unique<ArgAST>(expression());

    args.push_back(std::move(arg));

    while (match(TokenType::COMMA) && !atEnd())
    {
        std::unique_ptr<AST> cargexpr = expression();

        std::unique_ptr<AST> carg = std::make_unique<ArgAST>(std::move(cargexpr));

        args.push_back(std::move(carg));
    }

    std::unique_ptr<AST> argsast = std::make_unique<ArgsAST>(args);
    return argsast;
}
std::unique_ptr<AST> Parser::block()
{
    consume(TokenType::OCURB, "Expected '{' to open block");

    std::vector<std::unique_ptr<AST>> statements;

    while (!check(TokenType::CCURB) && !atEnd())
    {
        std::unique_ptr<AST> statementast = statement();
        if (statementast)
            statements.push_back(std::move(statementast));
    }

    consume(TokenType::CCURB, "Expected '}' to close block");

    std::unique_ptr<AST> b = std::make_unique<BlockAST>(statements);
    return b;
}
std::unique_ptr<AST> Parser::type()
{
    // only builtin types for now
    if (match(TokenType::UINT8) || match(TokenType::UINT16) || match(TokenType::UINT32) || match(TokenType::UINT64) ||
        match(TokenType::SINT8) || match(TokenType::SINT16) || match(TokenType::SINT32) || match(TokenType::SINT64) ||

        match(TokenType::FLOAT) ||
        match(TokenType::CHAR) ||
        match(TokenType::BOOL) ||
        match(TokenType::DOUBLE) ||
        match(TokenType::VOID))
    {
        return std::make_unique<TypeAST>(prev());
    }

    error("Expected type", true);
    return nullptr;
}

std::unique_ptr<AST> Parser::varref()
{
    Token iden = consume(TokenType::IDENTIFIER, "Expected identifier");
    std::unique_ptr<AST> varrefast = std::make_unique<VariableRefAST>(iden);

    while (match(TokenType::OPARN))
    {
        std::unique_ptr<AST> arglistast;
        if (!check(TokenType::CPARN))
             arglistast = arglist();

        consume(TokenType::CPARN, "Expected closing parentheses for argument list");

        varrefast = std::make_unique<CallAST>(std::move(varrefast), std::move(arglistast));
    }

    return varrefast;
}
// }}}
// }}}
// {{{ parser helper methods
Token& Parser::peek()
{
    return currToken;
}

Token& Parser::prev()
{
    return prevToken;
}

void Parser::advance()
{
    prevToken = currToken;
    bool nextTokenIter = true;

    while (true)
    {
        if (nextTokenIter)
            currToken = lexer.nextToken();

        nextTokenIter = true;

        if (currToken.type != TokenType::ERROR) break; // continue loop if it is an error token

        // if it is an error token then report error
        // also do not advance in this loop because error advances automatically
        error(currToken.message, true);
        nextTokenIter = false;
    }
}

Token& Parser::consume(TokenType type, std::string message)
{
    if (match(type))
    {
        return prev();
    }

    error(message);
    return prev();
}

bool Parser::match(TokenType type)
{
    if (check(type))
    {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type)
{
    return peek().type == type;
}

bool Parser::atEnd()
{
    return peek().type == TokenType::EOF_;
}

void Parser::panic()
{
    // PANICCCCC!!!!!!!
    // ERROR DETECTED!!!!!!!! :)

    PANICK = true;
}

void Parser::calmDown()
{
    // Well I guess the error is out of sight

    PANICK = false;
}

void Parser::syncTokens()
{
    while (!check(TokenType::SEMICOLON) && !atEnd()) advance(); // advance until next token is semicolon

    if (check(TokenType::SEMICOLON))
        advance(); // consume semicolon

    // if doesnt advance then peek is of type eof
}

void Parser::error(std::string const msg, bool nextT)
{
    if (!PANICK)
    {
        Token &badToken = prev();

        if (prev().type == TokenType::SOF || nextT)
            badToken = peek();

        reportError(badToken, msg, sourcefile);
        panic();
    }
    advance(); // to prevent infinite loops
}

// }}}
