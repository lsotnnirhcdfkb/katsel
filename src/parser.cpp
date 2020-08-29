/// @file parser.cpp
/// Parser method definitions
/// Parser converts a Token stream into a vector of ASTs.

#include "parser.h"

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile), PANICK(false) 
{
    advance(); // get first token
    prevToken.type = TokenType::SOF;
}

// {{{ parser parsing methods
std::unique_ptr<ASTs::AST> Parser::parse()
{
    std::vector<std::unique_ptr<ASTs::AST>> programV;

    while (!atEnd()) // if there is no expression
    {
        if (PANICK)
        {
            calmDown();
            syncTokens();
        }

        if (atEnd()) // if syncTokens reached the end
            break;

        std::unique_ptr<ASTs::AST> declast = decl();

        // if panicing then this ast
        // has an error and something
        // could be nullptr or the ast
        // is malformed so dont add it
        if (declast && !PANICK) programV.push_back(std::move(declast));
    }

    consume(TokenType::EOF_, "Expected EOF token at end of file (internal compiling error)");

    std::unique_ptr<ASTs::ProgramAST> program = std::make_unique<ASTs::ProgramAST>(programV);
    return program;
}
// {{{ declarations
std::unique_ptr<ASTs::AST> Parser::decl()
{
    return function();
}

std::unique_ptr<ASTs::AST> Parser::function()
{
    std::unique_ptr<ASTs::AST> rettype = type();
    Token name = consume(TokenType::IDENTIFIER, "Expected identifier to denote function name after return type");

    consume(TokenType::OPARN, "Expected '(' after function name");
    std::unique_ptr<ASTs::AST> fparamlist;
    if (!check(TokenType::CPARN))
        fparamlist = paramlist();
    else
        fparamlist = nullptr;

    consume(TokenType::CPARN, "Expected ')' after paramument list");

    std::unique_ptr<ASTs::AST> fblock;
    if (check(TokenType::OCURB))
    { // if not then this is just a declaration
        fblock = block();
    } else
    {
        consume(TokenType::SEMICOLON, "Expected ';' after function declaration");
    }

    std::unique_ptr<ASTs::FunctionAST> func = std::make_unique<ASTs::FunctionAST>(std::move(rettype), name, std::move(fparamlist), std::move(fblock));
    return func;
}
// }}}
// {{{ statements
std::unique_ptr<ASTs::AST> Parser::statement()
{
    std::unique_ptr<ASTs::AST> statementast = nullptr;
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
std::unique_ptr<ASTs::AST> Parser::varstatement()
{
    advance(); // consume the var keyword
    std::unique_ptr<ASTs::AST> typeast = type();

    Token name = consume(TokenType::IDENTIFIER, "Expected identifier as variable name");

    std::unique_ptr<ASTs::AST> expressionast = nullptr;
    if (match(TokenType::EQUAL))
    {
        expressionast = expression();
    }

    consume(TokenType::SEMICOLON, "Expected semicolon after var statement");

    std::unique_ptr<ASTs::VarStmtAST> stmtast = std::make_unique<ASTs::VarStmtAST>(std::move(typeast), name, std::move(expressionast));
    return stmtast;
}
std::unique_ptr<ASTs::AST> Parser::exprstatement()
{
    std::unique_ptr<ASTs::AST> expr = expression();
    consume(TokenType::SEMICOLON, "Expected semicolon after expression statement");
    return std::make_unique<ASTs::ExprStmtAST>(std::move(expr));
}
std::unique_ptr<ASTs::AST> Parser::retstatement()
{
    advance(); // consume the return keyword
    std::unique_ptr<ASTs::AST> expr;
    if (!check(TokenType::SEMICOLON))
        expr = expression();
    consume(TokenType::SEMICOLON, "Expected semicolon after return statement");

    return std::make_unique<ASTs::ReturnStmtAST>(std::move(expr));
}
// }}}
// {{{ expression
std::unique_ptr<ASTs::AST> Parser::expression()
{
    return assignmentexpr();
}

std::unique_ptr<ASTs::AST> Parser::assignmentexpr()
{
    std::unique_ptr<ASTs::AST> lhs = ternaryexpr(); // should be VariableRefAST if it is a valid lhs
    
    if (match(TokenType::EQUAL))
    {
        if (!(dynamic_cast<ASTs::LValueAST*>(&*lhs)))
            error("Invalid assignment target");

        Token equalSign = prev();
        std::unique_ptr<ASTs::AST> rhs = assignmentexpr();
        std::unique_ptr<ASTs::AssignAST> assignast = std::make_unique<ASTs::AssignAST>(std::move(lhs), std::move(rhs), equalSign);

        return assignast;
    }

    return lhs;
}
// {{{ ternary expr
std::unique_ptr<ASTs::AST> Parser::ternaryexpr()
{
    std::unique_ptr<ASTs::AST> binexpr = binorexpr();

    if (match(TokenType::QUESTION))
    {
        std::unique_ptr<ASTs::AST> trueexpr = binorexpr();
        consume(TokenType::COLON, "Expect colon after first expression");
        std::unique_ptr<ASTs::AST> falseexpr = ternaryexpr();

        std::unique_ptr<ASTs::TernaryOpAST> ternast = std::make_unique<ASTs::TernaryOpAST>(std::move(binexpr), std::move(trueexpr), std::move(falseexpr));

        return ternast;
    }

    return binexpr;
}
// }}}
// {{{ binary and or not
std::unique_ptr<ASTs::AST> Parser::binorexpr()
{
    std::unique_ptr<ASTs::AST> lnode = binandexpr();
    while (match(TokenType::DOUBLEPIPE) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = binandexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::binandexpr()
{
    std::unique_ptr<ASTs::AST> lnode = binnotexpr();
    while (match(TokenType::DOUBLEAMPER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = binnotexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::binnotexpr()
{
    if (match(TokenType::BANG))
    {
        std::unique_ptr<ASTs::AST> binnot = binnotexpr();
        std::unique_ptr<ASTs::UnaryAST> node = std::make_unique<ASTs::UnaryAST>(prev(), std::move(binnot));
        return node;
    }

    return compeqexpr();
}
// }}}
// {{{ comparison
std::unique_ptr<ASTs::AST> Parser::compeqexpr()
{
    std::unique_ptr<ASTs::AST> lnode = complgtexpr();
    while (match(TokenType::DOUBLEEQUAL) || match(TokenType::BANGEQUAL) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = complgtexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::complgtexpr()
{
    std::unique_ptr<ASTs::AST> lnode = bitxorexpr();
    while (match(TokenType::LESS) || match(TokenType::GREATER) || match(TokenType::LESSEQUAL) || match(TokenType::GREATEREQUAL) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = bitxorexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}
// }}}
// {{{ bit xor or and shift
std::unique_ptr<ASTs::AST> Parser::bitxorexpr()
{
    std::unique_ptr<ASTs::AST> lnode = bitorexpr();
    while (match(TokenType::CARET) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = bitorexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::bitorexpr()
{
    std::unique_ptr<ASTs::AST> lnode = bitandexpr();
    while (match(TokenType::PIPE) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = bitandexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::bitandexpr()
{
    std::unique_ptr<ASTs::AST> lnode = bitshiftexpr();
    while (match(TokenType::AMPER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = bitshiftexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::bitshiftexpr()
{
    std::unique_ptr<ASTs::AST> lnode = additionexpr();
    while (match(TokenType::DOUBLELESS) || match(TokenType::DOUBLEGREATER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = additionexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}
// }}}
// {{{ add mult unary
std::unique_ptr<ASTs::AST> Parser::additionexpr()
{
    std::unique_ptr<ASTs::AST> lnode = multexpr();

    while (match(TokenType::PLUS) || match(TokenType::MINUS) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = multexpr();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::multexpr()
{
    std::unique_ptr<ASTs::AST> lnode = unary();

    while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> rnode = unary();

        std::unique_ptr<ASTs::BinaryAST> pnode = std::make_unique<ASTs::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTs::AST> Parser::unary()
{
    if (match(TokenType::TILDE) || match(TokenType::MINUS))
    {
        Token op = prev();
        std::unique_ptr<ASTs::AST> unaryno = unary();

        std::unique_ptr<ASTs::UnaryAST> node = std::make_unique<ASTs::UnaryAST>(op, std::move(unaryno));

        return node;
    }

    return primary();
}
// }}}
// {{{ primary
std::unique_ptr<ASTs::AST> Parser::primary()
{
    if (match(TokenType::TRUELIT) || match(TokenType::FALSELIT) ||
            match(TokenType::FLOATLIT) ||
            match(TokenType::DECINTLIT) || match(TokenType::OCTINTLIT) || match(TokenType::BININTLIT) || match(TokenType::HEXINTLIT) ||
            match(TokenType::CHARLIT) || match(TokenType::STRINGLIT))
    {
        return std::make_unique<ASTs::PrimaryAST>(prev());
    }

    if (match(TokenType::OPARN))
    {
        std::unique_ptr<ASTs::AST> expr = expression();
        consume(TokenType::CPARN, "Expect ')' after expression");
        return expr;
    }

    return lvalue();
}
// }}}
// }}}
// {{{ parsing helping rules
std::unique_ptr<ASTs::AST> Parser::paramlist()
{
    std::vector<std::unique_ptr<ASTs::AST>> params;
    std::unique_ptr<ASTs::AST> paramtype = type();
    Token paramname = consume(TokenType::IDENTIFIER, "Expected paramuemnt name");

    std::unique_ptr<ASTs::AST> param = std::make_unique<ASTs::ParamAST>(std::move(paramtype), paramname);

    params.push_back(std::move(param));

    while (match(TokenType::COMMA) && !atEnd())
    {
        std::unique_ptr<ASTs::AST> cparamtype = type();
        Token cparamname = consume(TokenType::IDENTIFIER, "Expected paramuemnt name");

        std::unique_ptr<ASTs::AST> cparam = std::make_unique<ASTs::ParamAST>(std::move(cparamtype), cparamname);

        params.push_back(std::move(cparam));
    }

    std::unique_ptr<ASTs::AST> paramsast = std::make_unique<ASTs::ParamsAST>(params);
    return paramsast;
}
std::unique_ptr<ASTs::AST> Parser::arglist()
{
    std::vector<std::unique_ptr<ASTs::AST>> args;

    std::unique_ptr<ASTs::AST> arg = std::make_unique<ASTs::ArgAST>(expression());

    args.push_back(std::move(arg));

    while (match(TokenType::COMMA) && !atEnd())
    {
        std::unique_ptr<ASTs::AST> cargexpr = expression();

        std::unique_ptr<ASTs::AST> carg = std::make_unique<ASTs::ArgAST>(std::move(cargexpr));

        args.push_back(std::move(carg));
    }

    std::unique_ptr<ASTs::AST> argsast = std::make_unique<ASTs::ArgsAST>(args);
    return argsast;
}

std::unique_ptr<ASTs::AST> Parser::block()
{
    consume(TokenType::OCURB, "Expected '{' to open block");

    std::vector<std::unique_ptr<ASTs::AST>> statements;

    while (!check(TokenType::CCURB) && !atEnd())
    {
        std::unique_ptr<ASTs::AST> statementast = statement();
        if (statementast)
            statements.push_back(std::move(statementast));
    }

    consume(TokenType::CCURB, "Expected '}' to close block");

    std::unique_ptr<ASTs::AST> b = std::make_unique<ASTs::BlockAST>(statements);
    return b;
}

std::unique_ptr<ASTs::AST> Parser::type()
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
        return std::make_unique<ASTs::TypeAST>(prev());
    }

    error("Expected type", true);
    return nullptr;
}

// }}}
// {{{ lvalues
std::unique_ptr<ASTs::AST> Parser::lvalue()
{
    Token iden = consume(TokenType::IDENTIFIER, "Expected identifier");
    std::unique_ptr<ASTs::AST> lvalueast = std::make_unique<ASTs::LValueAST>(std::make_unique<ASTs::VariableRefAST>(iden));

    // merged call rule and lvalue rule into one
    // except really all I did was keep it the way it was before
    while (match(TokenType::OPARN))
    {
        Token oparn = prev();
        std::unique_ptr<ASTs::AST> arglistast;
        if (!check(TokenType::CPARN))
             arglistast = arglist();

        consume(TokenType::CPARN, "Expected closing parentheses after argument list");

        lvalueast = std::make_unique<ASTs::CallAST>(std::move(lvalueast), std::move(arglistast), oparn);
    }

    return lvalueast;
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
    while (!(check(TokenType::SEMICOLON) || check(TokenType::CCURB)) && !atEnd()) advance(); // advance until next token is semicolon

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
