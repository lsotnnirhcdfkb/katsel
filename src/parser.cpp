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
std::unique_ptr<ASTNS::Program> Parser::parse()
{
    std::vector<std::unique_ptr<ASTNS::Decl>> programV;

    while (!atEnd()) // if there is no expression
    {
        if (PANICK)
        {
            calmDown();
            syncTokens();
        }

        if (atEnd()) // if syncTokens reached the end
            break;

        std::unique_ptr<ASTNS::Decl> declast = decl();

        // if panicing then this ast
        // has an error and something
        // could be nullptr or the ast
        // is malformed so dont add it
        if (declast && !PANICK) programV.push_back(std::move(declast));
    }

    consume(TokenType::EOF_, "Expected EOF token at end of file (internal compiling error)");

    std::unique_ptr<ASTNS::Program> program = std::make_unique<ASTNS::Program>(programV);
    return program;
}
// {{{ declarations
std::unique_ptr<ASTNS::Decl> Parser::decl()
{
    return function();
}

std::unique_ptr<ASTNS::Decl> Parser::function()
{
    std::unique_ptr<ASTNS::Type> rettype = type();
    Token name = consume(TokenType::IDENTIFIER, "Expected identifier to denote function name after return type");

    consume(TokenType::OPARN, "Expected '(' after function name");
    std::unique_ptr<ASTNS::Param> fparamlist;
    if (!check(TokenType::CPARN))
        fparamlist = param();
    else
        fparamlist = nullptr;

    consume(TokenType::CPARN, "Expected ')' after paramument list");

    std::unique_ptr<ASTNS::BlockStmt> fblock;
    if (check(TokenType::OCURB))
    { // if not then this is just a declaration
        fblock = block();
    }
    else
    {
        consume(TokenType::SEMICOLON, "Expected ';' after function declaration");
    }

    std::unique_ptr<ASTNS::FunctionDecl> func = std::make_unique<ASTNS::FunctionDecl>(std::move(rettype), name, std::move(fparamlist), std::move(fblock));
    return func;
}
// }}}
// {{{ statements
std::unique_ptr<ASTNS::AST> Parser::statement()
{
    std::unique_ptr<ASTNS::AST> statementast = nullptr;
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
std::unique_ptr<ASTNS::AST> Parser::varstatement()
{
    advance(); // consume the var keyword
    std::unique_ptr<ASTNS::AST> typeast = type();

    Token name = consume(TokenType::IDENTIFIER, "Expected identifier as variable name");

    std::unique_ptr<ASTNS::AST> expressionast = nullptr;
    if (match(TokenType::EQUAL))
    {
        expressionast = expression();
    }

    consume(TokenType::SEMICOLON, "Expected semicolon after var statement");

    std::unique_ptr<ASTNS::VarStmtAST> stmtast = std::make_unique<ASTNS::VarStmtAST>(std::move(typeast), name, std::move(expressionast));
    return stmtast;
}
std::unique_ptr<ASTNS::AST> Parser::exprstatement()
{
    std::unique_ptr<ASTNS::AST> expr = expression();
    consume(TokenType::SEMICOLON, "Expected semicolon after expression statement");
    return std::make_unique<ASTNS::ExprStmtAST>(std::move(expr));
}
std::unique_ptr<ASTNS::AST> Parser::retstatement()
{
    advance(); // consume the return keyword
    std::unique_ptr<ASTNS::AST> expr;
    if (!check(TokenType::SEMICOLON))
        expr = expression();
    consume(TokenType::SEMICOLON, "Expected semicolon after return statement");

    return std::make_unique<ASTNS::ReturnStmtAST>(std::move(expr));
}
// }}}
// {{{ expression
std::unique_ptr<ASTNS::AST> Parser::expression()
{
    return assignmentexpr();
}

std::unique_ptr<ASTNS::AST> Parser::assignmentexpr()
{
    std::unique_ptr<ASTNS::AST> lhs = ternaryexpr(); // should be VariableRefAST if it is a valid lhs

    if (match(TokenType::EQUAL))
    {
        if (!(dynamic_cast<ASTNS::LValueAST*>(&*lhs)))
            error("Invalid assignment target");

        Token equalSign = prev();
        std::unique_ptr<ASTNS::AST> rhs = assignmentexpr();
        std::unique_ptr<ASTNS::AssignAST> assignast = std::make_unique<ASTNS::AssignAST>(std::move(lhs), std::move(rhs), equalSign);

        return assignast;
    }

    return lhs;
}
// {{{ ternary expr
std::unique_ptr<ASTNS::AST> Parser::ternaryexpr()
{
    std::unique_ptr<ASTNS::AST> binexpr = binorexpr();

    if (match(TokenType::QUESTION))
    {
        std::unique_ptr<ASTNS::AST> trueexpr = binorexpr();
        consume(TokenType::COLON, "Expect colon after first expression");
        std::unique_ptr<ASTNS::AST> falseexpr = ternaryexpr();

        std::unique_ptr<ASTNS::TernaryOpAST> ternast = std::make_unique<ASTNS::TernaryOpAST>(std::move(binexpr), std::move(trueexpr), std::move(falseexpr));

        return ternast;
    }

    return binexpr;
}
// }}}
// {{{ binary and or not
std::unique_ptr<ASTNS::AST> Parser::binorexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = binandexpr();
    while (match(TokenType::DOUBLEPIPE) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = binandexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::binandexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = binnotexpr();
    while (match(TokenType::DOUBLEAMPER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = binnotexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::binnotexpr()
{
    if (match(TokenType::BANG))
    {
        std::unique_ptr<ASTNS::AST> binnot = binnotexpr();
        std::unique_ptr<ASTNS::UnaryAST> node = std::make_unique<ASTNS::UnaryAST>(prev(), std::move(binnot));
        return node;
    }

    return compeqexpr();
}
// }}}
// {{{ comparison
std::unique_ptr<ASTNS::AST> Parser::compeqexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = complgtexpr();
    while (match(TokenType::DOUBLEEQUAL) || match(TokenType::BANGEQUAL) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = complgtexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::complgtexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = bitxorexpr();
    while (match(TokenType::LESS) || match(TokenType::GREATER) || match(TokenType::LESSEQUAL) || match(TokenType::GREATEREQUAL) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = bitxorexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}
// }}}
// {{{ bit xor or and shift
std::unique_ptr<ASTNS::AST> Parser::bitxorexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = bitorexpr();
    while (match(TokenType::CARET) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = bitorexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::bitorexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = bitandexpr();
    while (match(TokenType::PIPE) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = bitandexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::bitandexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = bitshiftexpr();
    while (match(TokenType::AMPER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = bitshiftexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::bitshiftexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = additionexpr();
    while (match(TokenType::DOUBLELESS) || match(TokenType::DOUBLEGREATER) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = additionexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}
// }}}
// {{{ add mult unary
std::unique_ptr<ASTNS::AST> Parser::additionexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = multexpr();

    while (match(TokenType::PLUS) || match(TokenType::MINUS) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = multexpr();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::multexpr()
{
    std::unique_ptr<ASTNS::AST> lnode = unary();

    while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT) && !atEnd())
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> rnode = unary();

        std::unique_ptr<ASTNS::BinaryAST> pnode = std::make_unique<ASTNS::BinaryAST>(op, std::move(lnode), std::move(rnode));

        lnode = std::move(pnode);
    }

    return lnode;
}

std::unique_ptr<ASTNS::AST> Parser::unary()
{
    if (match(TokenType::TILDE) || match(TokenType::MINUS))
    {
        Token op = prev();
        std::unique_ptr<ASTNS::AST> unaryno = unary();

        std::unique_ptr<ASTNS::UnaryAST> node = std::make_unique<ASTNS::UnaryAST>(op, std::move(unaryno));

        return node;
    }

    return primary();
}
// }}}
// {{{ primary
std::unique_ptr<ASTNS::AST> Parser::primary()
{
    if (match(TokenType::TRUELIT) || match(TokenType::FALSELIT) ||
            match(TokenType::FLOATLIT) ||
            match(TokenType::DECINTLIT) || match(TokenType::OCTINTLIT) || match(TokenType::BININTLIT) || match(TokenType::HEXINTLIT) ||
            match(TokenType::CHARLIT) || match(TokenType::STRINGLIT))
    {
        return std::make_unique<ASTNS::PrimaryAST>(prev());
    }

    if (match(TokenType::OPARN))
    {
        std::unique_ptr<ASTNS::AST> expr = expression();
        consume(TokenType::CPARN, "Expect ')' after expression");
        return expr;
    }

    return lvalue();
}
// }}}
// }}}
// {{{ parsing helping rules
std::unique_ptr<ASTNS::AST> Parser::paramlist()
{
    std::vector<std::unique_ptr<ASTNS::AST>> params;
    std::unique_ptr<ASTNS::AST> paramtype = type();
    Token paramname = consume(TokenType::IDENTIFIER, "Expected paramuemnt name");

    std::unique_ptr<ASTNS::AST> param = std::make_unique<ASTNS::ParamAST>(std::move(paramtype), paramname);

    params.push_back(std::move(param));

    while (match(TokenType::COMMA) && !atEnd())
    {
        std::unique_ptr<ASTNS::AST> cparamtype = type();
        Token cparamname = consume(TokenType::IDENTIFIER, "Expected paramuemnt name");

        std::unique_ptr<ASTNS::AST> cparam = std::make_unique<ASTNS::ParamAST>(std::move(cparamtype), cparamname);

        params.push_back(std::move(cparam));
    }

    std::unique_ptr<ASTNS::AST> paramsast = std::make_unique<ASTNS::ParamsAST>(params);
    return paramsast;
}
std::unique_ptr<ASTNS::AST> Parser::arglist()
{
    std::vector<std::unique_ptr<ASTNS::AST>> args;

    std::unique_ptr<ASTNS::AST> arg = std::make_unique<ASTNS::ArgAST>(expression());

    args.push_back(std::move(arg));

    while (match(TokenType::COMMA) && !atEnd())
    {
        std::unique_ptr<ASTNS::AST> cargexpr = expression();

        std::unique_ptr<ASTNS::AST> carg = std::make_unique<ASTNS::ArgAST>(std::move(cargexpr));

        args.push_back(std::move(carg));
    }

    std::unique_ptr<ASTNS::AST> argsast = std::make_unique<ASTNS::ArgsAST>(args);
    return argsast;
}

std::unique_ptr<ASTNS::AST> Parser::block()
{
    consume(TokenType::OCURB, "Expected '{' to open block");

    std::vector<std::unique_ptr<ASTNS::AST>> statements;

    while (!check(TokenType::CCURB) && !atEnd())
    {
        std::unique_ptr<ASTNS::AST> statementast = statement();
        if (statementast)
            statements.push_back(std::move(statementast));
    }

    consume(TokenType::CCURB, "Expected '}' to close block");

    std::unique_ptr<ASTNS::AST> b = std::make_unique<ASTNS::BlockAST>(statements);
    return b;
}

std::unique_ptr<ASTNS::AST> Parser::type()
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
        return std::make_unique<ASTNS::TypeAST>(prev());
    }

    error("Expected type", true);
    return nullptr;
}

// }}}
// {{{ lvalues
std::unique_ptr<ASTNS::AST> Parser::lvalue()
{
    Token iden = consume(TokenType::IDENTIFIER, "Expected identifier");
    std::unique_ptr<ASTNS::AST> lvalueast = std::make_unique<ASTNS::LValueAST>(std::make_unique<ASTNS::VariableRefAST>(iden));

    // merged call rule and lvalue rule into one
    // except really all I did was keep it the way it was before
    while (match(TokenType::OPARN))
    {
        Token oparn = prev();
        std::unique_ptr<ASTNS::AST> arglistast;
        if (!check(TokenType::CPARN))
             arglistast = arglist();

        consume(TokenType::CPARN, "Expected closing parentheses after argument list");

        lvalueast = std::make_unique<ASTNS::CallAST>(std::move(lvalueast), std::move(arglistast), oparn);
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

    while (true)
    {
        currToken = lexer.nextToken();

        if (currToken.type != TokenType::ERROR) break; // continue loop if it is an error token

        // override advance in error
        // it's there to prevent infinite loops, but in this function,
        // we don't need it to because it might cause problems
        error(currToken.message, true, true);
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

void Parser::error(std::string const msg, bool nextT, bool noadvance)
{
    if (!PANICK)
    {
        Token &badToken = prev();

        if (prev().type == TokenType::SOF || nextT)
            badToken = peek();

        reportError(badToken, msg, sourcefile);
        panic();
    }

    if (!noadvance)
        advance(); // to prevent infinite loops
}

// }}}
