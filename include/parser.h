/// @file parser.h
/// Parser class declaration

#pragma once

#include "lexer.h"
#include "token.h"
#include "tokentype.h"
#include "errors.h"
#include "ast.h"

#include <vector>

/// Parser to parse a sourcefile into ASTs
class Parser
{
public:
    /// The constructor
    /// @param l The lexer that produces tokens for this to parse
    /// @param sourcefile The file that is being parsed by this parser
    Parser(Lexer &l, File &sourcefile);

    /// Parse something
    std::unique_ptr<ASTNS::Program> parse();
    /// Parse a declaration
    std::unique_ptr<ASTNS::Decl> decl();
    /// Parse a function declaration
    std::unique_ptr<ASTNS::Decl> function();

    /// Parse a statement
    std::unique_ptr<ASTNS::Stmt> statement();
    /// Parse a variable declaration statement
    std::unique_ptr<ASTNS::Stmt> varstatement();
    /// Parse an expression statement
    std::unique_ptr<ASTNS::Stmt> exprstatement();
    /// Parse a return statement
    std::unique_ptr<ASTNS::Stmt> retstatement();

    /// Parse an expression
    std::unique_ptr<ASTNS::Expr> expression();
    /// Parse an assignment expression
    std::unique_ptr<ASTNS::Expr> assignmentexpr();
    /// Parse a ternary expression
    std::unique_ptr<ASTNS::Expr> ternaryexpr();
    /// Parse a binary or expression
    std::unique_ptr<ASTNS::Expr> binorexpr();
    /// Parse a binary and expression
    std::unique_ptr<ASTNS::Expr> binandexpr();
    /// Parse a binary not expression
    std::unique_ptr<ASTNS::Expr> binnotexpr();
    /// Parse an equality expression
    std::unique_ptr<ASTNS::Expr> compeqexpr();
    /// Parse a comparison expression
    std::unique_ptr<ASTNS::Expr> complgtexpr();
    /// Parse a bitwise xor expression
    std::unique_ptr<ASTNS::Expr> bitxorexpr();
    /// Parse a bitwise or expression
    std::unique_ptr<ASTNS::Expr> bitorexpr();
    /// Parse a bitwise and expression
    std::unique_ptr<ASTNS::Expr> bitandexpr();
    /// Parse a bit shift expression
    std::unique_ptr<ASTNS::Expr> bitshiftexpr();
    /// Parse a addition expression
    std::unique_ptr<ASTNS::Expr> additionexpr();
    /// Parse a multiplication expression
    std::unique_ptr<ASTNS::Expr> multexpr();
    /// Parse a unary expression
    std::unique_ptr<ASTNS::Expr> unary();
    /// Parse a primary
    std::unique_ptr<ASTNS::Expr> primary();

    /// Parse an lvalue
    std::unique_ptr<ASTNS::LValue> lvalue();
    /// Parse a variable reference
    std::unique_ptr<ASTNS::LValue> varref();
    /// Parse a code block
    std::unique_ptr<ASTNS::BlockStmt> block();
    /// Parse a type
    std::unique_ptr<ASTNS::Type> type();
    /// Parse a parameter list
    std::unique_ptr<ASTNS::Param> param();
    /// Parse a argumnet list
    std::unique_ptr<ASTNS::Arg> arg();

private:

    /// The previous token
    Token prevToken;
    /// The current token
    Token currToken;

    /// Whether or not the parser is panicin
    bool PANICK;

    /// The lexer that produces tokens to parse
    Lexer &lexer;
    /// The file being parsed
    File &sourcefile;

    /// Return the current token
    Token& peek();
    /// Return the previous token
    Token& prev();

    /// Consume the next token which must be type, or throw an error.
    /// @param type The type that the token must be
    /// @param message The error message error with
    Token& consume(TokenType type, std::string message);
    /// Advance to the next token
    void advance();

    /// Check the next token's type and consume it if it matches
    /// @param type The type to check against
    bool match(TokenType type);
    /// Return whether the next token's type matches
    /// @param type The type to check against
    bool check(TokenType type);

    /// Return whether the parser has reached the end of the token stream
    bool atEnd();

    /// Enter panic mode - silence all errors
    void panic();
    /// Leave panic mode
    void calmDown();
    /// Consume tokens until a statement boundary has been reached in order to return to a parsable state
    void syncTokens();

    /// Throw an error
    /// @param msg The error message to throw with
    /// @param nextT Whether to highlight the next token or the previous token
    void error(std::string const msg, bool nextT=false, bool noadvance=false);
};
