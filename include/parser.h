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
    std::unique_ptr<ASTs::AST> parse(); 
    /// Parse a declaration
    std::unique_ptr<ASTs::AST> decl(); 
    /// Parse a function declaration
    std::unique_ptr<ASTs::AST> function(); 

    /// Parse a statement
    std::unique_ptr<ASTs::AST> statement(); 
    /// Parse a variable declaration statement
    std::unique_ptr<ASTs::AST> varstatement(); 
    /// Parse an expression statement
    std::unique_ptr<ASTs::AST> exprstatement(); 
    /// Parse a return statement
    std::unique_ptr<ASTs::AST> retstatement(); 
    /// Parse a print statement
    std::unique_ptr<ASTs::AST> printstatement(); 

    /// Parse an expression
    std::unique_ptr<ASTs::AST> expression(); 
    /// Parse an assignment expression
    std::unique_ptr<ASTs::AST> assignmentexpr(); 
    /// Parse a ternary expression
    std::unique_ptr<ASTs::AST> ternaryexpr(); 
    /// Parse a binary or expression
    std::unique_ptr<ASTs::AST> binorexpr(); 
    /// Parse a binary and expression
    std::unique_ptr<ASTs::AST> binandexpr(); 
    /// Parse a binary not expression
    std::unique_ptr<ASTs::AST> binnotexpr(); 
    /// Parse an equality expression
    std::unique_ptr<ASTs::AST> compeqexpr(); 
    /// Parse a comparison expression
    std::unique_ptr<ASTs::AST> complgtexpr(); 
    /// Parse a bitwise xor expression
    std::unique_ptr<ASTs::AST> bitxorexpr(); 
    /// Parse a bitwise or expression
    std::unique_ptr<ASTs::AST> bitorexpr(); 
    /// Parse a bitwise and expression
    std::unique_ptr<ASTs::AST> bitandexpr(); 
    /// Parse a bit shift expression
    std::unique_ptr<ASTs::AST> bitshiftexpr(); 
    /// Parse a addition expression
    std::unique_ptr<ASTs::AST> additionexpr(); 
    /// Parse a multiplication expression
    std::unique_ptr<ASTs::AST> multexpr(); 
    /// Parse a unary expression
    std::unique_ptr<ASTs::AST> unary(); 
    /// Parse a primary
    std::unique_ptr<ASTs::AST> primary(); 

    /// Parse a variable reference
    std::unique_ptr<ASTs::AST> varref(); 
    /// Parse a code block
    std::unique_ptr<ASTs::AST> block(); 
    /// Parse a type
    std::unique_ptr<ASTs::AST> type(); 
    /// Parse a parameter list
    std::unique_ptr<ASTs::AST> paramlist(); 
    /// Parse a argumnet list
    std::unique_ptr<ASTs::AST> arglist(); 

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
    void error(std::string const msg, bool nextT=false);
};
