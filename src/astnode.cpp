#include "astnode.h"

BinaryAST::BinaryAST(Token op, std::unique_ptr<AST> &last, std::unique_ptr<AST> &rast): op(op), last(std::move(last)), rast(std::move(rast)) {}
TernaryOpAST::TernaryOpAST(std::unique_ptr<AST> &conditional, std::unique_ptr<AST> &trueast, std::unique_ptr<AST> &falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}
UnaryAST::UnaryAST(Token op, std::unique_ptr<AST> &ast): op(op), ast(std::move(ast)) {}
PrimaryAST::PrimaryAST(Token value): value(value) {}
