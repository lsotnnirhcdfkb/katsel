#include "ast.h"

BinaryAST::BinaryAST(Token op, std::unique_ptr<AST> last, std::unique_ptr<AST> rast): op(op), last(std::move(last)), rast(std::move(rast)) {}
TernaryOpAST::TernaryOpAST(std::unique_ptr<AST> conditional, std::unique_ptr<AST> trueast, std::unique_ptr<AST> falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}
UnaryAST::UnaryAST(Token op, std::unique_ptr<AST> ast): op(op), ast(std::move(ast)) {}
PrimaryAST::PrimaryAST(Token value): value(value) {}
ExprStmtAST::ExprStmtAST(std::unique_ptr<AST> ast): ast(std::move(ast)) {}
VarStmtAST::VarStmtAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> expression) : type(std::move(type)), name(name), expression(std::move(expression)) {}
TypeAST::TypeAST(Token type) : type(type) {}

ProgramAST::ProgramAST(std::vector<std::unique_ptr<AST>> &asts) {
    this->asts.reserve(asts.size());
    for (std::unique_ptr<AST> &ast : asts) {
        this->asts.push_back(std::move(ast));
    }
}

void BinaryAST::accept(Visitor *v) { v->visitBinaryAST(this); }
void TernaryOpAST::accept(Visitor *v) { v->visitTernaryOpAST(this); }
void UnaryAST::accept(Visitor *v) { v->visitUnaryAST(this); }
void PrimaryAST::accept(Visitor *v) { v->visitPrimaryAST(this); }
void ExprStmtAST::accept(Visitor *v) { v->visitExprStmtAST(this); }
void ProgramAST::accept(Visitor *v) { v->visitProgramAST(this); }
void VarStmtAST::accept(Visitor *v) { /* nothing for now */ }
void TypeAST::accept(Visitor *v) {}
