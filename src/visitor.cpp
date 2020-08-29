/// @file visitor.cpp
/// Visitor method definitions
/// Methods definitions for PrintVisitor to print out ASTs, and definitions for BlankVisitor, which does nothing to any ASTs.

#include "visitor.h"
#include "ast.h"

// print visitor {{{1
PrintVisitor::PrintVisitor(): indent(0), pindent(false) {}

void PrintVisitor::visitBinaryAST(const ASTs::BinaryAST *ast)
{
    print("(");
    ast->last->accept(this);
    print(std::string(ast->op.start, ast->op.end));
    ast->rast->accept(this);
    print(")");
}

void PrintVisitor::visitTernaryOpAST(const ASTs::TernaryOpAST *ast)
{
    print("(");
    ast->conditional->accept(this);
    print("?");
    ast->trueast->accept(this);
    print(":");
    ast->falseast->accept(this);
    print(")");
}

void PrintVisitor::visitUnaryAST(const ASTs::UnaryAST *ast)
{
    print("(");
    print(std::string(ast->op.start, ast->op.end));
    ast->ast->accept(this);
    print(")");
}

void PrintVisitor::visitPrimaryAST(const ASTs::PrimaryAST *ast)
{
    print(std::string(ast->value.start, ast->value.end));
}

void PrintVisitor::visitExprStmtAST(const ASTs::ExprStmtAST *ast)
{
    print("ExprStmt: ");
    ast->ast->accept(this);
    print("\n");
}

void PrintVisitor::visitProgramAST(const ASTs::ProgramAST *ast)
{
    print("Program:\n");
    ++indent;

    for (const std::unique_ptr<ASTs::AST> &ast : ast->asts)
    {
        ast->accept(this);
    }

    --indent;
    print("\n");
}

void PrintVisitor::visitFunctionAST(const ASTs::FunctionAST *ast)
{
    print("Function: name " + std::string(ast->name.start, ast->name.end) + ", ret ");
    ast->type->accept(this);
    if (ast->params)
    {
        print("\n");
        ++indent;
        ast->params->accept(this);
        --indent;
    }
    else
    {
        ++indent;
        print("\nno params\n");
        --indent;
    }

    ++indent;
    if (ast->body)
    {
        ast->body->accept(this);
    }
    else
    {
        print("declaration only");
    }
    --indent;
    print("\n");
}
void PrintVisitor::visitVarStmtAST(const ASTs::VarStmtAST *ast) 
{
    print("VarStmt: var " + std::string(ast->name.start, ast->name.end) + " of type ");
    ast->type->accept(this);
    print(" being assigned ");
    ast->expression->accept(this);
    print("\n");
}

void PrintVisitor::visitTypeAST(const ASTs::TypeAST *ast) 
{
    print("TypeAST: " + std::string(ast->type.start, ast->type.end));
}

void PrintVisitor::visitBlockAST(const ASTs::BlockAST *ast)
{
    print("Block:\n");
    ++indent;

    for (const std::unique_ptr<ASTs::AST> &ast : ast->stmts)
    {
        ast->accept(this);
    }
    --indent;
}

void PrintVisitor::visitParamAST(const ASTs::ParamAST *ast)
{
    print("(Param: " + std::string(ast->paramname.start, ast->paramname.end) + " with type ");
    ast->type->accept(this);
    print(")\n");
}

void PrintVisitor::visitParamsAST(const ASTs::ParamsAST *ast)
{
    print("Params:\n");
    ++indent;

    for (const std::unique_ptr<ASTs::AST> &ast : ast->params)
    {
        ast->accept(this);
    }
    --indent;
}

void PrintVisitor::visitAssignAST(const ASTs::AssignAST *ast)
{
    print("Assign: assign ");
    ast->rhs->accept(this);
    print(" to ");
    ast->lhs->accept(this);
    print("\n");
}

void PrintVisitor::visitReturnStmtAST(const ASTs::ReturnStmtAST *ast)
{
    print("Return statement: return ");
    if (ast->expr)
        ast->expr->accept(this);
    else
        print("void");
    print("\n");
}

void PrintVisitor::visitVariableRefAST(const ASTs::VariableRefAST *ast)
{
    print("(Variable reference: " + std::string(ast->var.start, ast->var.end) + ")");
}

void PrintVisitor::visitArgAST(const ASTs::ArgAST *ast) 
{
    print("(Argument: ");
    ast->expr->accept(this);
    print(")\n");
}

void PrintVisitor::visitArgsAST(const ASTs::ArgsAST *ast)
{
    print("Args:\n");
    ++indent;

    for (const std::unique_ptr<ASTs::AST> &ast : ast->args)
    {
        ast->accept(this);
    }
    --indent;
}

void PrintVisitor::visitCallAST(const ASTs::CallAST *ast) 
{
    print("Function call to function ");
    ast->varrefast->accept(this);
    print(" with args: ");
    ast->arglistast->accept(this);
}

void PrintVisitor::print(std::string &str)
{
    for (auto i = str.begin(); i != str.end(); ++i)
    {
        if (pindent)
        {
            std::cout << std::string(indent * 2, ' ');
        }

        pindent = false;
        std::cout << *i;

        if (*i == '\n')
            pindent = true;
    }

    std::cout << std::flush;
}
void PrintVisitor::print(std::string &&str)
{
    print(str);
}
// }}}
// blank visitor {{{1
// BLANKGEN START
void BlankVisitor::visitBinaryAST(const ASTs::BinaryAST *ast) {}
void BlankVisitor::visitTernaryOpAST(const ASTs::TernaryOpAST *ast) {}
void BlankVisitor::visitUnaryAST(const ASTs::UnaryAST *ast) {}
void BlankVisitor::visitPrimaryAST(const ASTs::PrimaryAST *ast) {}
void BlankVisitor::visitExprStmtAST(const ASTs::ExprStmtAST *ast) {}
void BlankVisitor::visitProgramAST(const ASTs::ProgramAST *ast) {}
void BlankVisitor::visitFunctionAST(const ASTs::FunctionAST *ast) {}
void BlankVisitor::visitBlockAST(const ASTs::BlockAST *ast) {}
void BlankVisitor::visitTypeAST(const ASTs::TypeAST *ast) {}
void BlankVisitor::visitParamAST(const ASTs::ParamAST *ast) {}
void BlankVisitor::visitParamsAST(const ASTs::ParamsAST *ast) {}
void BlankVisitor::visitVarStmtAST(const ASTs::VarStmtAST *ast) {}
void BlankVisitor::visitAssignAST(const ASTs::AssignAST *ast) {}
void BlankVisitor::visitVariableRefAST(const ASTs::VariableRefAST *ast) {}
void BlankVisitor::visitReturnStmtAST(const ASTs::ReturnStmtAST *ast) {}
void BlankVisitor::visitArgAST(const ASTs::ArgAST *ast) {}
void BlankVisitor::visitArgsAST(const ASTs::ArgsAST *ast) {}
void BlankVisitor::visitCallAST(const ASTs::CallAST *ast) {}
// BLANKGEN END
