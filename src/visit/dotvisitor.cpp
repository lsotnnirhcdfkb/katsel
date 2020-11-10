#include "visit/dotvisitor.h"

#include <iostream>
#include <sstream>

// DOTVISITOR START

// The following code was autogenerated - see the utils/ directory
void DotVisitor::visitProgram(ASTNS::Program *a)
{
    std::cout << "strict digraph {\n";
    std::cout << "node [shape=plain]\n";
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">Program</td></tr><tr>";
    std::cout << "<td port=\"decls\">decls</td>";
    std::cout << "</tr></table>>]\n";
    for (auto &i : a->decls)
    {
        i->accept(this);
        connect(thisid, "decls", lastid);
    }
    lastid = std::move(thisid);
    std::cout << "}\n";
}
void DotVisitor::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">BinaryExpr</td></tr><tr>";
    std::cout << "<td port=\"lhs\">lhs</td>";
    std::cout << "<td port=\"rhs\">rhs</td>";
    std::cout << "<td port=\"op\">op</td>";
    std::cout << "</tr></table>>]\n";
    if (a->lhs)
    {
        a->lhs->accept(this);
        connect(thisid, "lhs", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "lhs", nullptrnodeid);
    }
    if (a->rhs)
    {
        a->rhs->accept(this);
        connect(thisid, "rhs", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "rhs", nullptrnodeid);
    }
    std::string tokennodeid = makeTextNode("Token", a->op.stringify());
    connect(thisid, "op", tokennodeid);
    lastid = std::move(thisid);
}
void DotVisitor::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">TernaryExpr</td></tr><tr>";
    std::cout << "<td port=\"condition\">condition</td>";
    std::cout << "<td port=\"trues\">trues</td>";
    std::cout << "<td port=\"falses\">falses</td>";
    std::cout << "</tr></table>>]\n";
    if (a->condition)
    {
        a->condition->accept(this);
        connect(thisid, "condition", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "condition", nullptrnodeid);
    }
    if (a->trues)
    {
        a->trues->accept(this);
        connect(thisid, "trues", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "trues", nullptrnodeid);
    }
    if (a->falses)
    {
        a->falses->accept(this);
        connect(thisid, "falses", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "falses", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">UnaryExpr</td></tr><tr>";
    std::cout << "<td port=\"operand\">operand</td>";
    std::cout << "<td port=\"op\">op</td>";
    std::cout << "</tr></table>>]\n";
    if (a->operand)
    {
        a->operand->accept(this);
        connect(thisid, "operand", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "operand", nullptrnodeid);
    }
    std::string tokennodeid = makeTextNode("Token", a->op.stringify());
    connect(thisid, "op", tokennodeid);
    lastid = std::move(thisid);
}
void DotVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">PrimaryExpr</td></tr><tr>";
    std::cout << "<td port=\"value\">value</td>";
    std::cout << "</tr></table>>]\n";
    std::string tokennodeid = makeTextNode("Token", a->value.stringify());
    connect(thisid, "value", tokennodeid);
    lastid = std::move(thisid);
}
void DotVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">CallExpr</td></tr><tr>";
    std::cout << "<td port=\"func\">func</td>";
    std::cout << "<td port=\"args\">args</td>";
    std::cout << "</tr></table>>]\n";
    if (a->func)
    {
        a->func->accept(this);
        connect(thisid, "func", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "func", nullptrnodeid);
    }
    if (a->args)
    {
        a->args->accept(this);
        connect(thisid, "args", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "args", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitBlockStmt(ASTNS::BlockStmt *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">BlockStmt</td></tr><tr>";
    std::cout << "<td port=\"stmts\">stmts</td>";
    std::cout << "</tr></table>>]\n";
    for (auto &i : a->stmts)
    {
        i->accept(this);
        connect(thisid, "stmts", lastid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">ExprStmt</td></tr><tr>";
    std::cout << "<td port=\"expr\">expr</td>";
    std::cout << "</tr></table>>]\n";
    if (a->expr)
    {
        a->expr->accept(this);
        connect(thisid, "expr", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "expr", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitReturnStmt(ASTNS::ReturnStmt *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">ReturnStmt</td></tr><tr>";
    std::cout << "<td port=\"val\">val</td>";
    std::cout << "</tr></table>>]\n";
    if (a->val)
    {
        a->val->accept(this);
        connect(thisid, "val", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "val", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">VarStmt</td></tr><tr>";
    std::cout << "<td port=\"type\">type</td>";
    std::cout << "<td port=\"assignments\">assignments</td>";
    std::cout << "</tr></table>>]\n";
    if (a->type)
    {
        a->type->accept(this);
        connect(thisid, "type", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "type", nullptrnodeid);
    }
    for (auto &i : a->assignments)
    {
        i->accept(this);
        connect(thisid, "assignments", lastid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitBaseType(ASTNS::BaseType *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">BaseType</td></tr><tr>";
    std::cout << "<td port=\"type\">type</td>";
    std::cout << "</tr></table>>]\n";
    std::string tokennodeid = makeTextNode("Token", a->type.stringify());
    connect(thisid, "type", tokennodeid);
    lastid = std::move(thisid);
}
void DotVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"4\">FunctionDecl</td></tr><tr>";
    std::cout << "<td port=\"rettype\">rettype</td>";
    std::cout << "<td port=\"name\">name</td>";
    std::cout << "<td port=\"params\">params</td>";
    std::cout << "<td port=\"block\">block</td>";
    std::cout << "</tr></table>>]\n";
    if (a->rettype)
    {
        a->rettype->accept(this);
        connect(thisid, "rettype", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "rettype", nullptrnodeid);
    }
    std::string tokennodeid = makeTextNode("Token", a->name.stringify());
    connect(thisid, "name", tokennodeid);
    if (a->params)
    {
        a->params->accept(this);
        connect(thisid, "params", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "params", nullptrnodeid);
    }
    if (a->block)
    {
        a->block->accept(this);
        connect(thisid, "block", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "block", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">GlobalVarDecl</td></tr><tr>";
    std::cout << "<td port=\"type\">type</td>";
    std::cout << "<td port=\"assignments\">assignments</td>";
    std::cout << "</tr></table>>]\n";
    if (a->type)
    {
        a->type->accept(this);
        connect(thisid, "type", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "type", nullptrnodeid);
    }
    for (auto &i : a->assignments)
    {
        i->accept(this);
        connect(thisid, "assignments", lastid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitParam(ASTNS::Param *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">Param</td></tr><tr>";
    std::cout << "<td port=\"type\">type</td>";
    std::cout << "<td port=\"name\">name</td>";
    std::cout << "<td port=\"next\">next</td>";
    std::cout << "</tr></table>>]\n";
    if (a->type)
    {
        a->type->accept(this);
        connect(thisid, "type", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "type", nullptrnodeid);
    }
    std::string tokennodeid = makeTextNode("Token", a->name.stringify());
    connect(thisid, "name", tokennodeid);
    if (a->next)
    {
        a->next->accept(this);
        connect(thisid, "next", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "next", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
void DotVisitor::visitArg(ASTNS::Arg *a)
{
    std::string thisid = curid();
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">Arg</td></tr><tr>";
    std::cout << "<td port=\"value\">value</td>";
    std::cout << "<td port=\"next\">next</td>";
    std::cout << "</tr></table>>]\n";
    if (a->value)
    {
        a->value->accept(this);
        connect(thisid, "value", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "value", nullptrnodeid);
    }
    if (a->next)
    {
        a->next->accept(this);
        connect(thisid, "next", lastid);
    }
    else
    {
        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
        connect(thisid, "next", nullptrnodeid);
    }
    lastid = std::move(thisid);
}
// This code was autogenerated - see the utils/ directory

// DOTVISITOR END
std::string DotVisitor::curid()
{
    std::stringstream ss;
    ss << "struct" << _curid;
    ++_curid;
    return ss.str();
}
std::string DotVisitor::makeTextNode(std::string type, std::string text)
{
    std::string thisid = curid();
#define FINDREP(x, r) {\
    size_t it;\
    while ((it = text.find(x)) != std::string::npos)\
        text.replace(it, it + 1, r);\
    }

    FINDREP("&", "\x07amp;")
    FINDREP("<", "\x07lt;")
    FINDREP(">", "\x07rt;")
    FINDREP("\x07", "&")

#undef FINDREP
    std::cout << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td>" << type << "</td></tr><tr><td>" << text << "</td></tr></table>>]\n";
    return thisid;
}
void DotVisitor::connect(std::string startid, std::string fieldname, std::string connectto)
{
    std::cout << startid << ":" << fieldname << " -> " << connectto << ":__heading\n";
}
