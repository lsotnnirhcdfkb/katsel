#include "ast/dotvisitor.h"

#include <iostream>
#include <sstream>

// DOTVISITOR START
// The following code was autogenerated - see the utils/ directory
void ASTNS::DotVisitor::visitCU(ASTNS::CU *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">CU</td></tr><tr>";
            ostream << "<td port=\"decls\">decls</td>";
            ostream << "</tr></table>>]\n";
            {
                    if (a->decls)
                    {
                        a->decls->accept(this);
                        connect(thisid, "decls", lastid);
                    }
                    else
                    {
                        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
                        connect(thisid, "decls", nullptrnodeid);
                    }
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">FunctionDecl</td></tr><tr>";
            ostream << "<td port=\"retty\">retty</td>";
            ostream << "<td port=\"name\">name</td>";
            ostream << "<td port=\"params\">params</td>";
            ostream << "</tr></table>>]\n";
            {
                    std::string tokennodeid = makeTextNode("Token", a->params.stringify());
                    connect(thisid, "params", tokennodeid);
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">VarStmt</td></tr><tr>";
            ostream << "<td port=\"type\">type</td>";
            ostream << "<td port=\"assignments\">assignments</td>";
            ostream << "</tr></table>>]\n";
            {
                    std::string tokennodeid = makeTextNode("Token", a->assignments.stringify());
                    connect(thisid, "assignments", tokennodeid);
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitVarStmtItem(ASTNS::VarStmtItem *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">VarStmtItem</td></tr><tr>";
            ostream << "<td port=\"name\">name</td>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">ExprStmt</td></tr><tr>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitRetStmt(ASTNS::RetStmt *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">RetStmt</td></tr><tr>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitBlock(ASTNS::Block *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">Block</td></tr><tr>";
            ostream << "<td port=\"stmts\">stmts</td>";
            ostream << "<td port=\"implRet\">implRet</td>";
            ostream << "</tr></table>>]\n";
            {
                    if (a->implRet)
                    {
                        a->implRet->accept(this);
                        connect(thisid, "implRet", lastid);
                    }
                    else
                    {
                        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
                        connect(thisid, "implRet", nullptrnodeid);
                    }
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitPrimitiveType(ASTNS::PrimitiveType *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">PrimitiveType</td></tr><tr>";
            ostream << "<td port=\"ty\">ty</td>";
            ostream << "</tr></table>>]\n";
            {
                    std::string tokennodeid = makeTextNode("Token", a->ty.stringify());
                    connect(thisid, "ty", tokennodeid);
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitArg(ASTNS::Arg *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">Arg</td></tr><tr>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitParam(ASTNS::Param *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">Param</td></tr><tr>";
            ostream << "<td port=\"ty\">ty</td>";
            ostream << "<td port=\"name\">name</td>";
            ostream << "</tr></table>>]\n";
            {
                    std::string tokennodeid = makeTextNode("Token", a->name.stringify());
                    connect(thisid, "name", tokennodeid);
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitIfExpr(ASTNS::IfExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">IfExpr</td></tr><tr>";
            ostream << "<td port=\"cond\">cond</td>";
            ostream << "<td port=\"trues\">trues</td>";
            ostream << "<td port=\"falses\">falses</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitForExpr(ASTNS::ForExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"4\">ForExpr</td></tr><tr>";
            ostream << "<td port=\"start\">start</td>";
            ostream << "<td port=\"cond\">cond</td>";
            ostream << "<td port=\"increment\">increment</td>";
            ostream << "<td port=\"body\">body</td>";
            ostream << "</tr></table>>]\n";
            {
                    if (a->body)
                    {
                        a->body->accept(this);
                        connect(thisid, "body", lastid);
                    }
                    else
                    {
                        std::string nullptrnodeid = makeTextNode("nullptr_t", "nullptr");
                        connect(thisid, "body", nullptrnodeid);
                    }
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitAssignmentExpr(ASTNS::AssignmentExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">AssignmentExpr</td></tr><tr>";
            ostream << "<td port=\"target\">target</td>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitShortCircuitExpr(ASTNS::ShortCircuitExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">ShortCircuitExpr</td></tr><tr>";
            ostream << "<td port=\"lhs\">lhs</td>";
            ostream << "<td port=\"op\">op</td>";
            ostream << "<td port=\"rhs\">rhs</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">BinaryExpr</td></tr><tr>";
            ostream << "<td port=\"lhs\">lhs</td>";
            ostream << "<td port=\"op\">op</td>";
            ostream << "<td port=\"rhs\">rhs</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitCastExpr(ASTNS::CastExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">CastExpr</td></tr><tr>";
            ostream << "<td port=\"castto\">castto</td>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"2\">UnaryExpr</td></tr><tr>";
            ostream << "<td port=\"op\">op</td>";
            ostream << "<td port=\"expr\">expr</td>";
            ostream << "</tr></table>>]\n";
            {
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
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"3\">CallExpr</td></tr><tr>";
            ostream << "<td port=\"callee\">callee</td>";
            ostream << "<td port=\"oparn\">oparn</td>";
            ostream << "<td port=\"args\">args</td>";
            ostream << "</tr></table>>]\n";
            {
                    std::string tokennodeid = makeTextNode("Token", a->args.stringify());
                    connect(thisid, "args", tokennodeid);
            }
    lastid = std::move(thisid);
}
void ASTNS::DotVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    std::string thisid = curid();
            ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"__heading\" colspan=\"1\">PrimaryExpr</td></tr><tr>";
            ostream << "<td port=\"value\">value</td>";
            ostream << "</tr></table>>]\n";
            {
                    std::string tokennodeid = makeTextNode("Token", a->value.stringify());
                    connect(thisid, "value", tokennodeid);
            }
    lastid = std::move(thisid);
}
// This code was autogenerated - see the utils/ directory
// DOTVISITOR END
void ASTNS::DotVisitor::dotVisit(ASTNS::CUB *ast)
{
    ostream << "strict digraph {\n";
    ostream << "node [shape=plain]\n";
    ast->accept(this);
    ostream << "}\n";
}
std::string ASTNS::DotVisitor::curid()
{
    std::stringstream ss;
    ss << "struct" << _curid;
    ++_curid;
    return ss.str();
}
std::string ASTNS::DotVisitor::makeTextNode(std::string type, std::string text)
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
    ostream << thisid << " [label=<<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td>" << type << "</td></tr><tr><td>" << text << "</td></tr></table>>]\n";
    return thisid;
}
void ASTNS::DotVisitor::connect(std::string startid, std::string fieldname, std::string connectto)
{
    ostream << startid << ":" << fieldname << " -> " << connectto << ":__heading\n";
}

ASTNS::DotVisitor::DotVisitor(llvm::raw_ostream &ostream): ostream(ostream) {}
