#pragma once

namespace ASTNS {
    enum class AssignOperator {
        EQUAL,
    };
    enum class BinaryOperator {
        PLUS,
        MINUS,
        STAR,
        SLASH,
        PERCENT,
        GREATER,
        LESS,
        GREATEREQUAL,
        LESSEQUAL,
        AMPER,
        PIPE,
        CARET,
        DOUBLEGREATER,
        DOUBLELESS,
        DOUBLEEQUAL,
        BANGEQUAL,
    };
    enum class UnaryOperator {
        BANG,
        TILDE,
        MINUS,
        DOUBLEPLUS,
        DOUBLEMINUS,
    };
    enum class ShortCircuitOperator {
        DOUBLEPIPE,
        DOUBLEAMPER,
    };
    // ASTFWD START
    class AST;
    class CUB;
    class Decl;
    class ImplMember;
    class Stmt;
    class Expr;
    class Type;
    class ArgB;
    class ParamB;
    class VStmtIB;
    class PathB;
    class ListB;
    class DeclList;
    class StmtList;
    class ParamList;
    class ArgList;
    class VarStmtItemList;
    class ImplMemberList;
    class PureLocationB;
    class PureLocation;
    class ImplicitDecl;
    class CU;
    class ImplDecl;
    class FunctionDecl;
    class FunctionImplMember;
    class VarStmt;
    class VarStmtItem;
    class ExprStmt;
    class RetStmt;
    class PathType;
    class PointerType;
    class ThisType;
    class Arg;
    class Param;
    class ThisParam;
    class Block;
    class IfExpr;
    class WhileExpr;
    class AssignmentExpr;
    class ShortCircuitExpr;
    class BinaryExpr;
    class CastExpr;
    class UnaryExpr;
    class AddrofExpr;
    class DerefExpr;
    class CallExpr;
    class FieldAccessExpr;
    class MethodCallExpr;
    class BoolLit;
    class FloatLit;
    class IntLit;
    class CharLit;
    class StringLit;
    class ThisExpr;
    class PathExpr;
    class Path;
    // ASTFWD END
}
