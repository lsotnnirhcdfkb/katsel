#pragma once

#include <memory>
#include <vector>
#include "lex/token.h"
#include "ast/visitor.h"

namespace ASTNS
{
// ASTHEADER START

// The following code was autogenerated - see the utils/ directory
    class AST;
    class ArgB;
    class CUB;
    class DeclB;
    class ExprB;
    class PListB;
    class StmtB;
    class StmtEndingB;
    class TypeB;
    class VStmtIB;
    class AdditionExpr;
    class AnotherArg;
    class AnotherDecl;
    class AnotherParam;
    class AnotherStmt;
    class AnotherVarStmtItem;
    class Arg;
    class ArgList;
    class ArgList_OPT;
    class ArgSegment;
    class AssignmentExpr;
    class BinAndExpr;
    class BinOrExpr;
    class BitAndExpr;
    class BitOrExpr;
    class BitShiftExpr;
    class BitXorExpr;
    class Block;
    class BracedBlock;
    class BuiltinType;
    class CU;
    class CallExpr;
    class CastExpr;
    class CompEQExpr;
    class CompLGTExpr;
    class Decl;
    class DeclList;
    class Expr;
    class ExprStmt;
    class FunctionDecl;
    class ImplRet;
    class ImplRet_OPT;
    class IndentedBlock;
    class MultExpr;
    class Param;
    class ParamList;
    class ParamList_OPT;
    class ParamSegment;
    class PrimaryExpr;
    class RetExpr;
    class Stmt;
    class StmtEnding;
    class StmtList;
    class StmtList_OPT;
    class StmtSegment;
    class TernaryExpr;
    class Type;
    class UnaryExpr;
    class VarStmt;
    class VarStmtItem;
    class VarStmtItemList;
    class VarStmtItemSegment;
    class AST
    {
    public:
        virtual ~AST() {}
    };
    class ArgB : public AST
    {
    public:
        virtual ~ArgB() {}
        virtual void accept(ASTNS::ArgBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class CUB : public AST
    {
    public:
        virtual ~CUB() {}
        virtual void accept(ASTNS::CUBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class DeclB : public AST
    {
    public:
        virtual ~DeclB() {}
        virtual void accept(ASTNS::DeclBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class ExprB : public AST
    {
    public:
        virtual ~ExprB() {}
        virtual void accept(ASTNS::ExprBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class PListB : public AST
    {
    public:
        virtual ~PListB() {}
        virtual void accept(ASTNS::PListBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class StmtB : public AST
    {
    public:
        virtual ~StmtB() {}
        virtual void accept(ASTNS::StmtBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class StmtEndingB : public AST
    {
    public:
        virtual ~StmtEndingB() {}
        virtual void accept(ASTNS::StmtEndingBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class TypeB : public AST
    {
    public:
        virtual ~TypeB() {}
        virtual void accept(ASTNS::TypeBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class VStmtIB : public AST
    {
    public:
        virtual ~VStmtIB() {}
        virtual void accept(ASTNS::VStmtIBVisitor *v) = 0;
        virtual bool empty() = 0;
    };
    class AdditionExpr : public ExprB
    {
    public:
        AdditionExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class AnotherArg : public ArgB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class AnotherDecl : public DeclB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class AnotherParam : public PListB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class AnotherStmt : public StmtB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class AnotherVarStmtItem : public VStmtIB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class Arg : public ArgB
    {
    public:
        Arg(std::unique_ptr<ExprB> expr);
        enum class Form
        {
            A,
        };
        std::unique_ptr<ExprB> expr;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ArgBVisitor *v) override;
    };
    class ArgList : public ArgB
    {
    public:
        ArgList(std::unique_ptr<ArgB> argsegment, Token comma);
        enum class Form
        {
            AT,
        };
        std::unique_ptr<ArgB> argsegment;
        Token comma;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ArgBVisitor *v) override;
    };
    class ArgList_OPT : public ArgB
    {
    public:
        ArgList_OPT();
        enum class Form
        {
            EMPTY,
        };
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ArgBVisitor *v) override;
    };
    class ArgSegment : public ArgB
    {
    public:
        ArgSegment(std::unique_ptr<ArgB> argsegment, Token comma, std::unique_ptr<ArgB> anotherarg);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ArgB> argsegment;
        Token comma;
        std::unique_ptr<ArgB> anotherarg;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ArgBVisitor *v) override;
    };
    class AssignmentExpr : public ExprB
    {
    public:
        AssignmentExpr(std::unique_ptr<ExprB> target, Token equal, std::unique_ptr<ExprB> value);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> target;
        Token equal;
        std::unique_ptr<ExprB> value;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BinAndExpr : public ExprB
    {
    public:
        BinAndExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BinOrExpr : public ExprB
    {
    public:
        BinOrExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BitAndExpr : public ExprB
    {
    public:
        BitAndExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BitOrExpr : public ExprB
    {
    public:
        BitOrExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BitShiftExpr : public ExprB
    {
    public:
        BitShiftExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BitXorExpr : public ExprB
    {
    public:
        BitXorExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class Block : public ExprB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class BracedBlock : public ExprB
    {
    public:
        BracedBlock(Token ocurb, std::unique_ptr<StmtB> stmts, std::unique_ptr<ExprB> implret, Token ccurb);
        enum class Form
        {
            TAAT,
        };
        Token ocurb;
        std::unique_ptr<StmtB> stmts;
        std::unique_ptr<ExprB> implret;
        Token ccurb;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class BuiltinType : public TypeB
    {
    public:
        BuiltinType(Token type);
        enum class Form
        {
            T,
        };
        Token type;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::TypeBVisitor *v) override;
    };
    class CU : public CUB
    {
    public:
        CU(std::unique_ptr<DeclB> dl);
        CU();
        enum class Form
        {
            A,
            EMPTY,
        };
        std::unique_ptr<DeclB> dl;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::CUBVisitor *v) override;
    };
    class CallExpr : public ExprB
    {
    public:
        CallExpr(std::unique_ptr<ExprB> callee, Token oparn, std::unique_ptr<ArgB> args, Token cparn);
        enum class Form
        {
            ATAT,
        };
        std::unique_ptr<ExprB> callee;
        Token oparn;
        std::unique_ptr<ArgB> args;
        Token cparn;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class CastExpr : public ExprB
    {
    public:
        CastExpr(Token oparn, std::unique_ptr<TypeB> type, Token cparn, std::unique_ptr<ExprB> operand);
        enum class Form
        {
            TATA,
        };
        Token oparn;
        std::unique_ptr<TypeB> type;
        Token cparn;
        std::unique_ptr<ExprB> operand;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class CompEQExpr : public ExprB
    {
    public:
        CompEQExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class CompLGTExpr : public ExprB
    {
    public:
        CompLGTExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class Decl : public DeclB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class DeclList : public DeclB
    {
    public:
        DeclList(std::unique_ptr<DeclB> decllist, std::unique_ptr<DeclB> anotherdecl);
        enum class Form
        {
            AA,
        };
        std::unique_ptr<DeclB> decllist;
        std::unique_ptr<DeclB> anotherdecl;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::DeclBVisitor *v) override;
    };
    class Expr : public ExprB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class ExprStmt : public StmtB
    {
    public:
        ExprStmt(std::unique_ptr<ExprB> expr);
        enum class Form
        {
            A,
        };
        std::unique_ptr<ExprB> expr;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::StmtBVisitor *v) override;
    };
    class FunctionDecl : public DeclB
    {
    public:
        FunctionDecl(Token fun, std::unique_ptr<TypeB> retty, Token name, Token oparn, std::unique_ptr<PListB> paramlist, Token cparn, std::unique_ptr<ExprB> body);
        FunctionDecl(Token fun, std::unique_ptr<TypeB> retty, Token name, Token oparn, std::unique_ptr<PListB> paramlist, Token cparn, Token newl);
        enum class Form
        {
            TATTATA,
            TATTATT,
        };
        Token fun;
        std::unique_ptr<TypeB> retty;
        Token name;
        Token oparn;
        std::unique_ptr<PListB> paramlist;
        Token cparn;
        std::unique_ptr<ExprB> body;
        Token newl;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::DeclBVisitor *v) override;
    };
    class ImplRet : public ExprB
    {
    public:
        ImplRet(Token leftarrow, std::unique_ptr<ExprB> expr);
        enum class Form
        {
            TA,
        };
        Token leftarrow;
        std::unique_ptr<ExprB> expr;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class ImplRet_OPT : public ExprB
    {
    public:
        ImplRet_OPT();
        enum class Form
        {
            EMPTY,
        };
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class IndentedBlock : public ExprB
    {
    public:
        IndentedBlock(Token newl, Token indent, std::unique_ptr<StmtB> stmts, std::unique_ptr<ExprB> implret, Token dedent);
        enum class Form
        {
            TTAAT,
        };
        Token newl;
        Token indent;
        std::unique_ptr<StmtB> stmts;
        std::unique_ptr<ExprB> implret;
        Token dedent;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class MultExpr : public ExprB
    {
    public:
        MultExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<ExprB> lhs;
        Token op;
        std::unique_ptr<ExprB> rhs;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class Param : public PListB
    {
    public:
        Param(std::unique_ptr<TypeB> type, Token name);
        enum class Form
        {
            AT,
        };
        std::unique_ptr<TypeB> type;
        Token name;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::PListBVisitor *v) override;
    };
    class ParamList : public PListB
    {
    public:
        ParamList(std::unique_ptr<PListB> paramsegment, Token comma);
        enum class Form
        {
            AT,
        };
        std::unique_ptr<PListB> paramsegment;
        Token comma;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::PListBVisitor *v) override;
    };
    class ParamList_OPT : public PListB
    {
    public:
        ParamList_OPT();
        enum class Form
        {
            EMPTY,
        };
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::PListBVisitor *v) override;
    };
    class ParamSegment : public PListB
    {
    public:
        ParamSegment(std::unique_ptr<PListB> paramsegment, Token comma, std::unique_ptr<PListB> anotherparam);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<PListB> paramsegment;
        Token comma;
        std::unique_ptr<PListB> anotherparam;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::PListBVisitor *v) override;
    };
    class PrimaryExpr : public ExprB
    {
    public:
        PrimaryExpr(Token value);
        PrimaryExpr(Token oparn, std::unique_ptr<ExprB> expr, Token cparn);
        enum class Form
        {
            T,
            TAT,
        };
        Token value;
        Token oparn;
        std::unique_ptr<ExprB> expr;
        Token cparn;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class RetExpr : public ExprB
    {
    public:
        RetExpr(Token ret, std::unique_ptr<ExprB> expr);
        RetExpr(Token ret);
        enum class Form
        {
            TA,
            T,
        };
        Token ret;
        std::unique_ptr<ExprB> expr;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class Stmt : public StmtB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class StmtEnding : public StmtEndingB
    {
    public:
        StmtEnding(Token tok);
        enum class Form
        {
            T,
        };
        Token tok;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::StmtEndingBVisitor *v) override;
    };
    class StmtList : public StmtB
    {
    public:
        StmtList(std::unique_ptr<StmtB> stmtsegment, std::unique_ptr<StmtEndingB> stmtending);
        enum class Form
        {
            AA,
        };
        std::unique_ptr<StmtB> stmtsegment;
        std::unique_ptr<StmtEndingB> stmtending;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::StmtBVisitor *v) override;
    };
    class StmtList_OPT : public StmtB
    {
    public:
        StmtList_OPT();
        enum class Form
        {
            EMPTY,
        };
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::StmtBVisitor *v) override;
    };
    class StmtSegment : public StmtB
    {
    public:
        StmtSegment(std::unique_ptr<StmtB> stmtsegment, std::unique_ptr<StmtEndingB> stmtending, std::unique_ptr<StmtB> anotherstmt);
        enum class Form
        {
            AAA,
        };
        std::unique_ptr<StmtB> stmtsegment;
        std::unique_ptr<StmtEndingB> stmtending;
        std::unique_ptr<StmtB> anotherstmt;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::StmtBVisitor *v) override;
    };
    class TernaryExpr : public ExprB
    {
    public:
        TernaryExpr(std::unique_ptr<ExprB> cond, Token quest, std::unique_ptr<ExprB> trues, Token colon, std::unique_ptr<ExprB> falses);
        enum class Form
        {
            ATATA,
        };
        std::unique_ptr<ExprB> cond;
        Token quest;
        std::unique_ptr<ExprB> trues;
        Token colon;
        std::unique_ptr<ExprB> falses;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class Type : public TypeB
    {
    public:
        enum class Form
        {
        };
        Form form;
    };
    class UnaryExpr : public ExprB
    {
    public:
        UnaryExpr(Token op, std::unique_ptr<ExprB> operand);
        enum class Form
        {
            TA,
        };
        Token op;
        std::unique_ptr<ExprB> operand;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::ExprBVisitor *v) override;
    };
    class VarStmt : public StmtB
    {
    public:
        VarStmt(Token var, std::unique_ptr<TypeB> type, std::unique_ptr<VStmtIB> assignments);
        enum class Form
        {
            TAA,
        };
        Token var;
        std::unique_ptr<TypeB> type;
        std::unique_ptr<VStmtIB> assignments;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::StmtBVisitor *v) override;
    };
    class VarStmtItem : public VStmtIB
    {
    public:
        VarStmtItem(Token name, Token equal, std::unique_ptr<ExprB> expr);
        VarStmtItem(Token name);
        enum class Form
        {
            TTA,
            T,
        };
        Token name;
        Token equal;
        std::unique_ptr<ExprB> expr;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::VStmtIBVisitor *v) override;
    };
    class VarStmtItemList : public VStmtIB
    {
    public:
        VarStmtItemList(std::unique_ptr<VStmtIB> varstmtitemsegment, Token comma);
        enum class Form
        {
            AT,
        };
        std::unique_ptr<VStmtIB> varstmtitemsegment;
        Token comma;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::VStmtIBVisitor *v) override;
    };
    class VarStmtItemSegment : public VStmtIB
    {
    public:
        VarStmtItemSegment(std::unique_ptr<VStmtIB> varstmtitemsegment, Token comma, std::unique_ptr<VStmtIB> anothervarstmtitem);
        enum class Form
        {
            ATA,
        };
        std::unique_ptr<VStmtIB> varstmtitemsegment;
        Token comma;
        std::unique_ptr<VStmtIB> anothervarstmtitem;
        Form form;
        bool empty() override;
        virtual void accept(ASTNS::VStmtIBVisitor *v) override;
    };
// This code was autogenerated - see the utils/ directory

// ASTHEADER END
}
