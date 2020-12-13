#pragma once

#include "llvm/Support/raw_ostream.h"

#include "ast/visitor.h"
#include "ast/ast.h"

#include "ir/value.h"
#include "ir/type.h"
#include "ir/unit.h"
#include "ir/instruction.h"

namespace CodeGenNS
{
    class CodeGen;
    class TypeResolve : public ASTNS::TypeBVisitor
    {
    public:
        TypeResolve(CodeGen &cg);

        IR::Type* type(ASTNS::TypeB *ast);

    private:
        // TYPEVISITOR METHODS START

// The following code was autogenerated - see the utils/ directory
void visitBuiltinType(ASTNS::BuiltinType *ast) override;
// This code was autogenerated - see the utils/ directory

        // TYPEVISITOR METHODS END

        IR::Type *ret;

        CodeGen &cg;
    };

    class Declarator : public ASTNS::DeclBVisitor, public ASTNS::CUBVisitor
    {
    public:
        Declarator(CodeGen &cg);

    private:
        // DECLARATOR METHODS START

// The following code was autogenerated - see the utils/ directory
void visitCU(ASTNS::CU *ast) override;
void visitDeclList(ASTNS::DeclList *ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl *ast) override;
// This code was autogenerated - see the utils/ directory

        // DECLARATOR METHODS END

        CodeGen &cg;
    };

    class ParamVisitor : public ASTNS::PListBVisitor
    {
    public:
        struct Param
        {
            IR::Type *ty;
            std::string name;
            ASTNS::Param *ast;
        };

        ParamVisitor(CodeGen &cg);

        std::vector<Param> params(ASTNS::PListB *pl);

    private:
        // PARAMVISITOR METHODS START

// The following code was autogenerated - see the utils/ directory
void visitParam(ASTNS::Param *ast) override;
void visitParamList(ASTNS::ParamList *ast) override;
void visitParamList_OPT(ASTNS::ParamList_OPT *ast) override;
void visitParamSegment(ASTNS::ParamSegment *ast) override;
// This code was autogenerated - see the utils/ directory

        // PARAMVISITOR METHODS END

        std::vector<Param> ret;
        CodeGen &cg;

    };

    class ArgsVisitor : public ASTNS::ArgBVisitor
    {
    public:
        ArgsVisitor(CodeGen &cg);

        std::vector<IR::ASTValue> args(ASTNS::ArgB *pl);

    private:
        // ARGSVISITOR METHODS START

// The following code was autogenerated - see the utils/ directory
void visitArg(ASTNS::Arg *ast) override;
void visitArgList(ASTNS::ArgList *ast) override;
void visitArgList_OPT(ASTNS::ArgList_OPT *ast) override;
void visitArgSegment(ASTNS::ArgSegment *ast) override;
// This code was autogenerated - see the utils/ directory

        // ARGSVISITOR METHODS END

        std::vector<IR::ASTValue> ret;
        CodeGen &cg;
    };

    class DeclCodeGen : public ASTNS::DeclBVisitor, public ASTNS::CUBVisitor
    {
    public:
        DeclCodeGen(CodeGen &cg);

    private:
        // DECLCG METHODS START

// The following code was autogenerated - see the utils/ directory
void visitCU(ASTNS::CU *ast) override;
void visitDeclList(ASTNS::DeclList *ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl *ast) override;
// This code was autogenerated - see the utils/ directory

        // DECLCG METHODS END

        CodeGen &cg;
    };

    class StmtCodeGen : public ASTNS::StmtBVisitor, public ASTNS::VStmtIBVisitor
    {
    public:
        StmtCodeGen(CodeGen &cg);

        void stmt(ASTNS::StmtB *ast);

    private:
        // STMTCG METHODS START

// The following code was autogenerated - see the utils/ directory
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitStmtList(ASTNS::StmtList *ast) override;
void visitStmtList_OPT(ASTNS::StmtList_OPT *ast) override;
void visitStmtSegment(ASTNS::StmtSegment *ast) override;
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) override;
void visitVarStmtItemSegment(ASTNS::VarStmtItemSegment *ast) override;
// This code was autogenerated - see the utils/ directory

        // STMTCG METHODS END

        CodeGen &cg;
        IR::Type *varty;
    };

    class ExprCodeGen : public ASTNS::ExprBVisitor
    {
    public:
        ExprCodeGen(CodeGen &cg);

        IR::ASTValue expr(ASTNS::ExprB *ast);

    private:
        // EXPRCG METHODS START

// The following code was autogenerated - see the utils/ directory
void visitAdditionExpr(ASTNS::AdditionExpr *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitBinAndExpr(ASTNS::BinAndExpr *ast) override;
void visitBinOrExpr(ASTNS::BinOrExpr *ast) override;
void visitBitAndExpr(ASTNS::BitAndExpr *ast) override;
void visitBitOrExpr(ASTNS::BitOrExpr *ast) override;
void visitBitShiftExpr(ASTNS::BitShiftExpr *ast) override;
void visitBitXorExpr(ASTNS::BitXorExpr *ast) override;
void visitBracedBlock(ASTNS::BracedBlock *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitCastExpr(ASTNS::CastExpr *ast) override;
void visitCompEQExpr(ASTNS::CompEQExpr *ast) override;
void visitCompLGTExpr(ASTNS::CompLGTExpr *ast) override;
void visitIfExpr(ASTNS::IfExpr *ast) override;
void visitImplRet(ASTNS::ImplRet *ast) override;
void visitImplRet_OPT(ASTNS::ImplRet_OPT *ast) override;
void visitIndentedBlock(ASTNS::IndentedBlock *ast) override;
void visitMultExpr(ASTNS::MultExpr *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
// This code was autogenerated - see the utils/ directory

        // EXPRCG METHODS END

        IR::ASTValue ret;
        CodeGen &cg;
    };

    class Context
    {
    public:
        struct Local
        {
            size_t scopenum;
            IR::Register *v;
            std::string name;
        };

        Context(File const &file);

        IR::Unit unit;
        std::map<std::string, IR::Value*> globalSymbolTable;

        std::vector<Local> locals;
        size_t curScope = 1;

        IR::BuiltinType* getBuiltinType(IR::BuiltinType::Builtins ty);
        IR::FunctionType* getFunctionType(IR::Type *ret, std::vector<IR::Type*> paramtys);
        IR::VoidType* getVoidType();

        void addLocal(std::string const &name, IR::Register *val);
        Local* findLocal(std::string const &name);
        IR::Value* findValue(std::string const &name);
        IR::Value* findGlobal(std::string const &name);

        void incScope();
        void decScope();

        IR::Function *curFunc;
        IR::Block *curBlock;
        IR::Block *exitBlock;
        IR::Register *retReg;
        std::unique_ptr<IR::Block> blackHoleBlock;

        std::vector<std::unique_ptr<IR::ConstInt>> constants;
        IR::ConstInt* getConstInt(IR::BuiltinType *ty, int val);
        IR::Void* getVoidValue();

    private:
        std::vector<std::unique_ptr<IR::Type>> types;
        IR::Void voidValue;
    };

    class CodeGen
    {
    public:
        CodeGen(File const &file);

        void declarate(ASTNS::CUB *decls);
        void codegen(ASTNS::CUB *decls);

        void printUnit(llvm::raw_ostream &ostream);

        Context context;

        Declarator declarator;
        TypeResolve typeResolver;
        ParamVisitor paramVisitor;
        ArgsVisitor argsVisitor;

        DeclCodeGen declCodeGen;
        StmtCodeGen stmtCodeGen;
        ExprCodeGen exprCodeGen;

        bool errored;
    };
}
