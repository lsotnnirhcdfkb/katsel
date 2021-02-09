#pragma once

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "ast/astfwd.h"
#include "ast/visitor.h"

#include "ir/ir_builder.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/instructionfwd.h"
#include "ir/module.h"

// Codegen happens in 4 stages
// Stage 0 (begin):
//     - the unit has an empty symbol table, functions are empty
//     - everything is empty
// Stage 1 (type declarations):
//     - the type symbol table is built
//     - it was built with types, namespace
//     - imported modules are also added here
// Stage x |*being added soon*| (type definitions):
//     - datatype fields and variants (in the future) are filled in
// Stage 2 (value declarations):
//     - the value symbol table is built
//     - it is with functions and global variables
//     - functions are now forward declared
//     - impl blocks have the member functions forward declared
//     - global variables (in the future) will be declared
// Stage 3 (value definitions):
//     - code is generated for functions bodies
//     - global variables are assigned
//
// Each type of declaration has a Codegen class that does each of these things

namespace Codegen {
    // Helpers {{{
    namespace Helpers {
        // Locals {{{
        struct Local {
            size_t scopenum;
            NNPtr<IR::Register> v;
            std::string name;
        };

        struct Locals {
            std::vector<Local> locals;
            size_t cur_scope;

            void add_local(std::string const &name, IR::Register &val);
            Maybe<Local> get_local(std::string const &name);

            void inc_scope();
            void dec_scope();
        };
        // }}}
        // Path {{{
        class PathVisitor : public ASTNS::PathBVisitor {
        public:
            PathVisitor(Maybe<Locals&> locals, IR::Unit &unit);

            Maybe<IR::ASTValue> resolve_value(ASTNS::PathB &path);
            Maybe<IR::DeclSymbol &> resolve_decl_symbol(ASTNS::PathB &path);

        private:
            enum class PathType { VALUE, DECLARED } pty;

            Maybe<IR::ASTValue> vret;
            Maybe<NNPtr<IR::DeclSymbol>> dret;
            Maybe<NNPtr<Locals>> locals;
            IR::Unit &unit;

            // PATH VISITOR START
            void visit(ASTNS::Path &ast) override;
            // PATH VISITOR END
        };
        // }}}
        // TypeVisitor {{{
        class TypeVisitor : public ASTNS::TypeVisitor {
        public:
            TypeVisitor(Context &context, Maybe<NNPtr<IR::Type>> this_type, Helpers::PathVisitor &path_visitor);

            Maybe<IR::Type &> type(ASTNS::Type &ast);

            NNPtr<Context> context;
            Maybe<NNPtr<IR::Type>> ret;
            Maybe<NNPtr<IR::Type>> this_type;

            NNPtr<PathVisitor> path_visitor;
        private:
            // TYPEVISITOR METHODS START
            void visit(ASTNS::PathType &ast) override;
            void visit(ASTNS::PointerType &ast) override;
            void visit(ASTNS::ThisType &ast) override;
            // TYPEVISITOR METHODS END
        };
        // }}}
        // StmtCodegen {{{
        class ExprCodegen;
        class StmtCodegen : public ASTNS::StmtVisitor, public ASTNS::VStmtIBVisitor {
        public:
            StmtCodegen(IR::Builder &ir_builder, Locals &locals, ExprCodegen &expr_cg, TypeVisitor &type_visitor, PathVisitor &path_visitor);

            void stmt(ASTNS::Stmt &ast);
            bool success;

        private:
            // STMTCG METHODS START
            void visit(ASTNS::VarStmt &ast) override;
            void visit(ASTNS::VarStmtItem &ast) override;
            void visit(ASTNS::ExprStmt &ast) override;
            void visit(ASTNS::RetStmt &ast) override;
            // STMTCG METHODS END

            NNPtr<IR::Builder> ir_builder;
            NNPtr<Locals> locals;
            NNPtr<ExprCodegen> expr_cg;
            NNPtr<TypeVisitor> type_visitor;
            NNPtr<PathVisitor> path_visitor;
        };
        // }}}
        // ExprCodegen {{{
        class ExprCodegen : public ASTNS::ExprVisitor {
        public:
            ExprCodegen(IR::Builder &ir_builder, Helpers::Locals &locals, TypeVisitor &type_visitor, PathVisitor &path_visitor);

            Maybe<IR::ASTValue> expr(ASTNS::Expr &ast);

        private:
            // EXPRCG METHODS START
            void visit(ASTNS::Block &ast) override;
            void visit(ASTNS::IfExpr &ast) override;
            void visit(ASTNS::WhileExpr &ast) override;
            void visit(ASTNS::AssignmentExpr &ast) override;
            void visit(ASTNS::ShortCircuitExpr &ast) override;
            void visit(ASTNS::BinaryExpr &ast) override;
            void visit(ASTNS::CastExpr &ast) override;
            void visit(ASTNS::UnaryExpr &ast) override;
            void visit(ASTNS::AddrofExpr &ast) override;
            void visit(ASTNS::DerefExpr &ast) override;
            void visit(ASTNS::CallExpr &ast) override;
            void visit(ASTNS::FieldAccessExpr &ast) override;
            void visit(ASTNS::MethodCallExpr &ast) override;
            void visit(ASTNS::BoolLit &ast) override;
            void visit(ASTNS::FloatLit &ast) override;
            void visit(ASTNS::IntLit &ast) override;
            void visit(ASTNS::CharLit &ast) override;
            void visit(ASTNS::StringLit &ast) override;
            void visit(ASTNS::ThisExpr &ast) override;
            void visit(ASTNS::PathExpr &ast) override;
            // EXPRCG METHODS END

            NNPtr<IR::Builder> ir_builder;
            NNPtr<Helpers::Locals> locals;
            StmtCodegen stmt_cg;
            NNPtr<TypeVisitor> type_visitor;
            NNPtr<PathVisitor> path_visitor;
            Maybe<IR::ASTValue> ret;
        };
        // }}}
        // Param {{{
        class ParamVisitor : public ASTNS::ParamBVisitor {
        public:
            ParamVisitor(Codegen::Context &context, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Helpers::TypeVisitor &type_visitor);

            std::vector<IR::Function::Param> ret;

            bool errored;

            bool is_method, this_ptr, this_mut;

        private:
            // PARAMVISITOR METHODS START
            void visit(ASTNS::Param &ast) override;
            void visit(ASTNS::ThisParam &ast) override;
            // PARAMVISITOR METHODS END

            NNPtr<Codegen::Context> context;
            NNPtr<TypeVisitor> type_visitor;
            int index;
        };
        // }}}
        // Arg {{{
        class ArgVisitor : public ASTNS::ArgBVisitor {
        public:
            ArgVisitor(ExprCodegen &expr_cg, std::vector<std::unique_ptr<ASTNS::Arg>> &args);
            std::vector<IR::ASTValue> ret;

        private:
            NNPtr<ExprCodegen> expr_cg;
            // ARGSVISITOR METHODS START
            void visit(ASTNS::Arg &ast) override;
            // ARGSVISITOR METHODS END

        };
        // }}}
    }
    // }}}
    // Codegen ABC {{{
    class CG {
    public:
        virtual ~CG() = default;
        virtual bool type_declare() = 0;
        // TODO: add type define phase
        virtual bool value_declare() = 0;
        virtual bool value_define() = 0;
    };
    // }}}
    // Function {{{
    class Function : public CG {
    public:
        Function(IR::Unit &unit, Codegen::Context &context, ASTNS::FunctionDecl &ast, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol);
        bool type_declare() override;
        bool value_declare() override;
        bool value_define() override;

    private:
        IR::Unit &unit;
        Codegen::Context &context;
        ASTNS::FunctionDecl &ast;
        Maybe<NNPtr<IR::Type>> this_type;
        IR::DeclSymbol &parent_symbol;

        struct S1Data {
            std::unique_ptr<Helpers::PathVisitor> path_visitor;
            std::unique_ptr<Helpers::TypeVisitor> type_visitor;
            NNPtr<IR::Function> fun;
            std::vector<IR::Function::Param> params;
        };
        struct S2Data {
            std::unique_ptr<Helpers::ExprCodegen> expr_cg;
            std::unique_ptr<Helpers::Locals> locals;
            NNPtr<IR::Block> entry_block;
            std::unique_ptr<IR::Builder> ir_builder;
        };
        Maybe<S1Data> m_s1_data;
        Maybe<S2Data> m_s2_data;
    };
    /*
        class S0 : public Stage0CG {
        public:
            S0(IR::Unit &unit, Codegen::Context &context, ASTNS::FunctionDecl &ast, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol);
            Maybe<std::unique_ptr<CStage0CG>> type_declare() override;

        private:
            IR::Unit &unit;
            Codegen::Context &context;
            ASTNS::FunctionDecl &ast;

            Maybe<NNPtr<IR::Type>> this_type;
            IR::DeclSymbol &parent_symbol;
        };
        class S1 : public Stage1CG {
        public:
            S1(IR::Unit &unit, Codegen::Context &context, ASTNS::FunctionDecl &ast, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol, std::unique_ptr<Helpers::PathVisitor> path_visitor, std::unique_ptr<Helpers::TypeVisitor> type_visitor);
            Maybe<std::unique_ptr<CStage1CG>> value_declare() override;

        private:
            IR::Unit &unit;
            Codegen::Context &context;
            ASTNS::FunctionDecl &ast;

            std::unique_ptr<Helpers::PathVisitor> path_visitor;
            std::unique_ptr<Helpers::TypeVisitor> type_visitor;
            Maybe<NNPtr<IR::Type>> this_type;
            IR::DeclSymbol &parent_symbol;
        };
        class S2 : public Stage2CG {
        public:
            S2(IR::Unit &unit, ASTNS::FunctionDecl &ast, IR::Function &fun, std::unique_ptr<Helpers::PathVisitor> path_visitor, std::unique_ptr<Helpers::TypeVisitor> type_visitor, Maybe<NNPtr<IR::Type>> this_type, IR::DeclSymbol &parent_symbol, std::vector<Helpers::ParamVisitor::Param> &params, std::unique_ptr<Helpers::ExprCodegen> expr_cg, std::unique_ptr<Helpers::Locals> locals, IR::Block &entry_block, std::unique_ptr<IR::Builder> ir_builder);
            Maybe<std::unique_ptr<CStage2CG>> value_define() override;

        private:
            IR::Unit &unit;
            ASTNS::FunctionDecl &ast;
            IR::Function &fun;

            std::unique_ptr<Helpers::PathVisitor> path_visitor;
            std::unique_ptr<Helpers::TypeVisitor> type_visitor;
            Maybe<NNPtr<IR::Type>> this_type;
            IR::DeclSymbol &parent_symbol;
            std::vector<Helpers::ParamVisitor::Param> params;

            std::unique_ptr<Helpers::ExprCodegen> expr_cg;

            std::unique_ptr<Helpers::Locals> locals;

            NNPtr<IR::Block> entry_block;

            std::unique_ptr<IR::Builder> ir_builder;
        };
        */
    // }}}
    // Impl {{{
    class Impl : public CG, public ASTNS::ImplMemberVisitor {
    public:
        Impl(IR::Unit &unit, Codegen::Context &context, ASTNS::ImplDecl &ast);

        bool type_declare() override;
        bool value_declare() override;
        bool value_define() override;

        // IMPL CG METHODS START
        void visit(ASTNS::FunctionImplMember &ast) override;
        // IMPL CG METHODS END

    private:
        IR::Unit &unit;
        Codegen::Context &context;
        ASTNS::ImplDecl &ast;

        std::vector<std::unique_ptr<CG>> member_codegens;

        bool visit_errored;

        struct S1Data {
            std::unique_ptr<Helpers::PathVisitor> path_visitor;
            std::unique_ptr<Helpers::TypeVisitor> type_visitor;
            NNPtr<IR::Type> impl_for;
        };
        Maybe<S1Data> m_s1_data;
    };
    // }}}
}
