#pragma once

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "ast/astfwd.h"

#include "ir/value.h"
#include "ir/type.h"
#include "ir/instructionfwd.h"
#include "ir/module.h"

// CodeGen happens in 4 stages
// Stage 0 (begin):
//     - the unit has an empty symbol table, functions are empty
//     - everything is empty
// Stage 1 (type forward declarations):
//     - the type symbol table is built
//     - it was built with types, namespace
//     - imported modules are also added here
// Stage 2 (value forward declarations):
//     - the value symbol table is built
//     - it is with functions and global variables
//     - functions are now forward declared
//     - impl blocks have the member functions forward declared
//     - global variables (in the future) will be declared and assigned at this stage
// Stage 3 (block codegen):
//     - code is generated for functions bodies
//     - ... (?)
//
// Each kind of declaration has a Stage0, Stage1, and Stage2 class
// Each kind of declaration should have a function converting a
//   stage0 -> stage1, and stage1 -> stage2 class for that kind

namespace CodeGen {
    namespace Helpers {
        // Param {{{
        class CodeGen::ParamVisitor : public ASTNS::ParamBVisitor {
        public:
            struct Param {
                NNPtr<IR::Type const> ty;
                std::string name;
                NNPtr<ASTNS::ParamB> ast;
                bool mut;
            };

            ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Maybe<NNPtr<IR::Type>> this_type);

            std::vector<Param> ret;

            bool errored;

            bool is_method, this_ptr, this_mut;

        private:
            // PARAMVISITOR METHODS START
            void visit(ASTNS::Param &ast) override;
            void visit(ASTNS::ThisParam &ast) override;
            // PARAMVISITOR METHODS END

            CodeGen &cg;
            Maybe<NNPtr<IR::Type>> this_type;
            int index;
        };
        // }}}
        // Arg {{{
        class CodeGen::ArgVisitor : public ASTNS::ArgBVisitor {
        public:
            ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args);
            std::vector<IR::ASTValue> ret;

        private:
            // ARGSVISITOR METHODS START
            void visit(ASTNS::Arg &ast) override;
            // ARGSVISITOR METHODS END

            CodeGen::FunctionCodeGen &fcg;
        };
        // }}}
        // Path {{{
        class CodeGen::PathVisitor : public ASTNS::PathBVisitor {
        public:
            PathVisitor(CodeGen &cg, CodeGen::FunctionCodeGen &fcg);

            Maybe<IR::ASTValue> resolve_value(ASTNS::PathB &path);
            Maybe<IR::DeclSymbol &> resolve_decl_symbol(ASTNS::PathB &path);

        private:
            enum class PathType { VALUE, DECLARED } pty;

            Maybe<IR::ASTValue> vret;
            Maybe<NNPtr<IR::DeclSymbol>> dret;

            // PATH VISITOR START
            void visit(ASTNS::Path &ast) override;
            // PATH VISITOR END

            CodeGen &cg;
            NNPtr<FunctionCodeGen> fcg;
        };
        // }}}
// TypeVisitor {{{
class CodeGen::TypeVisitor : public ASTNS::TypeVisitor {
public:
    TypeVisitor(CodeGen &cg, Maybe<NNPtr<IR::Type>> this_type);

    Maybe<IR::Type &> type(ASTNS::Type &ast);

private:
    // TYPEVISITOR METHODS START
    void visit(ASTNS::PathType &ast) override;
    void visit(ASTNS::PointerType &ast) override;
    void visit(ASTNS::ThisType &ast) override;
    // TYPEVISITOR METHODS END

    Maybe<NNPtr<IR::Type>> ret;
    Maybe<NNPtr<IR::Type>> this_type;

    CodeGen &cg;
};
// }}}
// ForwDecl {{{
class CodeGen::ForwDecl : public ASTNS::DeclVisitor, public ASTNS::CUBVisitor {
public:
    ForwDecl(CodeGen &cg);

private:
    // FORWDECL METHODS START
    void visit(ASTNS::ImplicitDecl &ast) override;
    void visit(ASTNS::CU &ast) override;
    void visit(ASTNS::ImplDecl &ast) override;
    void visit(ASTNS::FunctionDecl &ast) override;
    // FORWDECL METHODS END

    CodeGen &cg;
};
// }}}
    // StmtCodeGen {{{
    class StmtCodeGen : public ASTNS::StmtVisitor, public ASTNS::VStmtIBVisitor {
    public:
        StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

        void stmt(ASTNS::Stmt &ast);

    private:
        // STMTCG METHODS START
        void visit(ASTNS::VarStmt &ast) override;
        void visit(ASTNS::VarStmtItem &ast) override;
        void visit(ASTNS::ExprStmt &ast) override;
        void visit(ASTNS::RetStmt &ast) override;
        // STMTCG METHODS END

        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
    // ExprCodeGen {{{
    class ExprCodeGen : public ASTNS::ExprVisitor {
    public:
        ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

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

        Maybe<IR::ASTValue> ret;
        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
// Decls {{{1
// }}}1
// Param and Args {{{1
// Param {{{2
class CodeGen::ParamVisitor : public ASTNS::ParamBVisitor {
public:
    struct Param {
        NNPtr<IR::Type const> ty;
        std::string name;
        NNPtr<ASTNS::ParamB> ast;
        bool mut;
    };

    ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Maybe<NNPtr<IR::Type>> this_type);

    std::vector<Param> ret;

    bool errored;

    bool is_method, this_ptr, this_mut;

private:
    // PARAMVISITOR METHODS START
    void visit(ASTNS::Param &ast) override;
    void visit(ASTNS::ThisParam &ast) override;
    // PARAMVISITOR METHODS END

    CodeGen &cg;
    Maybe<NNPtr<IR::Type>> this_type;
    int index;
};

// Arg {{{2
class CodeGen::ArgVisitor : public ASTNS::ArgBVisitor {
public:
    ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args);
    std::vector<IR::ASTValue> ret;

private:
    // ARGSVISITOR METHODS START
    void visit(ASTNS::Arg &ast) override;
    // ARGSVISITOR METHODS END

    CodeGen::FunctionCodeGen &fcg;
};
// Function {{{2
class CodeGen::FunctionCodeGen {
public:
    FunctionCodeGen(CodeGen &cg, NNPtr<ASTNS::FunctionDecl> ast, NNPtr<IR::Function> fun, Maybe<NNPtr<IR::Type>> this_type);

    bool codegen();

    struct Local {
        size_t scopenum;
        NNPtr<IR::Instrs::Register> v;
        std::string name;
    };

    std::vector<Local> locals;
    size_t cur_scope;

    void add_local(std::string const &name, IR::Instrs::Register &val);
    Maybe<Local&> get_local(std::string const &name);

    void inc_scope();
    void dec_scope();

    CodeGen &cg;
    NNPtr<ASTNS::FunctionDecl> ast;

    ExprCodeGen expr_cg;
    StmtCodeGen stmt_cg;

    NNPtr<IR::Function> fun;
    NNPtr<IR::Block> register_block;
    NNPtr<IR::Block> entry_block;
    NNPtr<IR::Block> exit_block;
    NNPtr<IR::Block> cur_block;
    NNPtr<IR::Instrs::Register> ret;

    TypeVisitor type_visitor;
    PathVisitor path_visitor;

    bool errored;
};
// Impl {{{2
class CodeGen::ImplCodeGen : public ASTNS::ImplMemberVisitor {
public:
    ImplCodeGen(CodeGen &cg, NNPtr<ASTNS::ImplDecl> ast);

    bool codegen();

private:
    // IMPLCG METHODS START
    void visit(ASTNS::FunctionImplMember &ast) override;
    // IMPLCG METHODS END

    CodeGen &cg;
    NNPtr<ASTNS::ImplDecl const> ast;

    Maybe<NNPtr<IR::Type>> impl_for;

    bool errored;
};
*/
    }

    class Stage3CG;
    class Stage2CG;
    class Stage1CG;

    class Stage0CG {
        virtual std::unique_ptr<Stage1CG> type_fw_declare() const = 0;
    };
    class Stage1CG {
        virtual std::unique_ptr<Stage2CG> value_fw_declare() const = 0;
    };
    class Stage2CG {
        virtual std::unique_ptr<Stage3CG> block_codegen() const = 0;
    };
    class Stage3CG {
        // codegen finished: no more stages to be done
    };

    namespace Function {
        class Stage0 {
            std::unique_ptr<Stage1> type_fw_declare() const override;
        };
        class Stage1 {
            std::unique_ptr<Stage2> value_fw_declare() const override;
        };
        class Stage2 {
            std::unique_ptr<Stage3> block_codegen() const override;
        };
        class Stage3 {};
    }

    namespace Impl {
        class Stage0 {
            std::unique_ptr<Stage1> type_fw_declare() const override;
        };
        class Stage1 {
            std::unique_ptr<Stage2> value_fw_declare() const override;
        };
        class Stage2 {
            std::unique_ptr<Stage3> block_codegen() const override;
        };
        class Stage3 {};
    }
}

/*
class CodeGen : public ASTNS::CUBVisitor, public ASTNS::DeclVisitor {
    class ForwDecl;
    class Declarator;

    class TypeVisitor;

    class ParamVisitor;
    class ArgVisitor;

    class FunctionCodeGen;
    class ImplCodeGen;

    class PathVisitor;
public:
    class Context;

    CodeGen(File const &file, NNPtr<ASTNS::CUB> cub);
    ~CodeGen();

    void forwdecl();
    void declarate();
    void codegen();

    std::unique_ptr<IR::Unit> unit;

    std::unique_ptr<Context> context;

    inline bool is_errored() { return errored; }

private:
    // CG METHODS START
    void visit(ASTNS::ImplicitDecl &ast) override;
    void visit(ASTNS::CU &ast) override;
    void visit(ASTNS::ImplDecl &ast) override;
    void visit(ASTNS::FunctionDecl &ast) override;
    // CG METHODS END

    bool errored;

    NNPtr<ASTNS::CUB> cub;
};
