#include "codegen/codegen.h"
#include "codegenlocal.h"
#include "ast/visitor.h"
#include "ast/ast.h"
#include "ir/function.h"
#include "ir/unit.h"

namespace {
    class _CG : public ASTNS::DeclVisitor, public ASTNS::CUBVisitor {
    public:
        _CG(File const &file):
            unit(std::make_unique<IR::Unit>(file)),
            success(true) {}

        // MAINCG METHODS START
        void visit(ASTNS::ImplicitDecl &ast) override;
        void visit(ASTNS::CU &ast) override;
        void visit(ASTNS::ImplDecl &ast) override;
        void visit(ASTNS::FunctionDecl &ast) override;
        // MAINCG METHODS END

        // for reasons beyond my understanding, this member has to be
        // wrapped in a unique_ptr, because if it is not, then even
        // when using:
        //     IR::Unit unit = std::move(cg.unit);
        // where 'cg' is an instance of this class, it still needs to
        // instantiate a constructor of the field 'Unit::functions' of
        // type 'std::vector<std::unique_ptr<IR::Function>>', which for
        // some reason instantiates std::uninitialized_copy, which
        // requires that std::unique_ptr<IR::Function> has a copy
        // constructor, and the copy constructor for std::unique_ptr is
        // supposed to be (and is) explicitly deleted
        std::unique_ptr<IR::Unit> unit;

        std::vector<std::unique_ptr<Codegen::CG>> codegens;

        void run() {
            unit->mod.add_decl_symbol("void", unit->context.get_void_type());
            unit->mod.add_decl_symbol("float", unit->context.get_float_type(32));
            unit->mod.add_decl_symbol("double", unit->context.get_float_type(64));
            unit->mod.add_decl_symbol("bool", unit->context.get_bool_type());
            unit->mod.add_decl_symbol("char", unit->context.get_char_type());
            unit->mod.add_decl_symbol("uint8", unit->context.get_int_type(8, false));
            unit->mod.add_decl_symbol("uint16", unit->context.get_int_type(16, false));
            unit->mod.add_decl_symbol("uint32", unit->context.get_int_type(32, false));
            unit->mod.add_decl_symbol("uint64", unit->context.get_int_type(64, false));
            unit->mod.add_decl_symbol("sint8", unit->context.get_int_type(8, true));
            unit->mod.add_decl_symbol("sint16", unit->context.get_int_type(16, true));
            unit->mod.add_decl_symbol("sint32", unit->context.get_int_type(32, true));
            unit->mod.add_decl_symbol("sint64", unit->context.get_int_type(64, true));

#define DEFINE_PASS(input_codegens, output_codegens, stage_name) \
            std::vector<std::unique_ptr<Codegen::CG>> output_codegens; \
            for (std::unique_ptr<Codegen::CG> &cg : input_codegens) { \
                if (cg->stage_name()) \
                    output_codegens.push_back(std::move(cg)); \
                else \
                    success = false; \
            }

            DEFINE_PASS(codegens, after_type_decl, type_declare);
            DEFINE_PASS(after_type_decl, after_value_decl, value_declare);
            DEFINE_PASS(after_value_decl, after_value_def, value_define);
#undef DEFINE_PASS
        }

        bool success;
    };
}

Maybe<std::unique_ptr<IR::Unit>> Codegen::codegen(NNPtr<ASTNS::CUB> cub) {
    _CG cg (cub->file);

    cub->accept(cg);
    cg.run();

    if (cg.success)
        return std::move(cg.unit);
    else
        return Maybe<std::unique_ptr<IR::Unit>>();
}

void _CG::visit(ASTNS::ImplicitDecl &ast) {}

void _CG::visit(ASTNS::CU &ast) {
    for (auto &decl : ast.decls) {
        decl->accept(*this);
    }
}

void _CG::visit(ASTNS::ImplDecl &ast) {
    codegens.push_back(std::make_unique<Codegen::Impl>(*unit, unit->context, ast));
}
void _CG::visit(ASTNS::FunctionDecl &ast) {
    codegens.push_back(std::make_unique<Codegen::Function>(*unit, unit->context, ast, Maybe<NNPtr<IR::Type>>(), unit->mod));
}
