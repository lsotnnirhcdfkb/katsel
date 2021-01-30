#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

CodeGen::Helpers::PathVisitor::PathVisitor(Maybe<Locals&> locals, IR::Module &mod, IR::Builder &ir_builder):
    locals(locals),
    mod(mod),
    ir_builder(ir_builder) {}

Maybe<IR::DeclSymbol &> CodeGen::Helpers::PathVisitor::resolve_decl_symbol(ASTNS::PathB &ast)  {
    pty = PathType::DECLARED;
    dret = Maybe<NNPtr<IR::DeclSymbol>>();
    ast.accept(*this);
    return dret.fmap<IR::DeclSymbol &>( [] (NNPtr<IR::DeclSymbol> i) { return Maybe<IR::DeclSymbol &>(*i); });
}

Maybe<IR::ASTValue> CodeGen::Helpers::PathVisitor::resolve_value(ASTNS::PathB &ast)  {
    pty = PathType::VALUE;
    vret = Maybe<IR::ASTValue>();
    ast.accept(*this);
    return vret;
}

static Maybe<IR::DeclSymbol &> trace_path_decl_only(IR::DeclSymbol &start, std::vector<Located<Tokens::Identifier>>::const_iterator const tok_start, std::vector<Located<Tokens::Identifier>>::const_iterator const tok_end) {
    NNPtr<IR::DeclSymbol> prev = start;
    NNPtr<IR::DeclSymbol> current = start;
    for (auto cur_token = tok_start; cur_token != tok_end; ++cur_token) {
        prev = current;
        auto m_current = current->get_decl_symbol(cur_token->value.name).fmap<NNPtr<IR::DeclSymbol>>([] (IR::DeclSymbol &ds) { return NNPtr<IR::DeclSymbol>(ds); });

        if (m_current.has()) {
            current = m_current.get();
        } else {
            if (prev.as_raw() != &start)
                ERR_NO_MEMBER_IN(*prev, cur_token->span);
            else
                ERR_UNDECL_SYMB(cur_token->span);
            return Maybe<IR::DeclSymbol &>();
        }
    }
    return *current;
}

void CodeGen::Helpers::PathVisitor::visit(ASTNS::Path &ast) {
    if (pty == PathType::DECLARED) {
        dret = trace_path_decl_only(mod, ast.segments.cbegin(), ast.segments.cend()).fmap<NNPtr<IR::DeclSymbol >>([] (IR::DeclSymbol &i) { return Maybe<NNPtr<IR::DeclSymbol>>(NNPtr(i)); });
    } else {
        if (ast.segments.size() == 1 && locals.has()) {
            // look for local
            std::string vname = ast.segments.back().value.name;
            Maybe<Local> loc = locals.get().get_local(vname);

            if (loc.has()) {
                IR::Instrs::Register &reg = *loc.get().v;
                IR::Instrs::DerefPtr &deref = ir_builder.cur_block()->add<IR::Instrs::DerefPtr>(IR::ASTValue(reg, ast));

                vret = IR::ASTValue(deref, ast);
                return;
            }
        } 

        // look through decl symbol table until last segment
        // look through value symbol table for last segment

        Maybe<IR::DeclSymbol &> m_last = trace_path_decl_only(mod, ast.segments.cbegin(), ast.segments.cend() - 1);
        if (!m_last.has()) {
            vret = Maybe<IR::ASTValue>();
            return;
        }

        NNPtr<IR::DeclSymbol> last = m_last.get();

        Maybe<IR::Value&> ret = last->get_value(ast.segments.back().value.name);

        if (!ret.has()) {
            if (last.as_raw() != &mod)
                ERR_NO_MEMBER_IN(*last, ast.segments.back().span);
            else
                ERR_UNDECL_SYMB(ast.segments.back().span);
            vret = Maybe<IR::ASTValue>();
        } else {
            vret = IR::ASTValue(ret.get(), ast);
        }
    }
}
