#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

Codegen::Helpers::PathVisitor::PathVisitor(Maybe<Locals&> locals, IR::Unit &unit):
    locals(locals.fmap([](Locals& l) { return NNPtr<Locals>(l); })),
    unit(unit) {}

Maybe<IR::DeclSymbol &> Codegen::Helpers::PathVisitor::resolve_decl_symbol(ASTNS::PathB &ast)  {
    pty = PathType::DECLARED;
    dret = Maybe<NNPtr<IR::DeclSymbol>>();
    ast.ast_accept(*this);
    return dret.fmap([](NNPtr<IR::DeclSymbol> i) -> IR::DeclSymbol & { return *i; });
}

Maybe<Located<NNPtr<IR::Value>>> Codegen::Helpers::PathVisitor::resolve_value(ASTNS::PathB &ast)  {
    pty = PathType::VALUE;
    vret = Maybe<Located<NNPtr<IR::Value>>>();
    ast.ast_accept(*this);
    return vret;
}

static Maybe<IR::DeclSymbol &> trace_path_decl_only(IR::DeclSymbol &start, std::vector<Located<Tokens::Identifier>>::const_iterator const tok_start, std::vector<Located<Tokens::Identifier>>::const_iterator const tok_end) {
    NNPtr<IR::DeclSymbol> prev = start;
    NNPtr<IR::DeclSymbol> current = start;
    for (auto cur_token = tok_start; cur_token != tok_end; ++cur_token) {
        prev = current;
        auto m_current = current->get_decl_symbol(cur_token->value.name).fmap([](IR::DeclSymbol &ds) { return NNPtr<IR::DeclSymbol>(ds); });

        if (m_current.has()) {
            current = m_current.get();
        } else {
            if (prev.as_raw() != &start)
                Errors::NO_MEMBER_IN(*prev, cur_token->span);
            else
                Errors::UNDECL_SYMB(cur_token->span);
            return Maybe<IR::DeclSymbol &>();
        }
    }
    return *current;
}

void Codegen::Helpers::PathVisitor::ast_visit(ASTNS::Path &ast) {
    if (pty == PathType::DECLARED) {
        dret = trace_path_decl_only(unit.mod, ast.segments.cbegin(), ast.segments.cend()).fmap([](IR::DeclSymbol &i) { return NNPtr<IR::DeclSymbol>(i); });
    } else {
        if (ast.segments.size() == 1 && locals.has()) {
            // look for local
            std::string const &vname = ast.segments.back().value.name;
            Maybe<Local> loc = locals.get()->get_local(vname);

            if (loc.has()) {
                vret = Located(ast, *loc.get().v);
                return;
            }
        } 

        // look through decl symbol table until last segment
        // look through value symbol table for last segment

        Maybe<IR::DeclSymbol &> m_last = trace_path_decl_only(unit.mod, ast.segments.cbegin(), ast.segments.cend() - 1);
        if (!m_last.has()) {
            vret = Maybe<Located<NNPtr<IR::Value>>>();
            return;
        }

        NNPtr<IR::DeclSymbol> last = m_last.get();

        Maybe<IR::Value&> ret = last->get_value(ast.segments.back().value.name);

        if (!ret.has()) {
            if (last.as_raw() != &unit.mod)
                Errors::NO_MEMBER_IN(*last, ast.segments.back().span);
            else
                Errors::UNDECL_SYMB(ast.segments.back().span);
            vret = Maybe<Located<NNPtr<IR::Value>>>();
        } else {
            vret = Located(ast, ret.get());
        }
    }
}
