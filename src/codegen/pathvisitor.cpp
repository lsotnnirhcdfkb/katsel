#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

CodeGen::PathVisitor::PathVisitor(CodeGen &cg): cg(cg) {}

Maybe<IR::DeclSymbol &> CodeGen::PathVisitor::resolve_decl_symbol(ASTNS::PathB &ast)  {
    pty = PathType::DECLARED;
    dret = Maybe<NNPtr<IR::DeclSymbol>>();
    ast.accept(*this);
    return dret.fmap<IR::DeclSymbol &>( [] (NNPtr<IR::DeclSymbol> i) { return Maybe<IR::DeclSymbol &>(*i); });
}

Maybe<IR::ASTValue> CodeGen::PathVisitor::resolve_value(ASTNS::PathB &ast, FunctionCodeGen &fcg)  {
    this->fcg = NNPtr<FunctionCodeGen>(fcg);
    pty = PathType::VALUE;
    vret = Maybe<IR::ASTValue>();
    ast.accept(*this);
    this->fcg = Maybe<NNPtr<FunctionCodeGen>>();
    return vret;
}

static Maybe<IR::DeclSymbol &> trace_path_decl_only(IR::DeclSymbol &start, std::vector<Token>::const_iterator const tok_start, std::vector<Token>::const_iterator const tok_end) {
    Maybe<NNPtr<IR::DeclSymbol>> prev;
    Maybe<NNPtr<IR::DeclSymbol>> current = start;
    for (auto cur_token = tok_start; cur_token != tok_end; ++cur_token) {
        prev = current;
        current = current.get()->get_decl_symbol(tok_start->as<Tokens::Identifier>().name).fmap<NNPtr<IR::DeclSymbol>>([] (IR::DeclSymbol &ds) { return NNPtr<IR::DeclSymbol>(ds); });

        if (!current.has()) {
            if (cur_token != tok_start)
                ERR_NO_MEMBER_IN(*prev.get(), *cur_token);
            else
                ERR_UNDECL_SYMB(cur_token->span);
            return Maybe<IR::DeclSymbol &>();
        }
    }
    return current.fmap<IR::DeclSymbol &> ([] (NNPtr<IR::DeclSymbol> i) { return Maybe<IR::DeclSymbol &>(*i); });
}

void CodeGen::PathVisitor::visit(ASTNS::Path &ast) {
    if (pty == PathType::DECLARED) {
        dret = trace_path_decl_only(cg.unit->mod, ast.segments.cbegin(), ast.segments.cend()).fmap<NNPtr<IR::DeclSymbol >>([] (IR::DeclSymbol &i) { return Maybe<NNPtr<IR::DeclSymbol >>(NNPtr(i)); });
    } else {
        if (ast.segments.size() == 1 && fcg.has()) {
            // look for local
            std::string vname = ast.segments.back().as<Tokens::Identifier>().name;
            Maybe<FunctionCodeGen::Local&> loc = fcg.get()->get_local(vname);

            if (loc.has()) {
                IR::Instrs::Register &reg = *loc.get().v;
                IR::Instrs::DerefPtr &deref = fcg.get()->cur_block->add<IR::Instrs::DerefPtr>(IR::ASTValue(reg, ast));

                vret = IR::ASTValue(deref, ast);
                return;
            }
        } 

        // look through decl symbol table until last segment
        // look through value symbol table for last segment

        Maybe<IR::DeclSymbol &> m_last = trace_path_decl_only(cg.unit->mod, ast.segments.cbegin(), ast.segments.cend() - 1);
        if (!m_last.has()) {
            vret = Maybe<IR::ASTValue>();
            return;
        }

        NNPtr<IR::DeclSymbol> last = m_last.get();

        Maybe<IR::Value&> ret = last->get_value(ast.segments.back().as<Tokens::Identifier>().name);

        if (!ret.has()) {
            ERR_NO_MEMBER_IN(*last, ast.segments.back());
            vret = Maybe<IR::ASTValue>();
        } else {
            vret = IR::ASTValue(ret.get(), ast);
        }
    }
}
