#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

CodeGen::PathVisitor::PathVisitor(CodeGen &cg): cg(cg) {}

Maybe<NNPtr<IR::DeclSymbol>> CodeGen::PathVisitor::resolve_decl_symbol(NNPtr<ASTNS::PathB> ast)  {
    pty = PathType::DECLARED;
    dret = Maybe<NNPtr<IR::DeclSymbol>>();
    ast->accept(*this);
    return dret;
}

Maybe<IR::ASTValue> CodeGen::PathVisitor::resolve_value(NNPtr<ASTNS::PathB> ast, FunctionCodeGen &fcg)  {
    this->fcg = NNPtr<FunctionCodeGen>(fcg);
    pty = PathType::VALUE;
    vret = Maybe<IR::ASTValue>();
    ast->accept(*this);
    this->fcg = Maybe<NNPtr<FunctionCodeGen>>();
    return vret;
}

static Maybe<NNPtr<IR::DeclSymbol>> trace_path_decl_only(NNPtr<IR::DeclSymbol> start, std::vector<Token>::const_iterator const tok_start, std::vector<Token>::const_iterator const tok_end) {
    Maybe<NNPtr<IR::DeclSymbol>> prev;
    Maybe<NNPtr<IR::DeclSymbol>> current = start;
    for (auto cur_token = tok_start; cur_token != tok_end; ++cur_token) {
        prev = current;
        current = current.get()->get_decl_symbol(tok_start->stringify());

        if (!current.has()) {
            if (cur_token != tok_start)
                ERR_NO_MEMBER_IN(prev.get(), *cur_token);
            else
                ERR_UNDECL_SYMB(*cur_token);
            return Maybe<NNPtr<IR::DeclSymbol>>();
        }
    }
    return current;
}

void CodeGen::PathVisitor::visit(ASTNS::Path &ast) {
    if (pty == PathType::DECLARED) {
        dret = trace_path_decl_only(&cg.unit->mod, ast.segments.cbegin(), ast.segments.cend());
    } else {
        if (ast.segments.size() == 1) {
            // look for local or global variable
            std::string vname = ast.segments.back().stringify();
            Maybe<NNPtr<FunctionCodeGen::Local>> loc = fcg.get()->get_local(vname);

            Maybe<NNPtr<IR::Value>> m_ret_val;
            if (loc.has()) {
                m_ret_val = loc.get()->v;
            } else {
                Maybe<NNPtr<IR::Value>> val = cg.unit->mod.get_value(vname);
                if (!val.has()) {
                    ERR_UNDECL_SYMB(ast);
                    vret = Maybe<IR::ASTValue>();
                    return;
                } else {
                    m_ret_val = val;
                }
            }

            NNPtr<IR::Value> ret_val = m_ret_val.get();

            if (dynamic_cast<IR::Instrs::Register*>(ret_val.as_raw()))
                vret = IR::ASTValue(fcg.get()->cur_block->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(ret_val, ast))), ast);
            else
                vret = IR::ASTValue(ret_val, ast);
        } else {
            // look through decl symbol table until last segment
            // look through value symbol table for last segment

            Maybe<NNPtr<IR::DeclSymbol>> m_last = trace_path_decl_only(&cg.unit->mod, ast.segments.cbegin(), ast.segments.cend() - 1);
            if (!m_last.has()) {
                vret = Maybe<IR::ASTValue>();
                return;
            }

            NNPtr<IR::DeclSymbol> last = m_last.get();

            Maybe<NNPtr<IR::Value>> ret = last->get_value(ast.segments.back().stringify());

            if (!ret.has()) {
                ERR_NO_MEMBER_IN(last, ast.segments.back());
                vret = Maybe<IR::ASTValue>();
            } else {
                vret = IR::ASTValue(ret.get(), ast);
            }
        }
    }
}
