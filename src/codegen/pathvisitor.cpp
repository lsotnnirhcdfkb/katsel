#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"

CodeGen::PathVisitor::PathVisitor(CodeGen &cg): cg(cg) {}

Maybe<NNPtr<IR::DeclSymbol>> CodeGen::PathVisitor::resolveDeclSymbol(NNPtr<ASTNS::PathB> ast)  {
    pty = PathType::DECLARED;
    dret = Maybe<NNPtr<IR::DeclSymbol>>();
    ast->accept(*this);
    return dret;
}

Maybe<IR::ASTValue> CodeGen::PathVisitor::resolveValue(NNPtr<ASTNS::PathB> ast, FunctionCodeGen &fcg)  {
    this->fcg = NNPtr<FunctionCodeGen>(fcg);
    pty = PathType::VALUE;
    vret = Maybe<IR::ASTValue>();
    ast->accept(*this);
    this->fcg = Maybe<NNPtr<FunctionCodeGen>>();
    return vret;
}

static Maybe<NNPtr<IR::DeclSymbol>> tracePathDeclOnly(NNPtr<IR::DeclSymbol> start, std::vector<Token>::const_iterator const tokStart, std::vector<Token>::const_iterator const tokEnd) {
    Maybe<NNPtr<IR::DeclSymbol>> prev;
    Maybe<NNPtr<IR::DeclSymbol>> current = start;
    for (auto curToken = tokStart; curToken != tokEnd; ++curToken) {
        prev = current;
        current = current.get()->getDeclSymbol(tokStart->stringify());

        if (!current.has()) {
            if (curToken != tokStart)
                ERR_NO_MEMBER_IN(prev.get(), *curToken);
            else
                ERR_UNDECL_SYMB(*curToken);
            return Maybe<NNPtr<IR::DeclSymbol>>();
        }
    }
    return current;
}

void CodeGen::PathVisitor::visit_path(ASTNS::Path &ast) {
    if (pty == PathType::DECLARED) {
        dret = tracePathDeclOnly(&cg.unit->mod, ast.segments.cbegin(), ast.segments.cend());
    } else {
        if (ast.segments.size() == 1) {
            // look for local or global variable
            std::string vname = ast.segments.back().stringify();
            Maybe<NNPtr<FunctionCodeGen::Local>> loc = fcg.get()->getLocal(vname);

            Maybe<NNPtr<IR::Value>> m_retVal;
            if (loc.has()) {
                m_retVal = loc.get()->v;
            } else {
                Maybe<NNPtr<IR::Value>> val = cg.unit->mod.getValue(vname);
                if (!val.has()) {
                    ERR_UNDECL_SYMB(ast);
                    vret = Maybe<IR::ASTValue>();
                    return;
                } else {
                    m_retVal = val;
                }
            }

            NNPtr<IR::Value> retVal = m_retVal.get();

            if (dynamic_cast<IR::Instrs::Register*>(retVal.asRaw()))
                vret = IR::ASTValue(fcg.get()->curBlock->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(retVal, ast))), ast);
            else
                vret = IR::ASTValue(retVal, ast);
        } else {
            // look through decl symbol table until last segment
            // look through value symbol table for last segment

            Maybe<NNPtr<IR::DeclSymbol>> m_last = tracePathDeclOnly(&cg.unit->mod, ast.segments.cbegin(), ast.segments.cend() - 1);
            if (!m_last.has()) {
                vret = Maybe<IR::ASTValue>();
                return;
            }

            NNPtr<IR::DeclSymbol> last = m_last.get();

            Maybe<NNPtr<IR::Value>> ret = last->getValue(ast.segments.back().stringify());

            if (!ret.has()) {
                ERR_NO_MEMBER_IN(last, ast.segments.back());
                vret = Maybe<IR::ASTValue>();
            } else {
                vret = IR::ASTValue(ret.get(), ast);
            }
        }
    }
}
