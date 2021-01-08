#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::PathVisitor::PathVisitor(CodeGen &cg): cg(cg) {}

IR::DeclSymbol* CodeGen::PathVisitor::resolveDeclSymbol(ASTNS::PathB *ast)  {
    pty = PathType::DECLARED;
    dret = nullptr;
    ast->accept(this);
    return dret;
}

IR::ASTValue CodeGen::PathVisitor::resolveValue(ASTNS::PathB *ast, FunctionCodeGen &fcg)  {
    this->fcg = &fcg;
    pty = PathType::VALUE;
    vret = IR::ASTValue();
    ast->accept(this);
    this->fcg = nullptr;
    return vret;
}

static IR::DeclSymbol* tracePathDeclOnly(IR::DeclSymbol *current, std::vector<Token>::const_iterator const tokStart, std::vector<Token>::const_iterator const tokEnd) {
    IR::DeclSymbol *prev = nullptr;
    for (auto curToken = tokStart; curToken != tokEnd; ++curToken) {
        prev = current;
        current = current->getDeclSymbol(tokStart->stringify());

        if (!current) {
            if (curToken != tokStart)
                ERR_NO_ITEM_IN(prev, *curToken);
            else
                ERR_UNDECL_SYMB(*curToken);
            return nullptr;
        }
    }
    return current;
}

void CodeGen::PathVisitor::visitPath(ASTNS::Path *ast) {
    if (pty == PathType::DECLARED) {
        dret = tracePathDeclOnly(&cg.unit->mod, ast->segments.cbegin(), ast->segments.cend());
    } else {
        if (ast->segments.size() == 1) {
            // look for local or global variable
            std::string vname = ast->segments.back().stringify();
            FunctionCodeGen::Local *loc = fcg->getLocal(vname);

            IR::Value *retVal;
            if (loc) {
                retVal = loc->v;
            } else {
                IR::Value *val = cg.unit->mod.getValue(vname);
                if (!val) {
                    ERR_UNDECL_SYMB(ast);
                    vret = IR::ASTValue();
                    return;
                } else {
                    retVal = val;
                }
            }

            if (dynamic_cast<IR::Instrs::Register*>(retVal))
                vret = IR::ASTValue(fcg->curBlock->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(retVal, ast))), ast);
            else
                vret = IR::ASTValue(retVal, ast);
        } else {
            // look through type symbol table until last segment
            // look through value symbol table for last segment

            IR::DeclSymbol *last = tracePathDeclOnly(&cg.unit->mod, ast->segments.cbegin(), ast->segments.cend() - 1);
            IR::Value *ret = last->getValue(ast->segments.back().stringify());
            if (!ret) {
                ERR_NO_ITEM_IN(last, ast->segments.back());
                vret = IR::ASTValue();
            } else {
                vret = IR::ASTValue(ret, ast);
            }
        }
    }
}
