#pragma once

#include <memory>

#include "ast/visitor.h"
#include "utils/ptr.h"

namespace llvm { class raw_ostream; }
namespace IR { class Unit; }
struct File;

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

    std::unique_ptr<TypeVisitor> type_visitor;
    std::unique_ptr<PathVisitor> path_visitor;

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
