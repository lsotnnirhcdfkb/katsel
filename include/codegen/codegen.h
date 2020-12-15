#pragma once

#include <memory>

namespace llvm { class raw_ostream; }
namespace ASTNS { class CUB; }
namespace IR { class Unit; }
struct File;

class CodeGen
{
    class Context;

    class ForwDecl;

    class TypeVisitor;

    class ParamVisitor;
    class ArgVisitor;

    class FunctionCodeGen;

public:
    CodeGen(File const &file);
    ~CodeGen();

    void declarate(ASTNS::CUB *cub);
    void codegen(ASTNS::CUB *cub);

    void printUnit(llvm::raw_ostream &ostream);

    std::unique_ptr<IR::Unit> unit;

    std::unique_ptr<Context> context;

    std::unique_ptr<TypeVisitor> typeVisitor;

    bool errored;
};
