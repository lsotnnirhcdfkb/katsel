#pragma once

#include "ir/block.h"
#include <string>
#include <cstddef>
#include <vector>

namespace ASTNS
{
    class AST;
    class Function;
}

namespace IR
{
    class Type;
    class BuiltinType;
    class FunctionType;

    class Value
    {
    public:
        virtual ~Value() {};
        virtual std::string stringify() const = 0;

        virtual bool assignable() const = 0;

        virtual Type* type() const = 0;
    };

    class Register : public Value
    {
    public:
        Register(int index, Type *type, ASTNS::AST *defAST, bool temp=true);

        std::string stringify() const override;
        void definition(std::ostream &os) const;
        ASTNS::AST* defAST() const;

        Type* type() const override;

        bool assignable() const override;

    private:
        ASTNS::AST* _defAST;
        int index;
        Type *ty;
        bool temp;
    };

    class Function : public Value
    {
    public:
        Function(FunctionType *ty, std::string name, ASTNS::Function *defAST);

        void add(std::unique_ptr<Block> block);

        std::string stringify() const override;
        void definition(std::ostream &os) const;
        ASTNS::Function* defAST() const;
        void cfgDot(std::ostream &os) const;

        Type* type() const override;

        bool assignable() const override;

        std::vector<std::unique_ptr<Block>> blocks;
        std::vector<std::unique_ptr<Register>> registers;

        Block* addBlock(std::string name);
        Register* addRegister(Type *ty, ASTNS::AST *defAST, bool temp=true);

        FunctionType *ty;
        std::string name;

    private:
        ASTNS::Function *_defAST;

        size_t blocki;
        size_t regi;
    };

    class ConstInt : public Value
    {
    public:
        ConstInt(BuiltinType *ty, int val);

        std::string stringify() const override;

        Type* type() const override;

        bool assignable() const override;

        int val;

    private:
        BuiltinType *ty;
    };

    struct ASTValue
    {
        // Because multiple ASTs can evaluate to the same value (same pointer)
        // (see primary expressions (multiple PrimaryASTs can evaluate to the same register),
        // also especially since ConstInts are uniqued together), 
        // this struct allows values to be associated with an ast, and more importantly,
        // it isn't supposed to be heap-allocated and uniqued, so one value can have multiple ASTs
        Value *val;
        ASTNS::AST *ast;

        inline ASTValue(Value *val, ASTNS::AST *ast): val(val), ast(ast) {}
        inline ASTValue(): val(nullptr), ast(nullptr) {}

        explicit inline operator bool() const
        {
            return val;
        }

        inline Type* type() const
        {
            return val->type();
        }
        inline std::string stringify() const
        {
            return val->stringify();
        }
        inline bool assignable() const
        {
            return val->assignable();
        }
    };
}
