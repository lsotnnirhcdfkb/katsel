#pragma once

#include <ostream>

class Block;
class Register;
class Value;

namespace Instrs
{
    class Instruction
    {
    public:
        virtual ~Instruction() {};
        virtual void stringify(std::ostream &os) const = 0;
    };


    class Store : public Instruction
    {
    public:
        Store(Register *target, Value *value);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *value;
    };

    class Or : public Instruction
    {
    public:
        Or(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class And : public Instruction
    {
    public:
        And(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class IntCmpNE : public Instruction
    {
    public:
        IntCmpNE(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class IntCmpEQ : public Instruction
    {
    public:
        IntCmpEQ(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class IntCmpULT : public Instruction
    {
    public:
        IntCmpULT(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class IntCmpUGT : public Instruction
    {
    public:
        IntCmpUGT(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class IntCmpULE : public Instruction
    {
    public:
        IntCmpULE(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class IntCmpUGE : public Instruction
    {
    public:
        IntCmpUGE(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class FloatCmpNE : public Instruction
    {
    public:
        FloatCmpNE(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class FloatCmpEQ : public Instruction
    {
    public:
        FloatCmpEQ(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class FloatCmpULT : public Instruction
    {
    public:
        FloatCmpULT(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class FloatCmpUGT : public Instruction
    {
    public:
        FloatCmpUGT(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class FloatCmpULE : public Instruction
    {
    public:
        FloatCmpULE(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class FloatCmpUGE : public Instruction
    {
    public:
        FloatCmpUGE(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class BitXor : public Instruction
    {
    public:
        BitXor(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class BitOr : public Instruction
    {
    public:
        BitOr(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class BitAnd : public Instruction
    {
    public:
        BitAnd(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class BitNot : public Instruction
    {
    public:
        BitNot(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class ShiftR : public Instruction
    {
    public:
        ShiftR(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class ShiftL : public Instruction
    {
    public:
        ShiftL(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class Add : public Instruction
    {
    public:
        Add(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class Sub : public Instruction
    {
    public:
        Sub(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class Mult : public Instruction
    {
    public:
        Mult(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class Div : public Instruction
    {
    public:
        Div(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class Mod : public Instruction
    {
    public:
        Mod(Register *target, Value *lhs, Value *rhs);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *lhs, *rhs;
    };
    class Neg : public Instruction
    {
    public:
        Neg(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class Trunc : public Instruction
    {
    public:
        Trunc(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class ZeroExt : public Instruction
    {
    public:
        ZeroExt(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class SignExt : public Instruction
    {
    public:
        SignExt(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class FloatTrunc : public Instruction
    {
    public:
        FloatTrunc(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class FloatExt : public Instruction
    {
    public:
        FloatExt(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class SIntToFloat : public Instruction
    {
    public:
        SIntToFloat(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class UIntToFloat : public Instruction
    {
    public:
        UIntToFloat(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class FloatToSInt : public Instruction
    {
    public:
        FloatToSInt(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };
    class FloatToUInt : public Instruction
    {
    public:
        FloatToUInt(Register *target, Value *op);
        void stringify(std::ostream &os) const override;

    private:
        Register *target;
        Value *op;
    };

    class Br
    {
    public:
        virtual ~Br();
        virtual void stringify(std::ostream &os) const = 0;
    };

    class GotoBr : public Br
    {
    public:
        GotoBr(Block *b);
        void stringify(std::ostream &os) const override;

    private:
        Block *b;
    };

    class Return : public Br
    {
    public:
        Return(Value *value);
        void stringify(std::ostream &os) const override;

    private:
        Value *value;
    };
}

