#pragma once

class Register;
class Value;

namespace Instrs
{
    class Instruction
    {
    public:
        virtual ~Instruction() {};
        virtual void print() = 0;
    };


    class Store : public Instruction
    {
    public:
        Store(Register *target, Value *value);
        void print() override;

    private:
        Register *target;
        Value *value;
    };

    class Br
    {
    public:
        virtual ~Br();
        virtual void print() = 0;
    };
}

