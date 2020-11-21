#pragma once

#include <ostream>

class Register;
class Value;

namespace Instrs
{
    class Instruction
    {
    public:
        virtual ~Instruction() {};
        virtual void stringify(std::ostream &ss) = 0;
    };


    class Store : public Instruction
    {
    public:
        Store(Register *target, Value *value);
        void stringify(std::ostream &ss) override;

    private:
        Register *target;
        Value *value;
    };

    class Br
    {
    public:
        virtual ~Br();
        virtual void stringify(std::ostream &ss) = 0;
    };
}

