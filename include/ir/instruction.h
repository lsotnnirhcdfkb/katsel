#pragma once

#include "ir/register.h"

namespace Instrs
{
    class Instruction
    {
    public:
        virtual ~Instruction() {};
        virtual void print() = 0;
    };


    class Load : public Instruction
    {
    public:
        Load(Register *target, Register *value);
        void print() override;

    private:
        Register *target;
        Register *value;
    };

    class LoadConst : public Instruction
    {
    public:
        LoadConst(Register *target, int value);
        void print() override;

    private:
        Register *target;
        int value;
    };

    class Br
    {
    public:
        virtual ~Br();
        virtual void print() = 0;
    };
}

