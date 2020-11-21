#pragma once

class Instruction
{
public:
    virtual ~Instruction() {};
    virtual void print() = 0;
};
