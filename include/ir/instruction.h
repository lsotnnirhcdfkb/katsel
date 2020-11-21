#pragma once

class Instruction
{
public:
    virtual ~Instruction() {};
    virtual void print() = 0;
};

class Branch
{
public:
    virtual ~Branch();
    virtual void print() = 0;
};
