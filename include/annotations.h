#pragma once
struct ExprAn
{
    bool valid = false;
    bool isLValue;
    int type;
}
struct FuncDeclAn
{
    bool valid = false;
    int retType;
    int nArgs;
    std::vector<int> argTypes;
}
