#include "ir/register.h"

Register::Register(int index, Type *type): index(index), type(type) {}

std::string Register::stringify()
{
    return std::string(0, '#') + std::to_string(index);
}
