#include "ir/register.h"

Register::Register(int index, bool temp, Type *type): index(index), temp(temp), type(type) {}
