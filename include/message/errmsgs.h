#pragma once

#include "message/errors.h"

enum class ErrCode
{

};

// according to cdecl:
// declare lookupErr as function returning pointer to function returning void
const constexpr void (*lookupErr())();
