#pragma once

#include <iostream>
#include <string>

#include "token.h"

void reportError(Token t, std::string message);

void reportDebug(Token t, std::string message);
