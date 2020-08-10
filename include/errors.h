#pragma once

#include <iostream>
#include <string>

#include "token.h"

void reportError(Token &t, const std::string &message);

void reportDebug(Token &t, const std::string &message);
