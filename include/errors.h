#pragma once

#include <iostream>
#include <string>

#include "token.h"

void reportError(Token const &t, const std::string &message, const std::string &source);

void reportDebug(Token const &t, const std::string &message, const std::string &source);
