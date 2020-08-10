#pragma once

#include <iostream>
#include <string>

#include "token.h"

void reportError(Token &t, const std::string &message, const std::string &source);

void reportDebug(Token &t, const std::string &message, const std::string &source);
