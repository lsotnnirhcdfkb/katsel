
#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include "lex/token.h"
#include "utils/file.h"
#include "message/ansistuff.h"

void reportError(Token const &t, const std::string &message);
void reportWarning(Token const &t, const std::string &message);
void reportDebug(Token const &t, const std::string &message);
