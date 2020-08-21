#pragma once

#include <iostream>
#include <string>

#include "token.h"
#include "file.h"

void reportError(Token const &t, const std::string &message, const File &sourcefile);

void reportDebug(Token const &t, const std::string &message, const File &sourcefile);
