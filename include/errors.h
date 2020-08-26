#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include "token.h"
#include "file.h"

void reportError(Token const &t, const std::string &message, const File &sourcefile);
