/// @file errors.h
/// A function to report and format errors in a consistent way

#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

#include "token.h"
#include "file.h"
#include "ansistuff.h"

/// Convenience function to format and report errors at a certain token and with a certain message
void reportError(Token const &t, const std::string &message, const File &sourcefile);
