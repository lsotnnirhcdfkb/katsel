/// @file file.h
/// A file containing the File struct

#pragma once

#include <string>

/// A file struct for passing around the compiler
struct File
{
    /// The filename of the file
    std::string filename;
    /// The contents of the file
    std::string source;
};
