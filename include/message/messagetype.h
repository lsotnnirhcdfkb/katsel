#pragma once

#include <string>

class Message
{
public:
    virtual std::string format() = 0; 
};
