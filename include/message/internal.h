#pragma once

#include <string>

struct Token;
struct Location;

void invalidTok [[ noreturn ]] (std::string const &name, Token const &primary);
void calledWithOpTyNEthis [[ noreturn ]] (std::string const &classN, std::string const &fnn, std::string const &opname);
void outOSwitchDDefaultLab [[ noreturn ]] (std::string const &fnn, Location const &highlight);
void fCalled [[ noreturn ]] (std::string const &fnn);
void outOSwitchNoh [[ noreturn ]] (std::string const &fnn);
