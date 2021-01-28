#pragma once

#include <string>

class Token;
class Location;

void invalid_tok [[ noreturn ]] (std::string const &name, Span const &primary);
void called_with_op_ty_nethis [[ noreturn ]] (std::string const &class_n, std::string const &fnn, std::string const &opname);
void out_oswitch_ddefault_lab [[ noreturn ]] (std::string const &fnn, Location const &highlight);
void f_called [[ noreturn ]] (std::string const &fnn);
void out_oswitch_noh [[ noreturn ]] (std::string const &fnn);
