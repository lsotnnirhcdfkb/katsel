#pragma once

#ifdef NOASSERT
#define ASSERT(expr)
#else
#include <string>
void report_abort_noh [[ noreturn ]] (std::string const &message);
#define STRINGIFY2(a) #a
#define STRINGIFY(a) STRINGIFY2(a)
#define ASSERT(expr) { if (!(expr)) report_abort_noh("fatal: ASSERT(" #expr ") failed at " __FILE__ ":" STRINGIFY(__LINE__)); }
#endif
