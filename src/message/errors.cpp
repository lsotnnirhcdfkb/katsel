#include "errors.h"
#include "message/error.h"
#include "message/ansistuff.h"
#include "utils/format.h"
#include <iostream>
#include <cstdlib>
#include <typeinfo>

ErrorFormat errformat = ErrorFormat::HUMAN;
// constructors for location {{{1
Location::Location(Token const &t): start(t.start), end(t.end), file(t.sourcefile) {}
Location::Location(IR::ASTValue const &v): Location(v.ast) {}
Location::Location(IR::ASTValue const *v): Location(v->ast) {}
Location::Location(std::string::iterator start, std::string::iterator end, File const *file): start(start), end(end), file(file) {}

Location::Location(ASTNS::AST *ast): Location(ast->start().start, ast->end().end, &ast->file) {}
// Error methods {{{1
Error::Error(MsgType type, Location const &location, std::string message): type(type), location(location), message(message) {}
Error& Error::underline(Underline const &underline)
{
    underlines.push_back(underline);
    return *this;
}
// Underline message methods {{{1
Error::Underline::Underline(Location const &location, char ch): location(location), ch(ch) {}
Error::Underline& Error::Underline::error(std::string const &message)
{
    return addmsg("error", A_FG_RED, message);
}
Error::Underline& Error::Underline::warning(std::string const &message)
{
    return addmsg("warning", A_FG_MAGENTA, message);
}
Error::Underline& Error::Underline::note(std::string const &message)
{
    return addmsg("note", A_FG_GREEN, message);
}
Error::Underline& Error::Underline::help(std::string const &message)
{
    return addmsg("help", A_FG_CYAN, message);
}
Error::Underline& Error::Underline::hint(std::string const &message)
{
    return addmsg("hint", A_FG_YELLOW, message);
}
Error::Underline& Error::Underline::message(std::string const &type, std::string const &message)
{
    return addmsg(type, A_FG_WHITE A_BOLD, message);
}
Error::Underline& Error::Underline::addmsg(std::string const &type, char const * const color, std::string const &message)
{
    messages.push_back(Message {type, message, color});
    return *this;
}
// other internal errors {{{1
void reportAbortNoh(std::string const &message)
{
    std::cerr << "!!! Unrecoverable brokenness discovered in compiler !!!: " << message << std::endl;
    std::cerr << "!!! this is a bug - whether or not it has a bug report is unknown" << std::endl;
    std::cerr << "!!! bugs can be reported on the Katsel GitHub page: https://github.com/hpj2ltxry43b/katsel/issues" << std::endl;
    std::cerr << "!!! please search far and wide (on the GitHub page) before reporting a bug, so that there are no duplicate bug reports!" << std::endl;
    std::cerr << "Aborting..." << std::endl;
    std::abort();
}
void invalidTok(std::string const &name, Token const &underline)
{
    reportAbortNoh(format("invalid token for %: \"%\"", name, underline));
}
void calledWithOpTyNEthis(std::string const &classN, std::string const &fnn, std::string const &opname)
{
    reportAbortNoh(format("%::% called with % type != this", classN, fnn, opname));
}
void outOSwitchDDefaultLab(std::string const &fnn, Location const &highlight)
{
    reportAbortNoh(format("% went out of switch despite default label", fnn));
}
void fCalled(std::string const &fnn)
{
    reportAbortNoh(format("% called", fnn));
}
void outOSwitchNoh(std::string const &fnn)
{
    reportAbortNoh(format("% went out of switch", fnn));
}
