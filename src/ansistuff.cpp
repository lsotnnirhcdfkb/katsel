#ifdef _WIN32
#include <windows.h>
#endif

#include "ansistuff.h"

// i know that global variables are evil
// but this one is static and not extern and theres a function to get it
// so i think itll be fine but we will see once i compile it
static bool ansiCodes = false;

#ifdef _WIN32

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING  0x0004
#endif

static HANDLE hOut;
static DWORD originalOutMode;

void enableTerminalCodes()
{
    // get handle to stdout
    hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut == INVALID_HANDLE_VALUE) // if the handle is invalid
    {
        ansiCodes = false;
        return;
    }

    originalOutMode = 0;

    if (!GetConsoleMode(hOut, &originalOutMode)) // if cant get the original output mode
    {
        ansiCodes = false;
        return;
    }

    DWORD requestedOutMode = originalOutMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;

    if (!SetConsoleMode(hOut, requestedOutMode)) // if cant set output mode
    {
        ansiCodes = false;
        return;
    }

    ansiCodes = true; // yay could get handle, get and set output mode
}

void resetTerminal()
{
    std::cout << "\033[0m";

    if (!SetConsoleMode(hOut, originalOutMode))
    {
        // can't set the terminal back to normal, so it has to deal with that now i guess
        return;
    }

    ansiCodes = false;
}
#else
void enableTerminalCodes()
{
    ansiCodes = true;
}

void resetTerminal()
{
    std::cout << "\033[0m";
    ansiCodes = true;
}
#endif

bool ansiCodesEnabled()
{
    return ansiCodes;
}
