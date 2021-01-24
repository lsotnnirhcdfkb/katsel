#ifdef _WIN32
#include <windows.h>
#endif

#include "message/ansistuff.h"

#include <iostream>

// i know that global variables are evil
// but this one is static and not extern and theres a function to get it
// so i think itll be fine but we will see once i compile it
static bool ansi_codes = false;

#ifdef _WIN32

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING  0x0004
#endif

static HANDLE h_out;
static DWORD original_out_mode;

void enable_terminal_codes() {
    // get handle to stdout
    h_out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h_out == INVALID_HANDLE_VALUE) // if the handle is invalid {
        ansi_codes = false;
        return;
    }

    original_out_mode = 0;

    if (!GetConsoleMode(h_out, &original_out_mode)) // if cant get the original output mode {
        ansi_codes = false;
        return;
    }

    DWORD requested_out_mode = original_out_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;

    if (!SetConsoleMode(h_out, requested_out_mode)) // if cant set output mode {
        ansi_codes = false;
        return;
    }

    ansi_codes = true; // yay could get handle, get and set output mode
}

void reset_terminal() {
    if (!SetConsoleMode(h_out, original_out_mode)) {
        // can't set the terminal back to normal, so it has to deal with that now i guess
        return;
    }

    ansi_codes = false;
}
#else
void enable_terminal_codes() {
    ansi_codes = true;
}

void reset_terminal() {
    ansi_codes = true;
}
#endif

bool ansi_codes_enabled() {
    return ansi_codes;
}
