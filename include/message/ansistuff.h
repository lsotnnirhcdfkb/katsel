#pragma once

#define A_RESET        "\033[0m"
#define A_BOLD         "\033[1m"
#define A_DIM          "\033[2m"
#define A_FG_BLACK     "\033[30m"
#define A_FG_RED       "\033[31m"
#define A_FG_GREEN     "\033[32m"
#define A_FG_YELLOW    "\033[33m"
#define A_FG_BLUE      "\033[34m"
#define A_FG_MAGENTA   "\033[35m"
#define A_FG_CYAN      "\033[36m"
#define A_FG_WHITE     "\033[37m"
#define A_BG_BLACK     "\033[40m"
#define A_BG_RED       "\033[41m"
#define A_BG_GREEN     "\033[42m"
#define A_BG_YELLOW    "\033[43m"
#define A_BG_BLUE      "\033[44m"
#define A_BG_MAGENTA   "\033[45m"
#define A_BG_CYAN      "\033[46m"
#define A_BG_WHITE     "\033[47m"

void enableTerminalCodes();
void resetTerminal();

bool ansiCodesEnabled();
