#include "instr_codes.h"
#include <stdio.h>
#include <stdlib.h>

int findInstructionCode(char instr)
{
    if (instr == ' ')
        return BLANK;
    else if (instr >= '0' && instr <= '9')
        return DIGIT;
    else if (instr == '+')
        return ADD;
    else if (instr == '-')
        return SUB;
    else if (instr == '*')
        return MUL;
    else if (instr == '/')
        return DIV;
    else if (instr == '%')
        return MOD;
    else if (instr == '!')
        return NOT;
    else if (instr == '`')
        return GREATER;
    else if (instr == '>')
        return RIGHT;
    else if (instr == '<')
        return LEFT;
    else if (instr == '^')
        return UP;
    else if (instr == 'v')
        return DOWN;
    else if (instr == '?')
        return RANDOM;
    else if (instr == '_')
        return HORIZONTAL_IF;
    else if (instr == '|')
        return VERTICAL_IF;
    else if (instr == '"')
        return STRINGMODE;
    else if (instr == ':')
        return DUP;
    else if (instr == '\\')
        return SWAP;
    else if (instr == '$')
        return POP;
    else if (instr == '.')
        return OUTPUT_INT;
    else if (instr == ',')
        return OUTPUT_CHAR;
    else if (instr == '#')
        return BRIDGE;
    else if (instr == 'g')
        return GET;
    else if (instr == 'p')
        return PUT;
    else if (instr == '&')
        return INPUT_INT;
    else if (instr == '~')
        return INPUT_CHAR;
    else if (instr == '@')
        return END;
    else
    {
        printf("SYNTAX ERROR: unknown command (%c)\n", instr);
        exit(-1);
    }
}
