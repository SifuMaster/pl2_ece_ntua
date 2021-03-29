#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "stack.h"
#include "torus_help.h"
#include "instr_codes.h"

int op, opcode;

#define NEXT_INSTRUCTION \
    goto *(void *)(label_tab[opcode])

enum direction
{
    RIGHT_D,
    DOWN_D,
    LEFT_D,
    UP_D
};

void move(int *row, int *col, enum direction dir)
{
    switch (dir)
    {
    case RIGHT_D:
        if ((*col) == COLUMNS - 1)
            (*col) = 0;
        else
            (*col)++;
        break;
    case DOWN_D:
        if ((*row) == ROWS - 1)
            (*row) = 0;
        else
            (*row)++;
        break;
    case LEFT_D:
        if ((*col) == 0)
            (*col) = COLUMNS - 1;
        else
            (*col)--;
        break;
    case UP_D:
        if ((*row) == 0)
            (*row) = ROWS - 1;
        else
            (*row)--;
    }
}

int main(int argc, char **argv)
{

    if (argc != 2)
    {
        printf("ERROR: Must define exactly one argument\n");
        exit(-1);
    }

    char torus[ROWS][COLUMNS];

    Node *top = NULL;

    //READING THE INPUT FILE AND CREATING THE TORUS FILLED WITH BLANKS
    FILE *program;
    char c;
    program = fopen(argv[1], "r");

    int i = 0;
    int j = 0;

    c = getc(program);

    while (c != EOF)
    {
        if (c == '\n')
        {
            for (j; j < COLUMNS; j++)
            {
                torus[i][j] = ' ';
            }

            i++;
            if (i > ROWS)
            {
                printf("ERROR: program has more rows than %d\n", ROWS);
                exit(-1);
            }
            j = 0;
        }
        else if (c == '\0')
        {
            printf("ERROR: program has encountered NULL\n");
            exit(-1);
        }
        else
        {
            torus[i][j] = c;
            j++;
            
            if (j > COLUMNS)
            {
                printf("ERROR: program has more columns than %d\n", COLUMNS);
                exit(-1);
            }
        }
        c = getc(program);
    }
    if (j < COLUMNS - 1)
    {
        for (j; j < COLUMNS; j++)
        {
            torus[i][j] = ' ';
        }
    }
    if (i < ROWS - 1)
    {
        for (i + 1; i < ROWS; i++)
        {
            for (j = 0; j < COLUMNS; j++)
            {
                torus[i][j] = ' ';
            }
        }
    }
    fclose(program);

    char test = torus[0][0];

    //DEFINING LABELS
    static void *label_tab[] = {
        &&_BLANK,
        &&_DIGIT,
        &&_ADD,
        &&_SUB,
        &&_MUL,
        &&_DIV,
        &&_MOD,
        &&_NOT,
        &&_GREATER,
        &&_RIGHT,
        &&_LEFT,
        &&_UP,
        &&_DOWN,
        &&_RANDOM,
        &&_HORIZONTAL_IF,
        &&_VERTICAL_IF,
        &&_STRINGMODE,
        &&_DUP,
        &&_SWAP,
        &&_POP,
        &&_OUTPUT_INT,
        &&_OUTPUT_CHAR,
        &&_BRIDGE,
        &&_GET,
        &&_PUT,
        &&_INPUT_INT,
        &&_INPUT_CHAR,
        &&_END};

    //MAIN LOOP
    static int PCi = 0, PCj = 0; //pc
    enum direction PCdir = RIGHT_D;

    long int *value1, *value2, *value3;

    value1 = (long int *)malloc(sizeof(long int));
    value2 = (long int *)malloc(sizeof(long int));
    value3 = (long int *)malloc(sizeof(long int));

    srand(time(NULL));

    op = torus[PCi][PCj];
    opcode = findInstructionCode(op);

    switch (opcode)
    {
    case BLANK:
    _BLANK:
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DIGIT:
    _DIGIT:
        move(&PCi, &PCj, PCdir);
        top = push(top, op - '0');
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case ADD:
    _ADD:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        top = push(top, (*value1) + (*value2));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case SUB:
    _SUB:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        top = push(top, (*value1) - (*value2));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case MUL:
    _MUL:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        top = push(top, (*value1) * (*value2));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DIV:
    _DIV:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        top = push(top, (*value1) / (*value2));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case MOD:
    _MOD:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        top = push(top, (*value1) % (*value2));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case NOT:
    _NOT:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1);
        if ((*value1) == 0)
            top = push(top, 1);
        else
            top = push(top, 0);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case GREATER:
    _GREATER:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        if ((*value1) > (*value2))
            top = push(top, 1);
        else
            top = push(top, 0);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case RIGHT:
    _RIGHT:
        PCdir = RIGHT_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case LEFT:
    _LEFT:
        PCdir = LEFT_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case UP:
    _UP:
        PCdir = UP_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DOWN:
    _DOWN:
        PCdir = DOWN_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case RANDOM:
    _RANDOM:
        PCdir = rand() % 4;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case HORIZONTAL_IF:
    _HORIZONTAL_IF:
        top = pop(top, value1);
        if ((*value1) == 0)
        {
            PCdir = RIGHT_D;
            move(&PCi, &PCj, PCdir);
        }
        else
        {
            PCdir = LEFT_D;
            move(&PCi, &PCj, PCdir);
        }

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case VERTICAL_IF:
    _VERTICAL_IF:
        top = pop(top, value1);
        if ((*value1) == 0)
        {
            PCdir = DOWN_D;
            move(&PCi, &PCj, PCdir);
        }
        else
        {
            PCdir = UP_D;
            move(&PCi, &PCj, PCdir);
        }

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case STRINGMODE:
    _STRINGMODE:
        move(&PCi, &PCj, PCdir);
        while (torus[PCi][PCj] != '"')
        {
            top = push(top, torus[PCi][PCj]);
            move(&PCi, &PCj, PCdir);
        }
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DUP:
    _DUP:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1);
        top = push(top, (*value1));
        top = push(top, (*value1));

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case SWAP:
    _SWAP:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value2);
        top = pop(top, value1);
        top = push(top, (*value2));
        top = push(top, (*value1));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case POP:
    _POP:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case OUTPUT_INT:
    _OUTPUT_INT:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1);
        printf("%ld ", (*value1));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case OUTPUT_CHAR:
    _OUTPUT_CHAR:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1);
        printf("%c", (*value1));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case BRIDGE:
    _BRIDGE:
        move(&PCi, &PCj, PCdir);
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case GET:
    _GET:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1); //y
        top = pop(top, value2); //x
        top = push(top, torus[(*value1)][(*value2)]);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case PUT:
    _PUT:
        move(&PCi, &PCj, PCdir);
        top = pop(top, value1); //y
        top = pop(top, value2); //x
        top = pop(top, value3); //value
        torus[(*value1)][(*value2)] = (*value3);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case INPUT_INT:
    _INPUT_INT:
        move(&PCi, &PCj, PCdir);
        (*value2) = scanf("%ld", value1);
        top = push(top, (*value1));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case INPUT_CHAR:
    _INPUT_CHAR:
        move(&PCi, &PCj, PCdir);
        (*value2) = scanf("%c", value1);
        top = push(top, (*value1));
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case END:
    _END:
        break;
    }
    exit(0);
}