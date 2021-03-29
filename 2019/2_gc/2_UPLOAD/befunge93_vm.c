#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "stack.h"
#include "torus_help.h"
#include "instr_codes.h"

int op, opcode;
int heapsize = 0;

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

void check_refs(cons_cell **pointer)
{

    ((*pointer)->refs)--;

    if (((*pointer)->refs == 0))
    {
        if ((*pointer)->value_a.datatype == CONSCELL_POINTER)
            check_refs(&((*pointer)->value_a.data.pointer));
        if ((*pointer)->value_b.datatype == CONSCELL_POINTER)
            check_refs(&((*pointer)->value_b.data.pointer));
        free((*pointer));
        (*pointer) = NULL;
        heapsize--;
        // printf("after free heapsize is %d\n", heapsize);
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
    int stacksize = 0;

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
        &&_END,
        &&_CONS,
        &&_HEAD,
        &&_TAIL};

    //MAIN LOOP
    static int PCi = 0, PCj = 0; //pc
    enum direction PCdir = RIGHT_D;

    tdata *value1, *value2, *value3;

    value1 = (tdata *)malloc(sizeof(tdata));
    value2 = (tdata *)malloc(sizeof(tdata));
    value3 = (tdata *)malloc(sizeof(tdata));

    tdata tdata_arg;

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
        // printf("DIGIT\n");

        move(&PCi, &PCj, PCdir);
        tdata_arg.data.data = op - '0';
        tdata_arg.datatype = LONGINT;

        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case ADD:
    _ADD:
        // printf("ADD\n");

        move(&PCi, &PCj, PCdir);

        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        tdata_arg.datatype = LONGINT;
        tdata_arg.data.data = (*value1).data.data + (*value2).data.data;
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case SUB:
    _SUB:
        // printf("SUB\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        tdata_arg.datatype = LONGINT;
        tdata_arg.data.data = (*value1).data.data - (*value2).data.data;
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case MUL:
    _MUL:
        // printf("MUL\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        tdata_arg.datatype = LONGINT;
        tdata_arg.data.data = (*value1).data.data * (*value2).data.data;
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DIV:
    _DIV:
        // printf("DIV\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        tdata_arg.datatype = LONGINT;
        tdata_arg.data.data = (*value1).data.data / (*value2).data.data;
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case MOD:
    _MOD:
        // printf("MOD\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        tdata_arg.datatype = LONGINT;
        tdata_arg.data.data = (*value1).data.data % (*value2).data.data;
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case NOT:
    _NOT:
        // printf("NOT\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);

        if ((*value1).data.data == 0)
        {
            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = 1;
            top = push(top, tdata_arg, &stacksize);
        }
        else
        {
            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = 0;
            top = push(top, tdata_arg, &stacksize);
        }

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case GREATER:
    _GREATER:
        // printf("GREATER\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        if ((*value1).data.data > (*value2).data.data)
        {
            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = 1;
            top = push(top, tdata_arg, &stacksize);
        }
        else
        {
            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = 0;
            top = push(top, tdata_arg, &stacksize);
        }

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case RIGHT:
    _RIGHT:
        // printf("RIGHT\n");

        PCdir = RIGHT_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case LEFT:
    _LEFT:
        // printf("LEFT\n");

        PCdir = LEFT_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case UP:
    _UP:
        // printf("UP\n");

        PCdir = UP_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DOWN:
    _DOWN:
        // printf("DOWN\n");

        PCdir = DOWN_D;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case RANDOM:
    _RANDOM:
        // printf("RANDOM\n");

        PCdir = rand() % 4;
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case HORIZONTAL_IF:
    _HORIZONTAL_IF:
        // printf("HOR IF\n");

        top = pop(top, value1, &stacksize);

        if ((*value1).data.data == 0)
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
        // printf("VER IF\n");

        top = pop(top, value1, &stacksize);

        if ((*value1).data.data == 0)
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
        // printf("STR MODE\n");

        move(&PCi, &PCj, PCdir);
        while (torus[PCi][PCj] != '"')
        {
            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = torus[PCi][PCj];
            top = push(top, tdata_arg, &stacksize);
            move(&PCi, &PCj, PCdir);
        }
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case DUP:
    _DUP:
        // printf("DUP\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);

        if ((*value1).datatype == LONGINT)
        {
            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = (*value1).data.data;
            top = push(top, tdata_arg, &stacksize);

            tdata_arg.datatype = LONGINT;
            tdata_arg.data.data = (*value1).data.data;
            top = push(top, tdata_arg, &stacksize);
        }
        else
        {
            tdata_arg.datatype = CONSCELL_POINTER;
            tdata_arg.data.pointer = (*value1).data.pointer;
            top = push(top, tdata_arg, &stacksize);

            tdata_arg.datatype = CONSCELL_POINTER;
            tdata_arg.data.pointer = (*value1).data.pointer;
            top = push(top, tdata_arg, &stacksize);

            ((*value1).data.pointer->refs)++;
        }

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case SWAP:
    _SWAP:
        // printf("SWAP\n");

        move(&PCi, &PCj, PCdir);

        top = pop(top, value2, &stacksize);
        top = pop(top, value1, &stacksize);

        tdata_arg = (*value2);
        top = push(top, tdata_arg, &stacksize);

        tdata_arg = (*value1);
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case POP:
    _POP:
        // printf("POP\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);

        if ((*value1).datatype == CONSCELL_POINTER)
            check_refs(&((*value1).data.pointer));

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case OUTPUT_INT:
    _OUTPUT_INT:
        // printf("OUT INT\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);
        printf("%ld ", (*value1).data.data);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case OUTPUT_CHAR:
    _OUTPUT_CHAR:
        // printf("OUT CHAR\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);
        printf("%c", (*value1).data.data);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case BRIDGE:
    _BRIDGE:
        // printf("BRIDGE\n");

        move(&PCi, &PCj, PCdir);
        move(&PCi, &PCj, PCdir);
        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case GET:
    _GET:
        // printf("GET\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize); //y
        top = pop(top, value2, &stacksize); //x

        tdata_arg.datatype = LONGINT;
        tdata_arg.data.data = torus[(*value1).data.data][(*value2).data.data];
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case PUT:
    _PUT:
        // printf("PUT\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize); //y
        top = pop(top, value2, &stacksize); //x
        top = pop(top, value3, &stacksize); //value
        if ((*value3).datatype == LONGINT)
        {
            torus[(*value1).data.data][(*value2).data.data] = (*value3).data.data;
            // printf("PUTTING %ld\n", (*value3).data.data);
            // printStack(top);
        }

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case INPUT_INT:
    _INPUT_INT:
        // printf("IN INT\n");

        move(&PCi, &PCj, PCdir);
        (*value2).datatype = LONGINT;
        scanf("%ld", &((*value2).data.data));

        tdata_arg = *value2;
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case INPUT_CHAR:
    _INPUT_CHAR:
        // printf("IN CHAR\n");

        move(&PCi, &PCj, PCdir);
        (*value2).datatype = LONGINT;
        scanf("%c", &((*value2).data.data));

        tdata_arg = (*value1);
        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case END:
    _END:
        break;
    case CONS:
    _CONS:
        // printf("CONS\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize); //b
        top = pop(top, value2, &stacksize); //a

        tdata_arg.datatype = CONSCELL_POINTER;
        if (++heapsize > max_mallocs)
        {
            printf("Heap Is Full\n");
            exit(-1);
        }

        // printf("heapsize after new is %d\n", heapsize);
        tdata_arg.data.pointer = (cons_cell *)malloc(sizeof(cons_cell));
        tdata_arg.data.pointer->refs = 1;

        tdata_arg.data.pointer->value_a = *value2;
        tdata_arg.data.pointer->value_b = *value1;

        top = push(top, tdata_arg, &stacksize);

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    case HEAD:
    _HEAD:
        // printf("HEAD\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);

        if ((*value1).data.pointer->value_a.datatype == CONSCELL_POINTER)
        {
            (*value1).data.pointer->value_a.data.pointer->refs++;
        }

        tdata_arg = (*value1).data.pointer->value_a;

        top = push(top, tdata_arg, &stacksize);

        check_refs(&((*value1).data.pointer));

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;

    case TAIL:
    _TAIL:
        // printf("TAIL\n");

        move(&PCi, &PCj, PCdir);
        top = pop(top, value1, &stacksize);

        if ((*value1).data.pointer->value_b.datatype == CONSCELL_POINTER)
        {
            (*value1).data.pointer->value_b.data.pointer->refs++;
        }

        tdata_arg = (*value1).data.pointer->value_b;
        top = push(top, tdata_arg, &stacksize);
        check_refs(&((*value1).data.pointer));

        op = torus[PCi][PCj];
        opcode = findInstructionCode(op);
        NEXT_INSTRUCTION;
    }

    //printStack(top);
    // printNodeSize();
    exit(0);
}
