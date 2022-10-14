#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include "stack.h"

#define MAX_HEAP_SIZE 30000000
ConsCell Heap[MAX_HEAP_SIZE];

#define MAX_PROGRAM_SIZE 65536 // +1 so the program terminates after the last command
int input[MAX_PROGRAM_SIZE] = {0}, *pc;

int find_offset(int pc)
{
    switch (pc)
    {
    case 0x00:
        return 0;
    case 0x01:
        return 1;
    case 0x02:
        return 2;
    case 0x03:
        return 3;
    case 0x04:
        return 4;
    case 0x05:
        return 5;
    case 0x06:
        return 6;
    case 0x07:
        return 7;
    case 0x08:
        return 8;
    case 0x09:
        return 9;
    case 0x0a:
        return 10;
    case 0x0b:
        return 11;
    case 0x0c:
        return 12;
    case 0x0d:
        return 13;
    case 0x0e:
        return 14;
    case 0x0f:
        return 15;
    case 0x10:
        return 16;
    case 0x11:
        return 17;
    case 0x12:
        return 18;
    case 0x13:
        return 19;
    case 0x14:
        return 20;
    case 0x15:
        return 21;
    case 0x16:
        return 22;
    case 0x17:
        return 23;
    case 0x18:
        return 24;
    case 0x2a:
        return 25;
    case 0x30:
        return 26;
    case 0x31:
        return 27;
    case 0x32:
        return 28;
    }
}

#define NEXT_INSTRUCTION goto *(void *)(label_tab[find_offset(*pc)])

long collect_garbage(ConsCell *Heap)
{
    long i = 0, shift = 0;
    while (i < MAX_HEAP_SIZE)
    {
        if (Heap[i].references == 0)
            shift++;
        else
            Heap[i - shift] = Heap[i];
        i++;
    }
    return shift;
}

int main(int argc, char **argv)
{
    clock_t start_time = clock();
    long i, number, heap_size = 0;
    int byte;
    Data data, a, b;
    StackNode *top = NULL, temp_node;

    static void *label_tab[] = {
        &&halt_label,
        &&jump_label,
        &&jnz_label,
        &&dup_label,
        &&swap_label,
        &&drop_label,
        &&push4_label,
        &&push2_label,
        &&push1_label,
        &&add_label,
        &&sub_label,
        &&mul_label,
        &&div_label,
        &&mod_label,
        &&eq_label,
        &&ne_label,
        &&lt_label,
        &&gt_label,
        &&le_label,
        &&ge_label,
        &&not_label,
        &&and_label,
        &&or_label,
        &&input_label,
        &&output_label,
        &&clock_label,
        &&cons_label,
        &&hd_label,
        &&tl_label,
    };

    if (argc != 2)
    {
        printf("ERROR: Must give exactly one file name\n");
        exit(-1);
    }

    FILE *file;
    file = fopen(argv[1], "r");

    i = 0;
    byte = getc(file);
    while (byte != EOF)
    {
        input[i++] = byte;
        byte = getc(file);
    };
    fclose(file);

    pc = input;
    NEXT_INSTRUCTION;

halt_label:
    return 0;
jump_label:
    number = *(++pc);
    number += *(++pc) << 8;
    pc = input + number;
    NEXT_INSTRUCTION;
jnz_label:
    top = pop(top, &data);
    if (0 != (data.number))
    {
        number = *(++pc);
        number += *(++pc) << 8;
        pc = input + number;
    }
    else
        pc += 3;

    NEXT_INSTRUCTION;
dup_label:
    data = getn(top, *(++pc));
    if (isAddressN(top, *pc))

        i = 1;
    else
        i = 0;
    top = push(top, data);
    top->isAddress = i;
    if (i)
        top->data.conscell->references++;
    pc++;
    NEXT_INSTRUCTION;
swap_label:
    top = swapn(top, *(++pc));
    pc++;
    NEXT_INSTRUCTION;
drop_label:
    if (top->isAddress)
        top->data.conscell->references--;
    top = pop(top, &data);
    pc++;
    NEXT_INSTRUCTION;
push4_label:
    number = *(++pc);
    number += ((*(++pc)) << 8);
    number += ((*(++pc)) << 16);
    number += ((*(++pc)) << 24);
    if ((long)pow(2, 31) & number)
        number = (~number) + 1;
    data.number = number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
push2_label:
    number = *(++pc);
    number += ((*(++pc)) << 8);
    if ((long)pow(2, 15) & number)
        number = (~number) + 1;
    data.number = number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
push1_label:
    number = *(++pc);
    if ((long)pow(2, 7) & number)
        number = (~number) + 1;
    data.number = number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
add_label:
    top = pop(top, &b);
    top = pop(top, &a);
    data.number = a.number + b.number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
sub_label:
    top = pop(top, &b);
    top = pop(top, &a);
    data.number = a.number - b.number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
mul_label:
    top = pop(top, &b);
    top = pop(top, &a);
    data.number = a.number * b.number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
div_label:
    top = pop(top, &b);
    top = pop(top, &a);
    data.number = a.number / b.number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
mod_label:
    top = pop(top, &b);
    top = pop(top, &a);
    data.number = a.number % b.number;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
eq_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if (a.number == b.number)
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
ne_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if (a.number != b.number)
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
lt_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if (a.number < b.number)
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
gt_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if (a.number > b.number)
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
le_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if (a.number <= b.number)
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
ge_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if (a.number >= b.number)
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
not_label:
    top = pop(top, &data);
    if (data.number)
        data.number = 0;
    else
        data.number = 1;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
and_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if ((a.number != 0) && (b.number != 0))
        data.number = 1;
    else
        data.number = 0;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
or_label:
    top = pop(top, &b);
    top = pop(top, &a);
    if ((a.number == 0) && (b.number == 0))
        data.number = 0;
    else
        data.number = 1;
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
input_label:
    data.number = getchar();
    top = push(top, data);
    pc++;
    NEXT_INSTRUCTION;
output_label:
    top = pop(top, &data);
    putchar(data.number);
    pc++;
    NEXT_INSTRUCTION;
clock_label:
    printf("%0.6lf\n", (float)(clock() - start_time) / CLOCKS_PER_SEC);
    pc++;
    NEXT_INSTRUCTION;
cons_label:
    i = isAddressN(top, 0);
    top = pop(top, &a);
    Heap[heap_size].isTailAddr = i;
    Heap[heap_size].tail = a;

    i = isAddressN(top, 0);
    top = pop(top, &b);
    Heap[heap_size].isHeadAddr = i;
    Heap[heap_size].head = b;

    Heap[heap_size].references = 1;
    data.conscell = &(Heap[heap_size]);
    top = push(top, data);
    top->isAddress = 1;
    heap_size++;
    if (heap_size > MAX_HEAP_SIZE)
        heap_size = collect_garbage(Heap);
    pc++;
    NEXT_INSTRUCTION;
hd_label:
    top = pop(top, &data);
    data.conscell->references--;
    i = data.conscell->isHeadAddr;
    data = data.conscell->head;
    top = push(top, data);
    top->isAddress = i;
    pc++;
    NEXT_INSTRUCTION;
tl_label:
    top = pop(top, &data);
    data.conscell->references--;
    i = data.conscell->isTailAddr;
    data = data.conscell->tail;
    top = push(top, data);
    top->isAddress = i;
    pc++;
    NEXT_INSTRUCTION;
    return 0;
};
