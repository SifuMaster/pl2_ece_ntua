#include "stack.h"
#include <stdlib.h>
#include <stdio.h>

long int isEmpty(Node *top)
{
    if (top == NULL)
        return 1;
    else
        return 0;
};

Node *push(Node *old_top, tdata new_data, int *stacksize)
{
    // printf("PUSHING!! %ld\n", new_data.data.data);
    checkStackSize(++(*stacksize));
    Node *new_top;

    new_top = (Node *)malloc(sizeof(Node));

    if (new_data.datatype == LONGINT)
    {
        new_top->data.datatype = LONGINT;
        new_top->data.data.data = new_data.data.data;
    }
    else
    {
        new_top->data.datatype = CONSCELL_POINTER;
        new_top->data.data.pointer = new_data.data.pointer;
    }
    new_top->prev = old_top;

    return new_top;
};

Node *pop(Node *old_top, tdata *data, int *stacksize)
{
    (*stacksize)--;
    Node *new_top;

    if (isEmpty(old_top))
    {
        new_top = NULL;
        (*data).datatype = LONGINT;
        (*data).data.data = 0;
    }
    else
    {
        (*data) = old_top->data;
        new_top = old_top->prev;
        free(old_top);
        old_top = NULL;
    }

    return new_top;
};

void printStack(Node *top)
{
    printf("\n----------STACK----------\n");
    while (top != NULL)
    {
        printf("%ld\n", top->data.data.data);
        top = top->prev;
    }
    printf("----------STACK----------\n");
};

void printNodeSize()
{
    printf("Size of Node is %ld", sizeof(Node));
}

void checkStackSize(int stacksize)
{
    if (stacksize > stack_max)
    {
        printf("Stack Overflow!\n");
        exit(-1);
    }
    else
        return;
}
