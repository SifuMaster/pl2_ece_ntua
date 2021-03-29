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

Node *push(Node *old_top, long int new_data)
{
    Node *new_top;

    new_top = (Node *)malloc(sizeof(Node));
    new_top->data = new_data;
    new_top->prev = old_top;

    return new_top;
};

Node *pop(Node *old_top, long int *data)
{
    Node *new_top;

    if (isEmpty(old_top))
    {
        new_top = NULL;
        *data = 0;
    }
    else
    {
        *data = old_top->data;
        new_top = old_top->prev;
        free(old_top);
    }

    return new_top;
};

void printStack(Node *top)
{
    printf("\n----------STACK----------\n");
    while (top != NULL)
    {
        printf("%ld\n", top->data);
        top = top->prev;
    }
    printf("----------STACK----------\n");
};
