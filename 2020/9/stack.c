#include <stdlib.h>
#include "stack.h"

StackNode *push(StackNode *top, Data new_data)
{

    StackNode *temp = (StackNode *)malloc(sizeof(StackNode));
    temp->prev = top;
    temp->data = new_data;
    temp->isAddress = 0;
    top = temp;

    return top;
}

StackNode *pop(StackNode *top, Data *data)
{

    (*data) = top->data;
    StackNode *temp = top->prev;
    free(top);
    top = temp;

    return top;
}

Data getn(StackNode *top, long n)
{

    while (n > 0)
    {
        top = top->prev;
        n--;
    }

    return top->data;
}

int isAddressN(StackNode *top, int n)
{
    while (n > 0)
    {
        top = top->prev;
        n--;
    }

    return top->isAddress;
}

StackNode *swapn(StackNode *top, long n)
{
    StackNode *noden = top;
    Data datan;

    while (n > 0)
    {
        noden = noden->prev;
        n--;
    };

    datan = noden->data;
    noden->data = top->data;
    top->data = datan;

    return top;
}
