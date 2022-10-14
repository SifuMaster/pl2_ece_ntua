
typedef struct ConsCell ConsCell;

typedef union Data
{
    long number;
    ConsCell *conscell;

} Data;

typedef struct ConsCell
{
    int references;
    int isHeadAddr;
    Data head;
    int isTailAddr;
    Data tail;
} ConsCell;

typedef struct StackNode
{
    Data data;
    struct StackNode *prev;
    int isAddress;
} StackNode;

StackNode *push(StackNode *top, Data new_data);

StackNode *pop(StackNode *top, Data *data);

Data getn(StackNode *top, long n);

StackNode *swapn(StackNode *top, long n);

int isAddressN(StackNode *top, int n);