#define stack_max 1048576   //2^20
#define max_mallocs 8388608 //2^24 /2 cuz every cons_cell contains 2 words

enum datatype
{
    LONGINT,
    CONSCELL_POINTER
};

typedef struct cons_cell cons_cell;

typedef union data {
    long int data;
    cons_cell *pointer;
} data;

typedef struct tdata
{
    enum datatype datatype;
    data data;

} tdata;

typedef struct cons_cell
{
    int refs;
    tdata value_a;
    tdata value_b;

} cons_cell;

typedef struct Node
{
    tdata data;
    struct Node *prev;
} Node;

long int isEmpty(Node *top);

Node *push(Node *old_top, tdata new_data, int *stacksize);

Node *pop(Node *old_top, tdata *data, int *stacksize);

void printStack(Node *top);

void printNodeSize();

void checkStackSize(int stacksize);