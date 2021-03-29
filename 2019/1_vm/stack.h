typedef struct Node {
    long int data;
    struct Node* prev;
}Node;

long int isEmpty(Node* top);

Node* push(Node* old_top, long int new_data);

Node *pop(Node *old_top, long int *data);

void printStack(Node* top);
