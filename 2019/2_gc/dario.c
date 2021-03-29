#include <stdio.h>
#include <stdlib.h>

int mkd(int a, int b)
{
    int c;
    int mk;
    if (a >= b)
    {

        while (b != 0)
        {

            c = a % b;
            a = b;
            b = c;
        }

        mk = a;
    }

    else
    {
        while (a != 0)
        {

            c = b % a;
            b = a;
            a = c;
        }

        mk = b;
    }
    return mk;
}

int main(int argc, char const *argv[])
{
    int a, b;
    a=150;
    b = 50;

    printf("%d \n", mkd(a, b));
    printf("ASDASDASDAS");
    return 0;
}
