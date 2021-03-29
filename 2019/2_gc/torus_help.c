#include "torus_help.h"
#include <stdio.h>

void printTorus(char torus[ROWS][COLUMNS]){
    for(int i=0; i<ROWS; i++)
    {
        for(int j=0; j<COLUMNS; j++)
        {
            printf("%c", torus[i][j]);
        }
        printf("\n");
    }
};
