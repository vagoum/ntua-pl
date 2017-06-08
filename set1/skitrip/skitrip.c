//modified for equality

#include <stdio.h>
#include <stdlib.h>

#define BSIZE 1<<15

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

char buffer[BSIZE];
long bpos = 0L, bsize = 0L;

int readInt()
{
    int d = 0L, x = 0L;
    char c;

    while (1)  {
        if (bpos >= bsize) {
            bpos = 0;
            if (feof(stdin)) return x;
            bsize = fread(buffer, 1, BSIZE, stdin);
        }
        c = buffer[bpos++];
        if (c >= '0' && c <= '9') { x = x*10 + (c-'0'); d = 1; }
        else if (d == 1) return x;
    }
    return -1;
}

int readFromIntFile(FILE *file)
{
    int d = 0L, x = 0L;
    char c;

    while (1)  {
        if (bpos >= bsize) {
            bpos = 0;
            if (feof(stdin)) return x;
            bsize = fread(buffer, 1, BSIZE, file);
        }
        c = buffer[bpos++];
        if (c >= '0' && c <= '9') { x = x*10 + (c-'0'); d = 1; }
        else if (d == 1) return x;
    }
    return -1;
}

int main(int argc, const char * argv[])
{
    int size, i, j, res;

    /************ Read file ***************/
    const char *fileName = argv[1];
    FILE *file = fopen(fileName, "r");

    fscanf(file, "%d", &size);
    int *arr = (int *)  malloc(sizeof(int)* size);

    for (i = 0; i < size; i++)
        fscanf(file, "%d", &arr[i]);

    fclose(file);
    /************ Read file ***************/

    /*********** Standard Input ************
    scanf("%d", &size);
    int *arr = (int *)  malloc(sizeof(int)* size);

    for (i = 0; i < size; i++)
        scanf("%d", &arr[i]);

    *******  Standard Input ************/

    int *minArr = (int *)malloc(sizeof(int) * size);

    minArr[0] = arr[0];
    for (i = 1; i < size; ++i)
        minArr[i] = MIN(arr[i], minArr[i-1]);

    int *maxArr = (int *)malloc(sizeof(int) * size);
    maxArr[size - 1] = arr[size - 1];
    for (j = size - 2; j >= 0; --j)
        maxArr[j] = MAX(arr[j], maxArr[j+1]);


    i = 0, j = 0, res = -1;
    while (j < size && i < size) {
        if (minArr[i] <= maxArr[j]) {
            res = MAX(res, j-i);
            j = j + 1;
        }
        else
            i = i + 1;
    }
    res = MAX(res, 0);
    printf("%d\n", res);

    return 0;
}
