#include <stdlib.h>
#include <stdio.h>
#include "token.h"

int get_token()
{
    int c;

        // Skip over spaces, tabs, and the DOS carriage return
    while ((c = getchar()) == ' ' || c == '\t' || c == '\r');
    if (c == '#')  // If the start of a comment
        while ((c = getchar()) != EOF && c != '\n'); // ignore rest of line
    return c;
}

void put_token(int token)
{
    putchar(token);
}
