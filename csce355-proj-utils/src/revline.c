/* Program to output the tokens on a line in reverse order
 * 
 * Author: Stephen A. Fenner, March 2021
 */

#include <stdlib.h>
#include <stdio.h>
#include "token.h"

// We use a global stack, implemented as a simple linked list of tokens

typedef struct sn 
{
    int token;
    struct sn *next;
} STACK_NODE, *SN;

void push(int c);
int pop(); // Returns '\0' if called with an empty stack

// Reads a single line from standard input and pushes the nonblank
// characters onto the global stack.
// The ending newline is read but not pushed.
void read_stack();
// Writes the characters on the global stack to standard output
void dump_stack();

//The global stack
SN the_stack = NULL;

// Linked list for garbage collection of stack nodes
SN free_list = NULL;

int main()
{
    int c;
    while ((c = getchar()) != EOF) {
        ungetc(c, stdin);
        read_stack();
        dump_stack();
        put_token('\n');
        fflush(stdout);
    }
    return 0;
}

// Push a character onto the global stack
void push(int token)
{
    SN new_stack_node;
    if (free_list != NULL) {
        new_stack_node = free_list;
        free_list = free_list->next;
    }
    else {
        new_stack_node = (SN) malloc(sizeof(STACK_NODE));
        if (new_stack_node == NULL) {
            fprintf(stderr, "\nrevline: Fatal error: out of memory\n");
            exit(1);
        }
    }
    new_stack_node->token = token;
    new_stack_node->next = the_stack;
    the_stack = new_stack_node;
}

// Pop a character off of the global stack and return it.
// Return '\0' if the stack is empty.
int pop()
{
    SN old;
    
    if (the_stack == NULL)
        return '\0';
    old = the_stack;
    the_stack = old->next;
    old->next = free_list;
    free_list = old;
    return old->token;
}

// Reads a single line from standard input and pushes the nonblank
// characters onto the global stack.
// The ending newline is read but not pushed.
void read_stack()
{
    int token;

    while ((token = get_token()) != '\n' && token != EOF)
        push(token);
}

// Outputs the global stack to standard output, swapping opening and
// closing parentheses
void dump_stack()
{
    int token;
    
    while (the_stack != NULL) {
        token = pop();
        put_token(token);
    }
}

