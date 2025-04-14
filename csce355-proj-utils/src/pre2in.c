/* Program to convert regexes from prefix to infix,
 * with minimal parenthesis insertion based on the associativity
 * of union and concatenation.
 *
 * Reads a series of regexes in postfix notation, one per line,
 * from standard input and writes the corresponding infix regexes,
 * one per line, to standard output.
 * Spaces and tabs in the input regex are ignored.
 * Quits normally on EOF (e.g., typing Ctrl-D on an empty line)
 * Quits on the first syntax error with a message to standard error.
 *
 * Author: Stephen A. Fenner, March 2021 (fenner@cse.sc.edu)
 */

/* Recognized postfix regex constructors:
 *   Nullary "operators" (atoms):
 *      '/' : empty set
 *      [0-9a-z] (digits and lowercase characters) : 1-char atoms
 *   Unary operator:
 *      '*' : Kleene *-operator
 *   Binary operators:
 *      '+' : union
 *      '.' : concatenation
 */

/* Order of infix precedence (lowest to highest -- for inserting parentheses):
 *    + (precedence == 0, associative)
 *    . (precedence == 1, associative; does not appear in infix regex)
 *    * (precedence == 2, unary postfix)
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "token.h"

#define OPEN_PAREN '('
#define CLOSE_PAREN ')'

void parse_prefix();
void parse_prefix_with_prec(int prec);

int lookahead;

int main(int argc, char *argv[]) // Command line args ignored for now
{
        // Read postfix regexes from standard input, one per line,
        // and parse them.
    int c;

    while ((c = getchar()) != EOF) {
        ungetc(c, stdin);
        parse_prefix();
        fflush(stdout);
    }
    return 0;
}

// This function is a wrapper function for parse_prefix_with_prec(), which
// does all the work.
void parse_prefix()
{
    lookahead = get_token();
    if (lookahead == '\n')
        return;

    parse_prefix_with_prec(0);
    put_token('\n');
    
        // The whole line should be consumed by parsing, else error
    if (lookahead != '\n') {
        if (lookahead == EOF)
            fprintf(stderr, "\npre2in: Syntax error: EOF encountered prematurely\n");
        else
            fprintf(stderr, "pre2in: Syntax error: extra tokens on the line\n");
        exit(1);
    }
}

// Parse a prefix regex from standard input (recursive).
// Output corresponding infix regex to standard output.
//    prec is the precedence level of the closest surrounding operator
//    (or 0 if none).
// Precondition: lookahead is the first token of the regex
// Postcondition: lookahead is the first token after the regex
void parse_prefix_with_prec(int prec)
{
    if (lookahead == '/' || isdigit(lookahead) || islower(lookahead)) {
            // atomic regex
        put_token(lookahead);
        lookahead = get_token();
        return;
    }
    if (lookahead == '*') {   // Unary *-operator
        lookahead = get_token();
        parse_prefix_with_prec(2);         // Parse single operand
        put_token('*');
        return;
    }
    if (lookahead == '.') {   // Binary .-operator
        lookahead = get_token();
            // Add paren if necessary
        if (prec > 1) put_token(OPEN_PAREN);
        parse_prefix_with_prec(1);         // Parse right operand
            // No explicit infix operator used for concat (just juxtaposition)
        parse_prefix_with_prec(1);         // Parse left operand
            // Add paren if necessary
        if (prec > 1) put_token(CLOSE_PAREN);
        return;
    }
    if (lookahead == '+') {   // Binary +-operator
        lookahead = get_token();
            // Add paren if necessary
        if (prec > 0) put_token(OPEN_PAREN);
        parse_prefix_with_prec(0);         // Parse right operand
        put_token('+');      // Insert operator symbol
        parse_prefix_with_prec(0);         // Parse left operand
        if (prec > 0) put_token(CLOSE_PAREN);
        return;
    }
    if (lookahead == '\n' || lookahead == EOF)
        fprintf(stderr, "\npre2in: Syntax error: line ended prematurely\n");
    else
        fprintf(stderr,"pre2in: Syntax error: unknown token: '%c'\n", lookahead);
    exit(1);
}
