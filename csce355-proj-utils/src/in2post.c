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

// Each function processes the given infix construct
// Precondition: lookahead is the first token of the construct
// Postcondition: lookahead is the first token after the construct
void myunion(), concatenation(), star(), atom();

void parse_infix();

int lookahead;

int main(int argc, char *argv[]) // Command line args ignored for now
{
        // Read infix regexes from standard input, one per line,
        // and parse them.
    int c;

    while ((c = getchar()) != EOF) {
        ungetc(c, stdin);
        parse_infix();
        fflush(stdout);
    }
    return 0;
}

// Parse an infix regex and convert to postfix.
void parse_infix()
{
    lookahead = get_token();
    if (lookahead == '\n')
        return;

        // The top-level recursive descent parser
    myunion();
    put_token('\n');

        // Check for extra tokens on the line
    if (lookahead != '\n') {
        fprintf(stderr, "in2post: Syntax error: newline expected\n");
        exit(1);
    }
}

// Parse and output an entire regex (assumed to be the union of
// one or more concatenations)
// Precondition: lookahead is the first token of the union
// Postcondition: lookahead is the first token after the union
void myunion()
{
    concatenation();
    while (lookahead == '+') {
        lookahead = get_token();
        concatenation();
        put_token('+');
    }
}

// Parse and output the concatenation of one or more starred regexes
// Precondition: lookahead is the first token of the concatenation
// Postcondition: lookahead is the first token after the concatenation
void concatenation()
{
    star();
    while (lookahead != '+' && lookahead != CLOSE_PAREN
           && lookahead != '\n' && lookahead != EOF) {
        star();
        put_token('.');
    }
}

// Parse and output a starred regex (a syntactic atom followed by
// zero or more stars)
// Precondition: lookahead is the first token of the starred regex
// Postcondition: lookahead is the first token after the starred regex
void star()
{
    atom();
    while (lookahead == '*') {
        lookahead = get_token();
        put_token('*');
    }
}

// Parse and output a regex that is a syntactic atom (either the empty set,
// a single alphabet symbol, or a parenthesized regex)
// Precondition: lookahead is the first token of the syntactic atom
// Postcondition: lookahead is the first token after the syntactic atom
void atom()
{
    if (lookahead == '/' || isdigit(lookahead) || islower(lookahead)) {
        put_token(lookahead);
        lookahead = get_token();
        return;
    }
    if (lookahead == OPEN_PAREN) {
        lookahead = get_token();   // Read past the opening parenthesis
        myunion();                 // Process what's inside the parentheses
        if (lookahead != CLOSE_PAREN) {
            fprintf(stderr, "\nin2post: Syntax error: '%c' expected\n", CLOSE_PAREN);
            exit(1);
        }
        lookahead = get_token();   // Read past the closing parenthesis
        return;
    }
    fprintf(stderr, "\nin2post: Syntax error: illegal token: '%c'\n", lookahead);
    exit(1);
}
