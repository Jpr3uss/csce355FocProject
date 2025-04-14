#ifndef TOKEN_H
#define TOKEN_H

// Grab and return the next token.  The token is read from standard input
// (skipping spaces and tabs but not newlines).
// '\0' is returned if EOF  or '\n' is encountered.
int get_token();

// Output token to standard output.
void put_token(int token);

#endif
