This directory contains the source files for the three utilities to
support the CSCE 355 programming project.

Author: Stephen A. Fenner (fenner@cse.sc.edu)
Date: April 3, 2025

The three executable utilities are:

  - in2post - converts infix regexes to the corresponding postfix
  - pre2in  - converts prefix regexes to the corresponding infix
  - revline - prints the tokens on the line in reverse

These utilities are meant to run on linux-like systems but should be
adaptable to other systems.

Each utility reads a sequence of zero or more lines from standard
input and outputs the results, line by line, to standard output.
The input can be typed in from a keyboard (end with Ctrl-D on an empty
line) or redirected from a text file.  Spaces and tabs on the input
are ignored and omitted from the output.  All text starting with '#'
and going to the end of the line is considered a comment and is
ignored.

PIPING:
Using standard input/output makes it convenient to pipe the output of
one utility to the input of another.  Using pipes allows for more
possible conversions:

To convert from infix to prefix:
    in2post | revline | pre2in | in2post | revline

To convert from postfix to infix:
    revline | pre2in | in2post | revline | pre2in

To convert from prefix to postfix:
    pre2in | in2post

To convert from postfix to prefix:
    revline | pre2in | in2post | revline

Each pipe above conforms to the following two rules:
   - the number of revline's in the pipe must be even
   - revline should never take infix expressions as input (line
     reversal only makes sense for prefix and postfix expressions)

Files:

Makefile - controls the build (just type `make')
in2post.c   - main source for in2post
pre2in.c    - main source for pre2in
revline.c   - main source for revline
token.[ch]  - tokenizer utility used by the three programs above

These programs are written in C and should be highly portable, making
minimal platform-related assumptions.  The Makefile assumes gcc is
available, but any reasonable C compiler should suffice.
