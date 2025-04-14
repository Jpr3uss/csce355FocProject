all : revline in2post pre2in

revline : revline.o token.o
	gcc revline.o token.o -o revline

in2post : in2post.o token.o
	gcc in2post.o token.o -o in2post

pre2in : pre2in.o token.o
	gcc pre2in.o token.o -o pre2in

revline.o : revline.c token.h
	gcc -c revline.c

in2post.o : in2post.c token.h
	gcc -c in2post.c

pre2in.o : pre2in.c token.h
	gcc -c pre2in.c

token.o : token.c token.h
	gcc -c token.c

clean :
	-rm revline in2post pre2in *.o
