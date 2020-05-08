override CFLAGS := -Wall -g ${CFLAGS}

all: a.out

a.out: pl.o compiler.o util.o
	gcc -g pl.o compiler.o util.o


pl.o: pl.c pl.h compiler.h
	gcc ${CFLAGS} -c pl.c

compiler.o: compiler.c pl.h
	gcc ${CFLAGS} -c compiler.c

util.o: util.c util.h
	gcc ${CFLAGS} -c util.c

pl.h: def.h
	touch pl.h

test: a.out
	./a.out test1.lisp > test_output
	./a.out test2.lisp >> test_output
	./a.out test3.lisp >> test_output
	./a.out test4.lisp >> test_output
	./a.out args.lisp >> test_output
	./a.out closure.lisp >> test_output
	./a.out even.lisp >> test_output
	./a.out fact.lisp >> test_output
	./a.out fix.lisp >> test_output
	./a.out forward_ref.lisp >> test_output
	./a.out forward_ref2.lisp >> test_output
	#./a.out shadow.lisp >> test_output


