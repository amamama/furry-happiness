override CFLAGS := -Wall -g ${CFLAGS}

all: a.out

a.out: pl.o cps.o
	gcc -g pl.o cps.o

pl.o: pl.c def.h
	gcc ${CFLAGS} -c pl.c

cps.o: cps.c
	gcc ${CFLAGS} -c cps.c

