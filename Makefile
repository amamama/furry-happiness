override CFLAGS := -Wall -g ${CFLAGS}

all: a.out

a.out: pl.o cps.o util.o
	gcc -g pl.o cps.o util.o

pl.o: pl.c def.h pl.h cps.h
	gcc ${CFLAGS} -c pl.c

cps.o: cps.c pl.h
	gcc ${CFLAGS} -c cps.c

util.o: util.c util.h
	gcc ${CFLAGS} -c util.c
