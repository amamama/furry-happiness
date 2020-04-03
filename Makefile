all: a.out

a.out: pl.o cps.o
	gcc -g pl.o cps.o

pl.o: pl.c def.h
	gcc -g -Wall  -c pl.c

cps.o: cps.c
	gcc -g -Wall -c cps.c

