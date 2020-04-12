#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "pl.h"

cell_p alloc_cell(cell_p car, cell_p cdr, cell_type t) {
	cell_p ret = malloc(sizeof(cell));
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

size_t length(cell_p list) {
	if(!list) return 0;
	return 1 + length(cdr(list));
}

cell_p copy(cell_p root, int depth) {
	if(!root) return root;
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			if(depth == 0) return root;
			return alloc_cell(car(root), cdr(root), cty(root));
		}
		case LIST: {
			if(depth == 0) return root;
			return alloc_cell(copy(car(root), depth > 0?depth - 1:depth), copy(cdr(root), depth), LIST);
		} default: {
			assert(false);
		}
	}
}

cell_p append(cell_p a, cell_p b) {
	if(!a) return b;
	return cons(car(a), append(cdr(a), b));
}

cell_p car_cdnr(cell_p r, unsigned int n) {
	if(n == 0) return car(r);
	return car_cdnr(cdr(r), n - 1);
}

