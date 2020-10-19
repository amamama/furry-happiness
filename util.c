#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "pl.h"
#include "util.h"

cell_p alloc_cell(cell_p car, cell_p cdr, cell_type t) {
	cell_p ret = malloc(sizeof(cell));
	assert(ret);
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

size_t length(cell_p list) {
	if(!list) return 0;
	return 1 + length(cdr(list));
}

bool is_dotted_list(cell_p root) {
	if(!root) return false;
	if(is(LIST, cdr(root))) return is_dotted_list(cdr(root));
	return true;
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

cell_p slice(cell_p list, cell_p begin, cell_p end) {
	if(list == end) return NULL;
	if(list == begin || begin == NULL) return cons(car(list), slice(cdr(list), NULL, end));
	return slice(cdr(list), begin, end);
}

cell_p car_cdnr(cell_p r, unsigned int n) {
	return car(cdnr(r, n));
}

cell_p canr(cell_p r, unsigned int n) {
	if(n == 0) return r;
	return canr(car(r), n - 1);
}

cell_p cdnr(cell_p r, unsigned int n) {
	if(n == 0) return r;
	return cdnr(cdr(r), n - 1);
}

