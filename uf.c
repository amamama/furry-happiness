#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
	enum {
		ATOM,
		NUMBER,
		TV,
		QV,
		LIST,
		PAIR,
		ARROW,
	} tag;
	size_t car;
	size_t cdr;
} type;

int print_type(type t) {
	switch(t.tag) {
		case ATOM:
		printf("@");
		break;

		case NUMBER:
		printf("\u2124");
		break;

		case TV:
		printf("TV: %zd", t.car);
		break;

		case QV:
		printf("\u2200: %zd", t.car);
		break;

		case LIST:
		printf("[%zd]", t.car);
		break;

		case PAIR:
		printf("%zd \u00d7 %zd", t.car, t.cdr);
		break;

		case ARROW:
		printf("%zd \u2192 %zd", t.car, t.cdr);
		break;

		default:
		assert(false);
	}
	return 0;
}

typedef struct {
	size_t parent;
	type type;
} uf_list;

typedef struct uf {
	size_t length;
	size_t capacity;
	uf_list *arr;
} uf, *uf_p;
#define type(u, idx) (u->arr[idx].type)
#define tag(u, idx) (type(u, idx).tag)
#define car(u, idx) (type(u, idx).car)
#define cdr(u, idx) (type(u, idx).cdr)

uf_p init_uf(void) {
	uf_p ret = malloc(sizeof(uf));
	ret->length = 0;
	ret->capacity = 1;
	ret->arr = malloc(sizeof(uf_list));
	return ret;
}

int print_uf(uf_p u) {
	printf("[\n");
	for(size_t i = 0; i < u->length; i++) {
		printf("%zd = {parent = %zd, ", i, u->arr[i].parent);
		print_type(type(u, i));
		printf("},\n");
	}
	printf("]\n");
	return 0;
}

size_t add_type(uf_p u, type t) {
	if(u->length == u->capacity) u->arr = realloc(u->arr, sizeof(uf_list) * (u->capacity *= 2));
	u->arr[u->length] = (uf_list){u->length, t};
	return u->length++;
}

size_t root(uf_p u, size_t l) {
	if(u->arr[l].parent == l) return l;
	else u->arr[l].parent = root(u, u->arr[l].parent);
}

size_t unite(uf_p u, size_t a, size_t b) {
	size_t ra = root(u, a), rb = root(u, b);
	if(ra == rb) return a;
	return u->arr[rb].parent = ra;
}

bool find(uf_p u, size_t a, size_t b) {
	return root(u, a) == root(u, b);
}

#define is_sametag(u, t1, t2, t) (tag(u, t1) == t && tag(u, t2) == t)
size_t unify(uf_p u, size_t t1, size_t t2) {
	assert(tag(u, t1) != QV && tag(u, t2) != QV);
	size_t rt1 = root(u, t1), rt2 = root(u, t2);
	if(rt1 == rt2) return rt1;
	if(is_sametag(u, rt1, rt2, ATOM) || is_sametag(u, rt1, rt2, NUMBER)) return rt1;
	if(is_sametag(u, rt1, rt2, LIST)) {
		unite(u, rt1, rt2);
		return unite(u, car(u, rt1), car(u, rt2));
	}
	if(is_sametag(u, rt1, rt2, PAIR) || is_sametag(u, rt1, rt2, ARROW)) {
		unite(u, rt1, rt2);
		unify(u, car(u, rt1), car(u, rt2));
		unify(u, cdr(u, rt1), cdr(u, rt2));
		return t1;
	}
	if(tag(u, rt1) == TV && tag(u, rt2) != TV) {
		return unite(u, rt2, rt1);
	}
	if(tag(u, rt2) == TV && tag(u, rt1) != TV) {
		return unite(u, rt1, rt2);
	}
	if(is_sametag(u, rt1, rt2, TV)) {
		return unite(u, rt1, rt2);
	}

	return assert(false), 0;
}

int main(void) {
	uf_p u = init_uf();
	type t[10] = {0};
	for(size_t i = 0; i < 7; i++) {
		add_type(u, t[i] = (type){i, i, i + 1});
	}
	print_uf(u);
}
