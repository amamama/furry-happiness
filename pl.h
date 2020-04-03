#ifndef PL_H
#define PL_H

#include <stdalign.h>
#include <stdint.h>

typedef struct cell *cell_p;
typedef struct cell {
	cell_p alignas(8) car;
	cell_p alignas(8) cdr;
} cell, *cell_p;

typedef enum {
	LIST,
	ATOM,
	NUMBER,
	FUNC,
	BROKEN_HEART = 7,
} cell_type;

#define cty(p) ((cell_type)((uintptr_t)p) & 0x7)
#define is(t, p) (cty(p) == t)
#define to(t, p) ((cell_p)((((uintptr_t)p) & ~0x7) | t))
#define cons(car, cdr) (alloc_cell(car, cdr, LIST))
#define car(p) (((cell_p)(((uintptr_t)p) & ~0x7))->car)
#define cdr(p) (((cell_p)(((uintptr_t)p) & ~0x7))->cdr)
#define str_to_atom(str) (alloc_cell((cell_p)str, (cell_p)(uintptr_t)strlen(str), ATOM))

cell_p alloc_cell(cell_p, cell_p, cell_type);
cell_p car_cdnr(cell_p, unsigned int);

//cell_p get_from_frame(cell_p, cell_p);
bool is_same_string(char const *, cell_p);

#define make_lambda(args, bodies) (cons(str_to_atom("lambda"), cons(args, bodies)))
#define app2(f, v) (cons(f, cons(v, NULL)))
#define nil (app2(str_to_atom("'"), NULL))



#define genvar(func, prefix) \
cell_p genvar_##func() { \
	static unsigned int n = 0; \
	static char func##_vars[65536] = ""; \
	static char *next_buffer = func##_vars; \
	char var[32] = ""; \
	size_t len = snprintf(var, sizeof(var), prefix "%x", n++); \
	assert(len < sizeof(var)); \
	strncpy(next_buffer, var, len + 1); /* for '\0' character */ \
	cell_p ret = str_to_atom(next_buffer); \
	next_buffer += len + 1; /* for '\0' */ \
	return ret; \
}



#define keyword(s, t, n, exp) to_constant(K, t)
#define predefined(s, t, n, exp) to_constant(P, t)
#define begin(k, K) enum {
#define end(k, K) NUM_OF_##K };
#define to_constant(T, t) T##_##t,
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_constant

#define keyword(s, t, n, exp)
#define predefined(s, t, n, exp)
#define begin(k, K) extern const char *k[];
#define end(k, K)
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end

#endif
