#ifndef PL_H
#define PL_H

#include <stdalign.h>

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

cell_p alloc_cell(cell_p, cell_p, cell_type);
cell_p car_cdnr(cell_p, unsigned int);

cell_p get_from_frame(cell_p, cell_p);
bool is_same_string(char const *, cell_p);

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
