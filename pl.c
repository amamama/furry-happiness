#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdalign.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define err(...) (fprintf(stderr, "%s:%d:%s:", __FILE__, __LINE__, __func__), fprintf(stderr,""  __VA_ARGS__))

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
} type;

#define type(p) ((type)((uintptr_t)p) & 0x7)
#define is(t, p) (type(p) == t)
#define to(t, p) ((cell_p)((((uintptr_t)p) & ~0x7) | t))
#define cons(car, cdr) (alloc_cell(car, cdr, LIST))
#define car(p) (((cell_p)(((uintptr_t)p) & ~0x7))->car)
#define cdr(p) (((cell_p)(((uintptr_t)p) & ~0x7))->cdr)

cell_p alloc_cell(cell_p car, cell_p cdr, type t) {
	cell_p ret = malloc(sizeof(cell));
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

cell_p car_cdnr(cell_p r, unsigned int n) {
	if(n == 0) return car(r);
	return car_cdnr(cdr(r), n - 1);
}

char source[1024] = "";

#define keyword(s, t, n, exp) to_constant(s, t)
#define predefined(s, t, n, exp) to_constant(s, t)
#define begin(k, K) enum {
#define end(k, K) NUM_OF_##K };
#define to_constant(s, t) K_##t,
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_constant

#define keyword(s, t, n, exp) to_lit(s)
#define predefined(s, t, n, exp) to_lit(s)
#define begin(k, K) const char *k[] = {
#define end(k, K) };
#define to_lit(s) s,
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_lit

typedef struct {
	enum {
		RO_BRA,
		RO_KET,
		DOT,
		QUOTE,
		NUM,
		ID,
		EOT,
	} type;
	struct {
		size_t pos;
		size_t len;
	};
	intptr_t num;
} token;

token tokenize(bool consume) {
	static size_t idx = 0;
	int old_idx = 0;
	for(; isspace(source[idx]); idx++);
	if(source[idx] == '\0') return (token){EOT, idx};
	if(source[idx] == '(') return (idx += consume, (token){RO_BRA, {idx - consume, 1}});
	if(source[idx] == ')') return (idx += consume, (token){RO_KET, {idx - consume, 1}});
	if(source[idx] == '.') return (idx += consume, (token){DOT, {idx - consume, 1}});
	if(source[idx] == '\'') return (idx += consume, (token){QUOTE, {idx - consume, 1}});
	if((source[idx] == '-' && isdigit(source[idx + 1])) || isdigit(source[idx])) {
		intptr_t num = 0;
		bool negative = source[idx] == '-';
		old_idx = idx;

		for(idx += negative; isdigit(source[idx]); num = num * 10 + source[idx++] - 0x30);
		size_t len = idx - old_idx;
		num *= negative?-1:1;

		idx = consume?idx:old_idx;
		return (token){NUM, {old_idx, len}, num};
	}
	else {
		token tok = {ID, {idx, 0}};
		old_idx = idx;

		for(; isgraph(source[idx]); idx++, tok.len++) {
			if(source[idx] == '(') break;
			if(source[idx] == ')') break;
			if(source[idx] == '.') break;
		}

		idx = consume?idx:old_idx;
		return tok;
	}
}

// lisp := "'"? lisp | "(" lisp* ("." lisp)? ")" | atom

cell_p parse(void);
cell_p parse_list(void) {
	cell_p ret = NULL;
	token tok = tokenize(false);
	if(tok.type == RO_KET) return ret;
	if(tok.type == DOT) return tokenize(true), parse();
	cell_p car = parse();
	cell_p cdr = parse_list();
	ret = cons(car, cdr);
	return ret;
}

cell_p parse(void) {
	token tok = tokenize(false);
	switch(tok.type) {
		case RO_BRA: {
			tokenize(true);
			cell_p root = parse_list();
			tok = tokenize(true);
			assert(tok.type == RO_KET);
			return root;
		} case QUOTE: {
			tokenize(true);
			cell_p root = cons(alloc_cell((cell_p)(uintptr_t)tok.pos, (cell_p)(uintptr_t)tok.len, ATOM), NULL);
			cdr(root) = parse();
			return root;
		} case NUM: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(intptr_t)tok.num, NULL, NUMBER);
		} case ID: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(uintptr_t)tok.pos, (cell_p)(uintptr_t)tok.len, ATOM);
		} default: {
				err("parse failed: pos: %zd, %*s", tok.pos, (int)tok.len, source + tok.pos);
				exit(EXIT_FAILURE);
		}
	}
}

#define print_rec(root, pre, f1, in, f2, post) (printf(pre), print_##f1(car(root)), printf(in), print_##f2(cdr(root)), printf(post), root)

cell_p print_cell(cell_p);
cell_p print_env(cell_p env) {
	if(!env) return NULL;
	print_rec(car(env), "", cell, " -> ", cell, ";");
	return print_env(cdr(env));
}
cell_p print_frame(cell_p frame) {
	if(!frame) return NULL;
	printf("<");
	print_env(car(frame));
	printf(">");
	return print_frame(cdr(frame));

}

cell_p print_cell(cell_p root) {
	static bool iscdr = false;
	switch(type(root)) {
		case LIST: {
			if(!root) return printf("🈚"), root;
			return print_rec(root, "[", cell, ", ", cell, "]");
		} case ATOM: {
			printf("%.*s", (int)(uintptr_t)cdr(root), source + (uintptr_t)car(root));
			break;
		} case NUMBER: {
			printf("%ld", (intptr_t)car(root));
			break;
		} case FUNC: {
			return print_rec(root, "{", cell, "; ", frame, "}");
		} default: {
			assert(false);
		}
	}
	return root;
}

bool is_same_string(char const *str, cell_p root) {
	if(!is(ATOM, root)) { return false; }
	size_t pos = (uintptr_t)car(root);
	size_t len = (uintptr_t)cdr(root);
	return strlen(str) == len && !strncmp(str, source + pos, len);
}

#define begin(k, K) bool in_##k(cell_p root) { for(size_t i = 0; i < NUM_OF_##K; i++) { if(is_same_string(k[i], root)) { return true; } } return false;
#define end(k, K) }
#define keyword(s, t, n, exp)
#define predefined(s, t, n, exp)
#include "def.h"
#undef begin
#undef end
#undef keyword
#undef predefined

bool is_same_atom(cell_p a, cell_p b) {
	if(type(a) != type(b)) return false;
	switch(type(a)) {
		case ATOM:
		return !strncmp(source + (uintptr_t)car(a), source + (uintptr_t)car(b), (uintptr_t)cdr(a));
		case NUMBER:
		return (intptr_t)car(a) == (intptr_t)car(b);

		default:
		return false;
	}
}

cell_p get_from_env(cell_p atom, cell_p env) {
	if(!env) return NULL;
	if(is_same_atom(atom, car(car(env)))) return cdr(car(env));
	return get_from_env(atom, cdr(env));
}

cell_p get_from_frame(cell_p atom, cell_p frame) {
	if(!frame) return NULL;
	cell_p obj = get_from_env(atom, car(frame));
	if(!obj) return get_from_frame(atom, cdr(frame));
	return obj;
}


cell_p eval(cell_p, cell_p);
cell_p eval_args(cell_p args, cell_p frame) {
	if(!args) return NULL;
	return cons(eval(car(args), frame), eval_args(cdr(args), frame));
}

cell_p make_new_env(cell_p arg_decl, cell_p args, cell_p frame) {
	cell_p env = NULL;
	cell_p evaled_args = eval_args(args, frame);
	for(; arg_decl && is(LIST, arg_decl); ) {
		assert(is(ATOM, car(arg_decl)));
		env = cons(cons(car(arg_decl), car(evaled_args)), env);
		arg_decl = cdr(arg_decl);
		evaled_args = cdr(evaled_args);
	}
	if(is(ATOM, arg_decl)) return cons(cons(arg_decl, evaled_args), env);
	return env;
}

cell_p make_new_frame(cell_p func, cell_p args, cell_p frame) {
	cell_p lambda = car(func);
	cell_p lambda_frame = cdr(func);
	cell_p env = make_new_env(car_cdnr(lambda, 1), args, frame);
	return cons(env, lambda_frame);
}

#define keyword(s, t, n, exp) def_func(t, n, exp)
#define predefined(s, t, n, exp) def_func(t, n, exp)
#define begin(k, K)
#define end(k, K)
#define def_func(t, n, exp) cell_p prim_##t(cell_p root, cell_p frame) { A(n); return exp; }
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef def_func

#define keyword(s, t, n, exp) to_funcname(t)
#define predefined(s, t, n, exp) to_funcname(t)
#define begin(k, K) cell_p (*k##_funcs[])(cell_p, cell_p) = {
#define end(k, K) };
#define to_funcname(s) prim_##s,
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_funcname

cell_p apply(cell_p func, cell_p args, cell_p frame) {
	cell_p new_frame = cons(NULL, make_new_frame(func, args, frame));
	cell_p body = cdr(cdr(car(func)));
	cell_p ret = NULL;
	for(; body; body = cdr(body)) {
		ret = eval(car(body), new_frame);
	}
	return ret;
}

cell_p eval(cell_p root, cell_p frame) {
	switch(type(root)) {
		case ATOM: {
			cell_p obj = get_from_frame(root, frame);
			if(obj) return obj;
			if(in_predefined(root)) {
				return root;
			}
			assert(false);
		} case NUMBER: {
			return root;
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_same_string(keyword[i], car(root))) return keyword_funcs[i](root, frame);
			}
			cell_p evaled_car = eval(car(root), frame);
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_same_string(predefined[i], evaled_car)) return predefined_funcs[i](root, frame);
			}
			assert(is(FUNC, evaled_car));
			return apply(evaled_car, cdr(root), frame);
		} default: {
			assert(false);
		}
	}
}

int main(void) {
	for(size_t i = 0; i < sizeof(source); i++) {
		source[i] = getchar();
		if(source[i] == EOF) {
			source[i] = 0;
			break;
		}
	}
	cell_p ast = print_cell(parse());
	puts("");
	print_cell(eval(ast, NULL));
	puts("");
}

