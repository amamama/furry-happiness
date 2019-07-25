#ifdef INCLUDE_FILE
begin(keyword, KEYWORD)
keyword("'", q, 0, cdr(root))
keyword("quote", quote, 0, cdr(root))
keyword("if", if, 0, arg(1)?arg(2):arg(3))
keyword("lambda", lambda, 0, alloc_cell(root, env, FUNC))
end(keyword, KEYWORD)

begin(predefined, PREDEFINED)
predefined("atom", atom, 1, is(ATOM, a(1))?alloc_cell(NULL, NULL, LIST):NULL)
predefined("eq", eq, 2, is_same_atom(a(1), a(2))?alloc_cell(NULL, NULL, LIST):NULL)
predefined("cons", cons, 2, alloc_cell(a(1), a(2), LIST))
predefined("car", car, 1, car(a(1)))
predefined("cdr", cdr, 1, cdr(a(1)))
predefined("_add", add, 2, alloc_cell(arith_biop(a(1), +, a(2)), NULL, NUMBER))
predefined("_sub", sub, 2, alloc_cell(arith_biop(a(1), -, a(2)), NULL, NUMBER))
predefined("_mul", mul, 2, alloc_cell(arith_biop(a(1), *, a(2)), NULL, NUMBER))
predefined("_div", div, 2, alloc_cell(arith_biop(a(1), /, a(2)), NULL, NUMBER))
predefined("_mod", mod, 2, alloc_cell(arith_biop(a(1), %, a(2)), NULL, NUMBER))
end(predefined, PREDEFINED)

#else

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
#define car(p) (((cell_p)(((uintptr_t)p) & ~0x7))->car)
#define cdr(p) (((cell_p)(((uintptr_t)p) & ~0x7))->cdr)

cell_p alloc_cell(cell_p car, cell_p cdr, type t) {
	cell_p ret = malloc(sizeof(cell));
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

char source[1024] = "";

#define INCLUDE_FILE
#define keyword(s, t, n, exp) to_constant(s, t)
#define predefined(s, t, n, exp) to_constant(s, t)
#define begin(k, K) enum {
#define end(k, K) NUM_OF_##K };
#define to_constant(s, t) K_##t,
#include __FILE__
#undef INCLUDE_FILE
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_constant

#define INCLUDE_FILE
#define keyword(s, t, n, exp) to_lit(s)
#define predefined(s, t, n, exp) to_lit(s)
#define begin(k, K) const char *k[] = {
#define end(k, K) };
#define to_lit(s) s,
#include __FILE__
#undef INCLUDE_FILE
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
	ret = alloc_cell(car, cdr, LIST);
	return ret;
}

cell_p parse(void) {
	token tok = tokenize(false);
	switch(tok.type) {
		case RO_BRA: {
			tokenize(true);
			cell_p root = parse_list();
			tok = tokenize(true);
			if(tok.type != RO_KET) {
				err("parse failed: pos: %zd, %*s", tok.pos, (int)tok.len, source + tok.pos);
				exit(EXIT_FAILURE);
			}
			return root;
		} case QUOTE: {
			tokenize(true);
			cell_p root = alloc_cell(alloc_cell((cell_p)(uintptr_t)tok.pos, (cell_p)(uintptr_t)tok.len, ATOM), NULL, LIST);
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

cell_p print_cell(cell_p);
cell_p print_env(cell_p env) {
	if(!env) return NULL;
	print_cell(car(car(env)));
	printf(" -> ");
	print_cell(cdr(car(env)));
	printf(";");
	print_env(cdr(env));
	return env;
}

cell_p print_cell(cell_p root) {
	static bool iscdr = false;
	if(is(LIST, root)) {
		if(!root) return printf("NULL"), root;
		printf("[");
		print_cell(car(root));
		printf(", ");
		print_cell(cdr(root));
		printf("]");
	} else if(is(ATOM, root)) {
		printf("%.*s", (int)(uintptr_t)cdr(root), source + (uintptr_t)car(root));
	} else if(is(NUMBER, root)) {
		printf("%ld", (intptr_t)car(root));
	} else if(is(FUNC, root)) {
		printf("{");
		print_cell(car(root));
		printf(", ");
		print_env(cdr(root));
		printf("}");
	} else {
		assert(false);
	}
	return root;
}

bool is_same_string(char const *str, cell_p root) {
	if(!is(ATOM, root)) { return false; }
	size_t pos = (uintptr_t)car(root);
	size_t len = (uintptr_t)cdr(root);
	return strlen(str) == len && !strncmp(str, source + pos, len);
}

#define INCLUDE_FILE
#define begin(k, K) bool in_##k(cell_p root) { for(size_t i = 0; i < NUM_OF_##K; i++) { if(is_same_string(k[i], root)) { return true; } } return false;
#define end(k, K) }
#define keyword(s, t, n, exp)
#define predefined(s, t, n, exp)
#include __FILE__
#undef INCLUDE_FILE
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

cell_p car_cdnr(cell_p r, unsigned int n) {
	if(n == 0) return car(r);
	return car_cdnr(cdr(r), n - 1);
}

cell_p get_from_env(cell_p atom, cell_p env) {
	if(!env) return NULL;
	if(is_same_atom(atom, car(car(env)))) return cdr(car(env));
	return get_from_env(atom, cdr(env));
}

cell_p eval(cell_p, cell_p);
cell_p eval_args(cell_p args, cell_p env) {
	if(!args) return NULL;
	return alloc_cell(eval(car(args), env), eval_args(cdr(args), env), LIST);
}

cell_p make_new_env(cell_p func, cell_p args, cell_p env) {
	cell_p lambda = car(func);
	cell_p new_env = cdr(func);
	cell_p ids = car_cdnr(lambda, 1);
	cell_p evaled_args = eval_args(args, env);
	for(; ids && is(LIST, ids); ) {
		assert(is(ATOM, car(ids)));
		new_env = alloc_cell(alloc_cell(car(ids), car(evaled_args), LIST), new_env, LIST);
		ids = cdr(ids);
		evaled_args = cdr(evaled_args);
	}
	if(is(ATOM, ids)) return alloc_cell(alloc_cell(ids, evaled_args, LIST), new_env, LIST);
	return new_env;
}

#define INCLUDE_FILE
#define A(n) A##n
#define arg(n) eval(car_cdnr(root, n), env)
#define decl_arg(n) cell_p a##n = arg(n)
#define A0
#define A1 A0; decl_arg(1)
#define A2 A1; decl_arg(2)
#define A3 A2; decl_arg(3)
#define A4 A3; decl_arg(4)
#define A5 A4; decl_arg(5)
#define A6 A5; decl_arg(6)
#define a(n) a##n
#define def_func(t, n, exp) cell_p prim_##t(cell_p root, cell_p env) { A(n); return exp; }
#define arith_biop(a,op, b) ((cell_p)(((intptr_t)car(a)) op ((intptr_t)car(b))))

#define keyword(s, t, n, exp) def_func(t, n, exp)
#define predefined(s, t, n, exp) def_func(t, n, exp)
#define begin(k, K)
#define end(k, K)
#include __FILE__
#undef INCLUDE_FILE
#undef A
#undef arg
#undef decl_arg
#define A0
#undef A1
#undef A2
#undef A3
#undef A4
#undef A5
#undef A6
#undef a
#undef def_func
#undef arith_biop
#undef keyword
#undef predefined
#undef begin
#undef end

#define INCLUDE_FILE
#define keyword(s, t, n, exp) to_funcname(t)
#define predefined(s, t, n, exp) to_funcname(t)
#define begin(k, K) cell_p (*k##_funcs[])(cell_p, cell_p) = {
#define end(k, K) };
#define to_funcname(s) prim_##s,
#include __FILE__
#undef INCLUDE_FILE
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_funcname

cell_p eval(cell_p root, cell_p env) {
	switch(type(root)) {
		case ATOM: {
			cell_p obj = get_from_env(root, env);
			if(obj) return obj;
			if(in_predefined(root)) {
				return root;
			}
			assert(false);
		} case NUMBER: {
			return root;
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_same_string(keyword[i], car(root))) return keyword_funcs[i](root, env);
			}
			cell_p evaled_car = eval(car(root), env);
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_same_string(predefined[i], evaled_car)) return predefined_funcs[i](root, env);
			}
			assert(is(FUNC, evaled_car));
			cell_p new_env = make_new_env(evaled_car, cdr(root), env);
			return eval(car(cdr(cdr(car(evaled_car)))), new_env);
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

#endif //INCLUDE_FILE
