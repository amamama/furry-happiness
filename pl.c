#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdalign.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define err (fprintf(stderr, "%s:%d:%s: error\n", __FILE__, __LINE__, __func__))

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

#define is(t, p) ((((uintptr_t)p) & 0x7) == t)
#define to(t, p) ((cell_p)((((uintptr_t)p) & ~0x7) | t))
#define car(p) (((cell_p)(((uintptr_t)p) & ~0x7))->car)
#define cdr(p) (((cell_p)(((uintptr_t)p) & ~0x7))->cdr)

cell_p alloc_cell(cell_p car, cell_p cdr, type t) {
	cell_p ret = malloc(sizeof(cell));
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

typedef struct {
	size_t pos;
	size_t len;
} id;

#define SRC "if;quote;lambda;atom;eq;cons;car;cdr;_add;_sub;"
char source[1024] = SRC;
#define  SRC_LEN sizeof(SRC)
enum {
	R_IF,
	R_QUOTE,
	R_LAMBDA,
	NUM_OF_KEYWORD,
	R_ATOM = NUM_OF_KEYWORD,
	R_EQ,
	R_CONS,
	R_CAR,
	R_CDR,
	R_ADD,
	R_SUB,
	NUM_OF_PREDEFINED
};

const id keyword[] = {
	{0, 2},
	{3, 5},
	{9, 6},
	{16, 4},
	{21, 2},
	{24, 4},
	{29, 3},
	{33, 3},
	{37, 4},
	{42, 4},
};

typedef struct {
	enum {
		RO_BRA,
		RO_KET,
		DOT,
		QUOTE,
		NUM,
		ID,
	} type;
	id id;
	int num;
} token;

token tokenize(bool consume) {
	static size_t idx = SRC_LEN;
	int old_idx = SRC_LEN;
	for(; isspace(source[idx]); idx++);
	if(source[idx] == '(') return (idx += consume, (token){RO_BRA, (id){idx - consume, idx + 1}});
	if(source[idx] == ')') return (idx += consume, (token){RO_KET, (id){idx - consume, idx + 1}});
	if(source[idx] == '{') return (idx += consume, (token){RO_BRA, (id){idx - consume, idx + 1}});
	if(source[idx] == '}') return (idx += consume, (token){RO_KET, (id){idx - consume, idx + 1}});
	if(source[idx] == '.') return (idx += consume, (token){DOT, (id){idx - consume, idx + 1}});
	if(source[idx] == '\'') return (idx += consume, (token){QUOTE, (id){idx - consume, idx + 1}});
	if((source[idx] == '-' && isdigit(source[idx + 1])) || isdigit(source[idx])) {
		int num = 0;
		bool negative = source[idx] == '-';
		old_idx = idx;
		for(idx += negative; isdigit(source[idx]); num = num * 10 + source[idx++] - 0x30);
		idx = consume?idx:old_idx;
		return (token){NUM, (id){old_idx, 10}, num};
	}
	else {
		id id = {idx, 0};
		old_idx = idx;
		for(; isgraph(source[idx]); idx++) {
			if(source[idx] == '(') break;
			if(source[idx] == ')') break;
			if(source[idx] == '{') break;
			if(source[idx] == '}') break;
			if(source[idx] == '.') break;
			if(source[idx] == '\'') break;
			id.len++;
		}
		idx = consume?idx:old_idx;
		return (token){ID, id};
	}
}

// lisp := "'"? lisp | "(" lisp* ("." lisp)? ")" | atom

int error(token tok) {
	return fprintf(stderr, "parse failed: %*s", tok.id.len, source + tok.id.pos);
}
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
			tok.type != RO_KET && error(tok);
			return root;
		}
		case QUOTE: {
			tokenize(true);
			cell_p root = alloc_cell(alloc_cell((cell_p)(uintptr_t)keyword[R_QUOTE].pos, (cell_p)(uintptr_t)keyword[R_QUOTE].len, ATOM), NULL, LIST);
			cdr(root) = parse();
			return root;
		}
		case NUM: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(intptr_t)tok.num, NULL, NUMBER);
		}
		case ID: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(uintptr_t)tok.id.pos, (cell_p)(uintptr_t)tok.id.len, ATOM);
		}
		default:
		error(tok);
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
		err;
	}
	return root;
}

bool is_keyword(id keyword, cell_p root) {
	if(is(ATOM, root)) {
		size_t pos = (uintptr_t)car(root);
		size_t len = (uintptr_t)cdr(root);
		return keyword.len == len && !strncmp(source + keyword.pos, source + pos, len);
	}
	return false;
}

bool in_keyword(cell_p root) {
	for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
		if(is_keyword(keyword[i], root)) return true;
	}
	return false;
}

bool is_same_atom(cell_p a, cell_p b) {
	if(!is(ATOM, a) || !is(ATOM, b)) return false;
	if(cdr(a) != cdr(b)) return false;
	return !strncmp(source + (uintptr_t)car(a), source + (uintptr_t)car(b), (uintptr_t)cdr(a));
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
	cell_p ids = car(cdr(car(func)));
	cell_p new_env = cdr(func);
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

cell_p prim_if(cell_p root, cell_p env) {
	cell_p cdr = eval(car(cdr(root)), env);
	return cdr?eval(car(cdr(cdr(root))), env):eval(car(cdr(cdr(cdr(root)))), env);
}

cell_p prim_quote(cell_p root, cell_p env) {
	return cdr(root);
}

cell_p prim_lambda(cell_p root, cell_p env) {
	return alloc_cell(root, env, FUNC);
}

cell_p prim_atom(cell_p root, cell_p env) {
	return is(ATOM, eval(car(cdr(root)), env))?alloc_cell(NULL, NULL, LIST):NULL;
}

cell_p prim_eq(cell_p root, cell_p env) {
	cell_p a = eval(car(cdr(root)), env);
	cell_p b = eval(car(cdr(cdr(root))), env);
	if(!a && !b) goto t;
	if(is(ATOM, a) && is(ATOM, b) && is_same_atom(a, b)) goto t;
	if(is(NUMBER, a) && is(NUMBER, b) && car(a) == car(b)) goto t;
	return NULL;
	t: return alloc_cell(NULL, NULL, LIST);
}

cell_p prim_cons(cell_p root, cell_p env) {
	cell_p a = eval(car(cdr(root)), env);
	cell_p b = eval(car(cdr(cdr(root))), env);
	return alloc_cell(a, b, LIST);
}

cell_p prim_car(cell_p root, cell_p env) {
	return car(eval(car(cdr(root)), env));
}

cell_p prim_cdr(cell_p root, cell_p env) {
	return cdr(eval(car(cdr(root)), env));
}

cell_p prim_add(cell_p root, cell_p env) {
	cell_p a = eval(car(cdr(root)), env);
	cell_p b = eval(car(cdr(cdr(root))), env);
	return alloc_cell((cell_p)((intptr_t)car(a) + (intptr_t)car(b)), NULL, NUMBER);
}

cell_p prim_sub(cell_p root, cell_p env) {
	cell_p a = eval(car(cdr(root)), env);
	cell_p b = eval(car(cdr(cdr(root))), env);
	return alloc_cell((cell_p)((intptr_t)car(a) - (intptr_t)car(b)), NULL, NUMBER);
}

cell_p (*prim_funcs[])(cell_p, cell_p) = {
	prim_if,
	prim_quote,
	prim_lambda,
	prim_atom,
	prim_eq,
	prim_cons,
	prim_car,
	prim_cdr,
	prim_add,
	prim_sub,
};

cell_p eval(cell_p root, cell_p env) {
	if(is(ATOM, root)) {
		cell_p obj = get_from_env(root, env);
		if(obj) return obj;
		if(in_keyword(root)) {
			return root;
		}
		assert(false);
	} else if(is(NUMBER, root)) {
		return root;
	} else if(is(LIST, root)) {
		for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
			if(is_keyword(keyword[i], car(root))) return prim_funcs[i](root, env);
		}
		cell_p car = eval(car(root), env);
		for(size_t i = NUM_OF_KEYWORD; i < NUM_OF_PREDEFINED; i++) {
			if(is_keyword(keyword[i], car)) return prim_funcs[i](root, env);
		}
		assert(is(FUNC, car));
		cell_p new_env = make_new_env(car, cdr(root), env);
		eval(car(cdr(cdr(car(car)))), new_env);
	} else {
		assert(false);
	}
}

int main(void) {
	for(size_t i = SRC_LEN; i < sizeof(source); i++) {
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
