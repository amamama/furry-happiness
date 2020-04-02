#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define err(...) (fprintf(stderr, "%s:%d:%s:", __FILE__, __LINE__, __func__), fprintf(stderr,""  __VA_ARGS__))


#include "pl.h"
#include "uf.h"

cell_p alloc_cell(cell_p car, cell_p cdr, cell_type t) {
	cell_p ret = malloc(sizeof(cell));
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

cell_p car_cdnr(cell_p r, unsigned int n) {
	if(n == 0) return car(r);
	return car_cdnr(cdr(r), n - 1);
}

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

char source[65536] = "";

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
	if(source[idx] == '\0') return (token){EOT, {idx, 0}};
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
			cell_p root = cons(alloc_cell((cell_p)(source + tok.pos), (cell_p)(uintptr_t)tok.len, ATOM), NULL);
			cdr(root) = alloc_cell(parse(), NULL, LIST);
			return root;
		} case NUM: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(intptr_t)tok.num, NULL, NUMBER);
		} case ID: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(source + tok.pos), (cell_p)(uintptr_t)tok.len, ATOM);
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
	print_rec(car(env), "", cell, " -> ", cell, ";\n");
	return print_env(cdr(env));
}


cell_p visited[65536];
size_t idx = 0;

cell_p print_frame(cell_p frame) {
	if(!frame) return NULL;

	for(size_t i = 0; i <= idx; i++) {
		if(visited[i] == frame) return printf("%p", frame), NULL;
	}
	visited[idx++] = frame;
	printf("%p : ", frame);

	printf("<");
	print_env(car(frame));
	printf(">\n");
	return print_frame(cdr(frame));
}

#define print_cell print_cell_aux
#define cell cell_aux
cell_p print_cell(cell_p root) {
	switch(cty(root)) {
		case LIST: {
			//if(!root) return printf("🈚"), root;
			if(!root) return printf("🈳"), root;
			//if(!root) return printf("🍌"), root;
			return printf("%p : ", root), print_rec(root, "(", cell, ", ", cell, ")");
		} case ATOM: {
			printf("%.*s", (int)(uintptr_t)cdr(root), (char*)car(root));
			break;
		} case NUMBER: {
			printf("%ld", (intptr_t)car(root));
			break;
		} case FUNC: {
			return print_rec(root, "{", cell, "; ", frame, "}\n");
		} default: {
			assert(false);
		}
	}
	return root;
}

#undef print_cell
#undef cell

cell_p print_cell(cell_p root) {
	idx = 0;
	return print_cell_aux(root);
}

cell_p print_list(cell_p root) {
	switch(cty(root)) {
		case LIST: {
			if(!root) return printf("()"), root;
			printf("(");
			print_list(car(root));
			for(cell_p c = cdr(root); c; c = cdr(c)) {
				printf(" ");
				print_list(car(c));
			}
			printf(")");
			return root;
		} case ATOM: {
			printf("%.*s", (int)(uintptr_t)cdr(root), (char*)car(root));
			break;
		} case NUMBER: {
			printf("%ld", (intptr_t)car(root));
			break;
		} case FUNC: {
			return err("FUNC ha denai hazu"), NULL;
		} default: {
			assert(false);
		}
	}
	return root;
}

bool is_same_string(char const *str, cell_p root) {
	if(!is(ATOM, root)) { return false; }
	char *pos = (char*)car(root);
	size_t len = (uintptr_t)cdr(root);
	return strlen(str) == len && !strncmp(str, pos, len);
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
	if(cty(a) != cty(b)) return false;
	switch(cty(a)) {
		case ATOM:;
		uintptr_t longer_length = (uintptr_t)cdr(a) < (uintptr_t)cdr(b)?(uintptr_t)cdr(b):(uintptr_t)cdr(a);
		return !strncmp((char*)car(a), (char*)car(b), longer_length);
		case NUMBER:
		return (intptr_t)car(a) == (intptr_t)car(b);

		default:
		return false;
	}
}

cell_p get_from_env(cell_p atom, cell_p env) {
	if(!env) return NULL;
	if(is_same_atom(atom, car(car(env)))) return car(env);
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
#define to_funcname(t) prim_##t,
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
	switch(cty(root)) {
		case ATOM: {
			if(in_predefined(root)) return root;
			cell_p obj = get_from_frame(root, frame);
			if(!obj) print_cell(root), puts(" is not in frame");
			assert(obj);
			return cdr(obj);
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


char cps_vars[65536] = "";
cell_p gen_cps_var() {
	static unsigned int n = 0;
	static char *next_buffer = cps_vars;
	char var[32] = "";
	size_t len = snprintf(var, sizeof(var), "継続%x", n++);
	assert(len < sizeof(var));
	strncpy(next_buffer, var, len + 1); //for '\0' character
	cell_p ret = str_to_atom(next_buffer);
	next_buffer += len + 1; //for '\0'
	return ret;
}

// cps変換のため，
// (lambda (args) pre_bodies (define v e) post_bodies)
// -> (lambda (args) pre_bodies (rewrite_define(<<<(lambda (v) (set! v e) post_bodies)>>>) '()))
// のようにする．
// これによって，ナイーブな後方参照や相互再帰ができなくなる．
/*
((lambda ()
	(define add5 (lambda (x) (_add (id 5) x)))
	(define id (lambda (x) x))
	(add5 10)
	))
*/
// 手動でset!すると動くのでこれで上記の問題はこれでなんとかする
// あとでマクロを作ることができればそれで解決する
cell_p rewrite_define(cell_p);
cell_p rewrite_define_aux(cell_p root) {
	assert(is_same_string("lambda", car(root)));
	cell_p body = cdr(cdr(root));
	for(; body && !is_same_string("define", car(car(body))); body = cdr(body)) {
		car(body) = rewrite_define(car(body));
	}
	if(!body) return root;
	cell_p var = car_cdnr(car(body), 1);
	cell_p exp = rewrite_define(car_cdnr(car(body), 2));
	cell_p set_exp = cons(str_to_atom("set!"), cons(var, cons(exp, NULL)));
	cell_p new_lambda = cons(car(root), cons(cons(var, NULL), cons(set_exp, cdr(body))));
	new_lambda = rewrite_define(new_lambda);
	car(body) = cons(new_lambda, cons(cons(str_to_atom("'"), cons(NULL, NULL)), NULL));
	cdr(body) = NULL;
	return root;
}

cell_p rewrite_define(cell_p root) {
	if(!root) return root;
	switch(cty(root)) {
		case ATOM:
		case NUMBER:
		return root;
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_same_string(keyword[i], car(root))) {
					switch(i) {
						case K_lambda: {
							return rewrite_define_aux(root);
						}
						case K_define: {
							// this case should not be executed
							assert(false);
							return NULL;
						}
					}
				}
			}
			for(cell_p c = root; c; c = cdr(c)) {
				car(c) = rewrite_define(car(c));
			}
			return root;
		} default: {
			assert(false);
		}
	}
}

// (lambda (args) bodies)
/* (cont
   (lambda (k0 args) to_cps(body1,
   (lambda (k1) to_cps(body2,
   (lambda (k2) to_cps(body...,
   (lambda (kn) to_cps(bodyn, k0)))))))))
   */

#define make_lambda(args, bodies) (cons(str_to_atom("lambda"), cons(args, bodies)))
#define app2(f, v) (cons(f, cons(v, NULL)))

cell_p to_cps(cell_p, cell_p);
cell_p bodies_to_cps(cell_p bodies, cell_p cont_var) {
	if(!cdr(bodies)) return to_cps(car(bodies), cont_var);
	cell_p new_var = gen_cps_var();
	cell_p new_lambda = make_lambda(cons(new_var, NULL), cons(bodies_to_cps(cdr(bodies), cont_var), NULL));
	return to_cps(car(bodies), new_lambda);
}

cell_p apply_to_cps_aux(cell_p exp, cell_p cont_vars, cell_p cont) {
	cell_p body = cdr(exp)?apply_to_cps_aux(cdr(exp), cdr(cont_vars), cont):cont;
	cell_p new_cont = make_lambda(cons(car(cont_vars), NULL), cons(body, NULL));
	return to_cps(car(exp), new_cont);
}
cell_p apply_to_cps(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = root; e; e = cdr(e)) {
		cont_vars = cons(gen_cps_var(), cont_vars);
	}
	cell_p apply_cont = cons(car(cont_vars), cons(cont, cdr(cont_vars)));
	return apply_to_cps_aux(root, cont_vars, apply_cont);
}

cell_p predefined_to_cps(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = cdr(root); e; e = cdr(e)) {
		cont_vars = cons(gen_cps_var(), cont_vars);
	}
	cell_p predefined_cont = cons(cont, cons(cons(car(root), cont_vars), NULL));
	return apply_to_cps_aux(cdr(root), cont_vars, predefined_cont);
}

cell_p to_cps(cell_p root, cell_p cont) {
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			// (cont v)
			return cons(cont, cons(root, NULL));
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_same_string(keyword[i], car(root))) {
					switch(i) {
						case K_q:
						case K_quote: {
							// (quote v)
							// (cont (quote v))
							 return cons(cont, cons(root, NULL));
						} case K_if: {
							// (if cond t e)
							// ((λk0 .to_cps(cond, (λk1. (if k1 to_cps(t, k0) to_cps(e, k0))))) cont)
							cell_p cond = car_cdnr(root, 1);
							cell_p then_cls = car_cdnr(root, 2);
							cell_p else_cls = car_cdnr(root, 3);
							cell_p new_var0 = gen_cps_var();
							cell_p new_var1 = gen_cps_var();
							cell_p new_then_cls = to_cps(then_cls, new_var0);
							cell_p new_else_cls = to_cps(else_cls, new_var0);
							cell_p new_if = cons(str_to_atom("if"), cons(new_var1, cons(new_then_cls, cons(new_else_cls, NULL))));
							cell_p new_cont = make_lambda(cons(new_var1, NULL), cons(new_if, NULL));
							cell_p new_lambda = make_lambda(cons(new_var0, NULL), cons(to_cps(cond, new_cont), NULL));
							return cons(new_lambda, cons(cont, NULL));
						} case K_lambda: {
							// (lambda (args) bodies)
							/* (cont
							(lambda (k0 args) to_cps(body1,
									(lambda (k1) to_cps(body2,
										(lambda (k2) to_cps(body...,
											(lambda (kn) to_cps(bodyn, k0)))))))))
							*/
							cell_p new_var = gen_cps_var();
							cell_p bodies = cdr(cdr(root));
							cell_p new_args = cons(new_var, car_cdnr(root, 1));
							cell_p new_body = bodies_to_cps(bodies, new_var);
							cell_p new_lambda = make_lambda(new_args, cons(new_body, NULL));
							 return cons(cont, cons(new_lambda, NULL));
						}
						case K_define: {
							// this case should not be appeared
							assert(false);
							return NULL;
						}
						case K_set: {
							//(set! v e)
							// to_cps(e, (lambda (k0) (cont (set! v k0))))
							cell_p var = car_cdnr(root, 1);
							cell_p exp = car_cdnr(root, 2);
							cell_p new_var = gen_cps_var();
							cell_p body = cons(cont, cons(cons(str_to_atom("set!"), cons(var, cons(new_var, NULL))), NULL));
							cell_p new_cont = make_lambda(cons(new_var, NULL), cons(body, NULL));
							 return to_cps(exp, new_cont);
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_same_string(predefined[i], car(root))) {
					return predefined_to_cps(root, cont);
				}
			}
			return apply_to_cps(root, cont);
		} default: {
			assert(false);
		}
	}
}

int main(int argc, char **argv) {
	FILE *fp = argv[1][0] == '-'?stdin:fopen(argv[1], "r");
	for(size_t i = 0; i < sizeof(source); i++) {
		source[i] = fgetc(fp);
		if(source[i] == EOF) {
			source[i] = 0;
			break;
		}
	}

	cell_p ast = print_cell(parse());
	puts("\n--- print_cell ---");
	print_list(ast);
	puts("\n--- print_list ---");
	ast = rewrite_define(ast);
	print_list(ast);
	puts("\n--- rewrite_define ---");
	cell_p ast2 = to_cps(ast, make_lambda(cons(str_to_atom("x"), NULL), cons(str_to_atom("x"), NULL)));
	print_list(ast2);
	puts("\n--- to_cps ---");
	print_cell(eval(ast, NULL));
	puts("\n--- eval ast ---");
	print_cell(eval(ast2, NULL));
	puts("\n--- eval ast2 ---");
}

