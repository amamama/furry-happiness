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
#include "cps.h"

cell_p alloc_cell(cell_p car, cell_p cdr, cell_type t) {
	cell_p ret = malloc(sizeof(cell));
	ret->car = car;
	ret->cdr = cdr;
	return to(t, ret);
}

cell_p copy_cell(cell_p root, int depth) {
	if(!root) return root;
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			if(depth == 0) return root;
			return alloc_cell(car(root), cdr(root), cty(root));
		}
		case LIST: {
			if(depth == 0) return root;
			return alloc_cell(copy_cell(car(root), depth > 0?depth - 1:depth), copy_cell(cdr(root), depth), LIST);
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

cell_p parse_body(void) {
	cell_p ret = NULL;
	token tok = tokenize(false);
	if(tok.type == EOT) return ret;
	cell_p body = parse();
	ret = cons(body, parse_body());
	return ret;
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
	assert(idx < 65536);
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
			if(!is(FUNC, evaled_car)) exit((print_list(car(root)), puts("\nexit")));
			assert(is(FUNC, evaled_car));
			return apply(evaled_car, cdr(root), frame);
		} default: {
			assert(false);
		}
	}
}

// closure 変換のため，
// ((lambda (args) body) e1 ... en) すなわちlambda式が出現している部分式を
// →(define clo_n (lambda (args) body)) (clo_n e1 ...  en) そのlambda式が出現している環境でdefineし直して，clo_nで参照する
// ように変更する
// このとき，((lambda (_ f) (f)) (if 0 (define a 1) (define a 2)) (lambda (x) a)) が正しく動くことが望ましい
// ただしく動けば1が帰る
// 現状↑のケースはrewrite_defineで死ぬがdefineはlambdaのbody直下にしか現れないと仮定して良い

genvar(clo, "λ")

cell_p rewrite_lambda(cell_p);
cell_p rewrite_lambda_aux(cell_p);
cell_p rewrite_lambda_list(cell_p root) {
	if(!root) return cons(NULL, NULL);
	cell_p new_list = rewrite_lambda_list(cdr(root));
	cell_p new_car = rewrite_lambda_aux(car(root));
	return cons(cons(car(new_car), car(new_list)), cdr(new_car)?append(cdr(new_car), cdr(new_list)):cdr(new_list));
	
}

cell_p rewrite_lambda_aux(cell_p root) {
	if(!root) return cons(root, NULL);
	switch(cty(root)) {
		case ATOM:
		case NUMBER:
		return cons(root, NULL);
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_same_string(keyword[i], car(root))) {
					switch(i) {
						case K_lambda: {
							cell_p args = car_cdnr(root, 1);
							cell_p body = cdr(cdr(root));
							cell_p new_body = rewrite_lambda(body);
							cell_p var = genvar_clo();
							cell_p new_define = app3(str_to_atom("define"), var, make_lambda(args, new_body));
							return cons(var, cons(new_define, NULL));
						}
						case K_set:
						case K_define: {
							cell_p exp = car_cdnr(root, 2);
							if(is(LIST, exp) && is_same_string("lambda", car(exp))) return cons(root, NULL);
							cell_p new_exp = rewrite_lambda_aux(exp);
							return cons(app3(str_to_atom("define"), car_cdnr(root, 1), car(new_exp)), cdr(new_exp));
						}
					}
				}
			}
			return rewrite_lambda_list(root);
		} default: {
			assert(false);
		}
	}
}

cell_p rewrite_lambda(cell_p bodies) {
	if(!bodies) return NULL;
	cell_p new_bodies = rewrite_lambda(cdr(bodies));
	//puts("before ---");
	//print_list(car(bodies));
	//puts("\nafter ---");
	cell_p new_body = rewrite_lambda_aux(car(bodies));
	//print_list(car(new_body));
	//puts("\nend ---");
	cell_p ret = cdr(new_body)?append(cdr(new_body), cons(car(new_body), new_bodies)):cons(car(new_body), new_bodies);
	//puts("========");
	//print_list(ret);
	//puts("\n========");
	return ret;
}

// closure 変換をする
// (lambda () (define a 0) ((lambda (_ f) (f)) (if 0 (set! a 1) (set! a 2)) (lambda () a))) が動かなくなる？
// 今まではaは外部環境のaを参照しに行っていたため，set!で代入した後を正しく参照できていたが，変換を行うと値渡しでlistを作成するので，set!で代入してもクロージャのリストの方にはその変更は伝わらない
// 回避するには「その場で」listを作成する必要がある．→多くの場面でリストが2回出てきてしまう


cell_p substitute_closure(cell_p atom, cell_p sub_list) {
	return get_from_env(atom, sub_list);
}

bool is_free_vars(cell_p atom, cell_p free_vars) {
	if(!free_vars) return false;
	if(is_same_atom(atom, car(free_vars))) return true;
	return is_free_vars(atom, cdr(free_vars));
}

bool index_of_vars(cell_p atom, cell_p free_vars) {
	assert(free_vars);
	if(is_same_atom(atom, car(free_vars))) return 0;
	return 1 + index_of_vars(atom, cdr(free_vars));
}

cell_p collect_free_vars(cell_p lambda, cell_p free_vars) {
	cell_p args = car_cdnr(lambda, 1);
}

cell_p lambda_to_closureless(cell_p root, cell_p free_vars, cell_p clo_subs) {
}

cell_p to_closureless(cell_p root, cell_p free_vars, cell_p clo_subs) {
	if(!root) return root;
	switch(cty(root)) {
		case ATOM: {
			cell_p sub = substitute_closure(root, clo_subs);
			if(sub) return to_closureless(cdr(sub), free_vars, clo_subs);
			if(!is_free_vars(root, free_vars)) return root;
			return app3(str_to_atom("clo-ref"), str_to_atom("self"), int_to_atom(index_of_vars(root, free_vars)));
		}
		case NUMBER: {
			return root;
		}
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_same_string(keyword[i], car(root))) {
					switch(i) {
						case K_q:
						case K_quote:
						return root;
						case K_if: {
							cell_p cond = car_cdnr(root, 1);
							cell_p t = car_cdnr(root, 2);
							cell_p f = car_cdnr(root, 3);
							cell_p new_cond = to_closureless(cond, free_vars, clo_subs);
							cell_p new_t = to_closureless(t, free_vars, clo_subs);
							cell_p new_f = to_closureless(f, free_vars, clo_subs);
							cell_p new_if = app4(str_to_atom("if"), new_cond, new_t, new_f);
							return new_if;

						}
						case K_lambda: {
							//this case should not be appeared
							assert(false);
						}
						case K_define: {
							cell_p exp = car_cdnr(root, 2);
							if(!is(LIST, exp) || !is_same_string("lambda", car(exp))) return app3(str_to_atom("define"), car_cdnr(root, 1), to_closureless(exp, free_vars, clo_subs));
							cell_p used_free_vars = collect_free_vars(root, free_vars);
							return lambda_to_closureless(root, used_free_vars, clo_subs);
						}
						case K_set: {
						}
						default: {
							assert(false);
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_same_string(predefined[i], car(root))) {
					for(cell_p c = cdr(root); c; c = cdr(c)) {
						car(c) = to_closureless(car(c), free_vars, clo_subs);
					}
					return root;
				}
			}
			car(root) = app3(str_to_atom("clo-ref"), to_closureless(car(root), free_vars, clo_subs), int_to_atom(0));
			for(cell_p c = cdr(root); c; c = cdr(c)) {
				car(c) = to_closureless(car(c), free_vars, clo_subs);
			}
			return root;
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

	cell_p body = parse_body();
	cell_p ast = print_cell(make_lambda(NULL, body));
	puts("\n--- print_cell ast ---");
	print_list(ast);
	puts("\n--- print_list ast ---");
	print_list(eval(app2(ast, nil), NULL));
	puts("\n--- eval ast ---");
	cell_p copied_ast = make_lambda(NULL, rewrite_lambda(copy_cell(body, -1)));
	print_list(copied_ast);
	puts("\n--- rewrite_lambda ---");
	print_cell(eval(app2(copied_ast, nil), NULL));
	puts("\n--- eval copied_body ---");
	ast = rewrite_define(ast);
	print_list(ast);
	puts("\n--- rewrite_define ast ---");
	print_cell(eval(app2(ast, nil), NULL));
	puts("\n--- eval ast ---");
	cell_p ast2 = to_cps(app2(ast, nil), make_lambda(cons(str_to_atom("x"), NULL), cons(str_to_atom("x"), NULL)));
	print_list(ast2);
	puts("\n--- to_cps ast2 ---");
	print_cell(eval(ast2, NULL));
	puts("\n--- eval ast2 ---");
}

