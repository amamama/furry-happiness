#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define err(...) (fprintf(stderr, "%s:%d:%s:", __FILE__, __LINE__, __func__), fprintf(stderr,""  __VA_ARGS__))


#include "pl.h"
#include "util.h"
#include "compiler.h"

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

char *lexer_src = NULL;
size_t lexer_index = 0;

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
		char* pos;
		size_t len;
	};
	intptr_t num;
} token;

int init_lexer(char s[65536]) {
	lexer_index = 0;
	lexer_src = s;
	//err("%s\n", lexer_src);
	return 0;
}

token tokenize(bool consume) {
	size_t len = 0;
	token ret = {EOT, {NULL, 0}};
	for(; isspace(lexer_src[lexer_index]); lexer_index++);
	if(lexer_src[lexer_index] == '\0') return ret;
	else if(lexer_src[lexer_index] == '(') ret = (token){RO_BRA, {lexer_src + lexer_index++, len = 1}};
	else if(lexer_src[lexer_index] == ')') ret = (token){RO_KET, {lexer_src + lexer_index++, len = 1}};
	else if(lexer_src[lexer_index] == '.') ret = (token){DOT, {lexer_src + lexer_index++, len = 1}};
	else if(lexer_src[lexer_index] == '\'') ret = (token){QUOTE, {lexer_src + lexer_index++, len = 1}};
	else if((lexer_src[lexer_index] == '-' && isdigit(lexer_src[lexer_index + 1])) || isdigit(lexer_src[lexer_index])) {
		token tok = {NUM, {lexer_src + lexer_index, 0}, 0};
		bool negative = lexer_src[lexer_index] == '-';

		for(lexer_index += negative; isdigit(lexer_src[lexer_index]); tok.num = tok.num * 10 + lexer_src[lexer_index++] - 0x30) tok.len++;

		if(negative) {
			tok.num *= negative?-1:1;
			tok.len++;
			//printf("negative %ld\n", tok.num);
		}
		len = tok.len;
		ret = tok;
	} else {
		token tok = {ID, {lexer_src + lexer_index, 0}};

		for(; isgraph(lexer_src[lexer_index]); lexer_index++, tok.len++) {
			if(lexer_src[lexer_index] == '(') break;
			if(lexer_src[lexer_index] == ')') break;
			if(lexer_src[lexer_index] == '.') break;
		}

		len = tok.len;
		ret = tok;
	}
	if(!consume) lexer_index -= len;
	return ret;
}

// lisp := "'"? lisp | "(" lisp* ("." lisp)? ")" | atom

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
			cell_p root = cons(alloc_cell((cell_p)(tok.pos), (cell_p)(uintptr_t)tok.len, ATOM), NULL);
			cdr(root) = alloc_cell(parse(), NULL, LIST);
			return root;
		} case NUM: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(intptr_t)tok.num, NULL, NUMBER);
		} case ID: {
			tok = tokenize(true);
			return alloc_cell((cell_p)(tok.pos), (cell_p)(uintptr_t)tok.len, ATOM);
		} default: {
				err("parse failed: %*s\n", (int)tok.len, tok.pos);
				assert(false);
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

cell_p print_cell_aux(cell_p);
cell_p print_env(cell_p env) {
	if(!env) return NULL;
	print_rec(car(env), "", cell_aux, " -> ", cell_aux, ";\n");
	return print_env(cdr(env));
}


cell_p visited[65536];
size_t idx = 0;

cell_p print_frame(cell_p frame) {
	if(!frame) return NULL;

	for(size_t i = 0; i <= idx; i++) {
		if(visited[i] == frame) return printf("[%p]", frame), frame;
	}
	visited[idx++] = frame;
	assert(idx < 65536);
	printf("%p : ", frame);

	printf("<");
	print_env(car(frame));
	printf(">\n");
	return print_frame(cdr(frame));
}

cell_p print_cell_aux(cell_p root) {
	//if(!root) return printf("🈚"), root;
	if(!root) return printf("🈳"), root;
	for(size_t i = 0; i <= idx; i++) {
		if(visited[i] == root) return printf("[%p]", root), root;
	}

	switch(cty(root)) {
		case LIST: {
			visited[idx++] = root;
			assert(idx < 65536);
			return printf("%p : ", root), print_rec(root, "(", cell_aux, ", ", cell_aux, ")");
		} case ATOM: {
			printf("%.*s", (int)(uintptr_t)cdr(root), (char*)car(root));
			break;
		} case NUMBER: {
			printf("%ld", (intptr_t)car(root));
			break;
		} case FUNC: {
			return print_rec(root, "{", cell_aux, "; ", frame, "}\n");
		} default: {
			assert(false);
		}
	}
	return root;
}

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
				if(!is(LIST, c)) {
					printf(". ");
					print_list(c);
					break;
				}
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
		} default: {
			err("ATOM, NUMBER, LIST igai ha denai hazu");
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

cell_p make_new_env(cell_p arg_decl, cell_p evaled_args) {
	cell_p env = NULL;
	if(!is_dotted_list(arg_decl) && length(arg_decl) != length(evaled_args)) {
		puts("=========");
		print_list(arg_decl);
		puts("\n:::::::::");
		print_list(evaled_args);
		puts("\n=======");
		err("hikisuu no kazu ga okasii\n");
	}
	for(; arg_decl && is(LIST, arg_decl); ) {
		assert(is(ATOM, car(arg_decl)));
		env = cons(cons(car(arg_decl), car(evaled_args)), env);
		arg_decl = cdr(arg_decl);
		evaled_args = cdr(evaled_args);
	}
	if(is(ATOM, arg_decl)) return cons(cons(arg_decl, evaled_args), env);
	return env;
}

cell_p make_new_frame(cell_p func, cell_p evaled_args, cell_p frame) {
	cell_p lambda = car(func);
	cell_p lambda_frame = cdr(func);
	cell_p env = make_new_env(car_cdnr(lambda, 1), evaled_args);
	return cons(env, lambda_frame);
}

cell_p eval_body(cell_p lambda, cell_p frame) {
	cell_p body = cdr(cdr(lambda));
	for(; cdr(body); body = cdr(body)) {
		eval(car(body), frame);
	}
	return eval(car(body), frame);
}


// for compiler.c
cell_p move_arg(cell_p env, size_t depth) {
	if(depth == 1) {
		cdr(env) = cons(cdr(env), NULL);
		return env;
	}
	return move_arg(cdr(env), depth - 1);
}

cell_p apply(cell_p func, cell_p args, cell_p frame) {
	assert(is(FUNC, func));
	cell_p evaled_args = eval_args(args, frame);
	cell_p new_frame = make_new_frame(func, evaled_args, frame);
	return eval_body(car(func), cons(NULL, new_frame));
}

cell_p apply_closure(cell_p closure, cell_p args, cell_p frame) {
	assert(is(CLO, closure));
	cell_p lambda = car(closure);
	cell_p env = cdr(closure);
	cell_p evaled_args = eval_args(args, frame);
	cell_p new_env = make_new_env(car_cdnr(lambda, 1), cons(env, evaled_args));
	return eval_body(lambda, cons(new_env, cdr(frame)));
}

#define keyword(s, t, n, exp) def_func(t, n, exp)
#define predefined(s, t, n, exp) def_func(t, n, exp)
#define begin(k, K)
#define end(k, K)
#define def_func(t, n, exp) \
	cell_p prim_##t(cell_p root, cell_p frame) { \
		if(n != 0 && !(length(root) == n + 1)) err("hikisuu ga okasii\n"), print_list(root), puts(""); \
		A(n); \
		return exp; \
	}
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

#define keyword(s, t, n, exp) def_func(t, s)
#define predefined(s, t, n, exp) def_func(t, s)
#define begin(k, K)
#define end(k, K)
#define def_func(t, s) bool is_##t(cell_p root) { return is(LIST, root) && is_same_string(s, car(root)); }
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef def_func

#define keyword(s, t, n, exp) to_funcname(t)
#define predefined(s, t, n, exp) to_funcname(t)
#define begin(k, K) bool (*is_##k[])(cell_p) = {
#define end(k, K) };
#define to_funcname(t) is_##t,
#include "def.h"
#undef keyword
#undef predefined
#undef begin
#undef end
#undef to_funcname

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
				if(is_keyword[i](root)) return keyword_funcs[i](root, frame);
			}
			cell_p evaled_car = eval(car(root), frame);
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_same_string(predefined[i], evaled_car)) return predefined_funcs[i](root, frame);
			}
			if(!is(FUNC, evaled_car) && !is(CLO, evaled_car)) {
				puts("=========");
				print_list(root);
				puts("\n======== evaled_car:");
				print_list(root);
				puts("\n------");
			}
			assert(is(FUNC, evaled_car) || is(CLO, evaled_car));
			if(is(FUNC, evaled_car))
				return apply(evaled_car, cdr(root), frame);
			if(is(CLO, evaled_car))
				return apply_closure(evaled_car, cdr(root), frame);
		} default: {
			assert(false);
		}
	}
}

bool is_value(cell_p root) {
	return is(ATOM, root)
		|| is(NUMBER, root)
		|| is_lambda(root)
		|| is_q(root)
		|| is_quote(root);
}

/*
bool is_member(cell_p atom, cell_p list) {
	if(!list) return false;
	if(is_same_atom(atom, car(list))) return true;
	return is_member(atom, cdr(list));
}

cell_p to_set(cell_p list) {
	if(!list) return NULL;
	cell_p ret = to_set(cdr(list));
	return is_member(car(list), ret) ? ret : cons(car(list), ret);
}

cell_p union_list(cell_p a, cell_p b) {
	return to_set(append(a, b));
}
*/


int print_list_with_ln(cell_p l) {
	if(!l) return 0;
	print_list(car(l));
	puts("");
	return print_list_with_ln(cdr(l));
}

int main(int argc, char **argv) {
	FILE *fp = argv[1][0] == '-'?stdin:fopen(argv[1], "r");
	char source[65536] = "";
	for(size_t i = 0; i < sizeof(source); i++) {
		source[i] = fgetc(fp);
		if(source[i] == EOF) {
			source[i] = 0;
			break;
		}
	}
	init_lexer(source);
	cell_p body = parse_body(); // body
	cell_p defs = read("((define hoge 'fuga) (define foo 'bar))");
	cell_p ast = make_lambda(NULL, append(defs, body)); // (lambda () predefined body)
	print_list(copy(ast, -1));
	puts("\n--- print_list ast ---");
	//print_list(eval(cons(ast, NULL), NULL));
	//puts("\n--- eval ast ---");
	cell_p ast1 = rewrite_define(ast, NULL);
	// (lambda () ((lambda (predefined ...) (set! ) ... body) '() ...))
	ast1 = car_cdnr(ast1, 2);
	// ((lambda (predefined ...) (set! ) ... body) '() ...)
	print_list(ast1);
	puts("\n--- rewrite_define ast ---");
	//print_list(eval(ast1, NULL));
	//puts("\n--- eval ast ---");
	cell_p ast2 = to_cps(ast1, read("(lambda (x) x))"));
	// ((lambda (k) ...) (lambda (k) ...))
	// ここでcall/ccの定義を突っ込む
	ast2 = app2(make_lambda(cons(str_to_atom("call-with-current-continuation"), NULL), cons(ast2, NULL)), read("(lambda (k f) (f k (lambda (_ v) (k v))))"));
	// ((lambda (call/cc) ( ↑)) (lambda (k f) (f k (lambda (_ v) (k v)))))
	print_list(ast2);
	puts("\n--- to_cps ast ---");
	print_list(eval(ast2, NULL));
	puts("\n--- eval ast ---");
	cell_p ast3 = to_closure(ast2, NULL);
	// ((cons (lambda (k) ...) env) (cons (lambda (k) ...) env))
	print_list(ast3);
	puts("\n--- to_closure ast ---");
	cell_p global = cons(cons(cons(str_to_atom("環境"), str_to_atom("global_env")), NULL), NULL);
	print_list(eval(ast3, global));
	puts("\n--- eval ast ---");
	cell_p body1 = cons(ast3, NULL);
	//(body)
	body1 = rewrite_lambda(body1);
	//((define λ1 ...) (define ...) body)
	print_list(body1);
	puts("\n--- rewrite_lambda body1 ---");
	cell_p ast4 = cons(make_lambda(NULL, body1), NULL);
	print_list(eval(ast4, global));
	puts("\n--- eval ast4 ---");
	cell_p body2 = hoist_lambda(body1);
	//((define λ1 ...) (define ...) body)
	print_list_with_ln(body2);
	puts("\n--- hoist_lambda body2 ---");
	cell_p ast5 = make_lambda(NULL, body2);
	//(lambda () (define ...) (define ...) body)
	ast5 = rewrite_define(ast5, NULL);
	//(lambda () ((lambda (λ1 ...) (set! ) ... body) ...))
	ast5 = car_cdnr(ast5, 2);
	//((lambda (λ1 ...) (set! ) ... body) ...)
	print_list(ast5);
	puts("\n--- rewrite_define ast5 ---");
	//cell_p ast6 = cons(ast5, NULL);
	//print_list(eval(ast6, global));
	print_list(eval(ast5, global));
	puts("\n--- eval ast5 ---");
	/*
	cell_p global_var = read("(global hennsuu no ichiran)");
	cell_p global_val = read("('global 'hennsuu 'no 'nakami)");
	cell_p ast = print_cell(make_lambda(global_var, copy(body, -1)));
	puts("\n--- print_cell ast ---");
	print_list(ast);
	puts("\n--- print_list ast ---");
	print_list(eval(cons(ast, global_val), NULL));
	puts("\n--- eval ast ---");
	cell_p ast1 = copy(body, -1);
	ast1 = rewrite_define(make_lambda(global_var, ast1), NULL);
	print_list(ast1);
	puts("\n--- rewrite_define ast1 ---");
	ast1 = to_closure(ast1, NULL);
	//ast1 = car_cdnr(ast1, 1);
	print_list(ast1);
	puts("\n--- to_closure ast1 ---");
	cell_p body1 = cons(cons(ast1, global_val), NULL);
	body1 = rewrite_lambda(body1);
	print_list(body1);
	puts("\n--- rewrite_lambda body1 ---");
	ast1 = make_lambda(NULL, body1);
	print_list(ast1);
	puts("\n--- print_list ast1 ---");
	cell_p global = cons(cons(cons(str_to_atom("環境"), NULL), NULL), NULL);
	print_list(eval(cons(ast1, NULL), global));
	puts("\n--- eval ast1 ---");
	ast = rewrite_define(ast, NULL);
	print_list(ast);
	puts("\n--- rewrite_define ast ---");
	print_list(eval(cons(ast, global_val), NULL));
	puts("\n--- eval rewrite_define(ast) ---");
	cell_p k = make_lambda(read("(k)"), cons(cons(str_to_atom("k"), cons(read("(lambda (x) x)"), global_val)), NULL));
	cell_p ast2 = to_cps(ast, k);
	print_list(ast2);
	puts("\n--- to_cps ast2 ---");
	print_list(eval(ast2, NULL));
	puts("\n--- eval ast2 ---");
	*/
}

