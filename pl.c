#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define err(...) (fprintf(stderr, "%s:%d:%s:", __FILE__, __LINE__, __func__), fprintf(stderr,""  __VA_ARGS__))


#include "pl.h"
#include "util.h"
#include "cps.h"

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
	if(!is_dotted_list(arg_decl) && length(arg_decl) != length(args)) {
		puts("=========");
		print_list(args);
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
#define def_func(t, n, exp) \
	cell_p prim_##t(cell_p root, cell_p frame) { \
		if(n != 0 && !(length(root) > n)) err("hikisuu ga tarinai\n"), print_list(root), puts(""); \
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
				if(is_keyword[i](root)) return keyword_funcs[i](root, frame);
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

// closure 変換のため，
// ((lambda args body) e1 ... en) すなわちlambda式が出現している部分式を
// →(define clo_n (lambda args body)) (clo_n e1 ...  en) そのlambda式が出現している環境でdefineし直して，clo_nで参照する
// ように変更する
// このとき，((lambda (_ f) (f)) (if 0 (define a 1) (define a 2)) (lambda (x) a)) が正しく動くことが望ましい
// ただしく動けば1が帰る
// 現状↑のケースはrewrite_defineで死ぬがdefineはlambdaのbody直下にしか現れないと仮定して良い

bool is_value(cell_p root) {
	return is(ATOM, root)
		|| is(NUMBER, root)
		|| is_lambda(root)
		|| is_q(root)
		|| is_quote(root);
}

genvar(clo, "λ")

cell_p rewrite_lambda_body(cell_p);
cell_p rewrite_lambda_aux(cell_p);
cell_p rewrite_lambda_list(cell_p root) {
	if(!root || !is(LIST, root)) return cons(root, NULL);
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
				if(is_keyword[i](root)) {
					switch(i) {
						case K_lambda: {
							cell_p args = car_cdnr(root, 1);
							cell_p body = cdr(cdr(root));
							cell_p new_body = rewrite_lambda_body(body);
							cell_p var = genvar_clo();
							cell_p new_define = app3(str_to_atom("define"), var, make_lambda(args, new_body));
							return cons(var, cons(new_define, NULL));
						}
						case K_set:
						case K_define: {
							cell_p exp = car_cdnr(root, 2);
							cell_p new_exp = NULL;
							if(is_lambda(exp)) {
								cell_p args = car_cdnr(exp, 1);
								cell_p body = cdr(cdr(exp));
								cell_p new_body = rewrite_lambda_body(body);
								new_exp = cons(make_lambda(args, new_body), NULL);
							} else {
								new_exp = rewrite_lambda_aux(exp);
							}
							return cons(app3(car(root), car_cdnr(root, 1), car(new_exp)), cdr(new_exp));
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

cell_p rewrite_lambda_body(cell_p bodies) {
	if(!bodies) return NULL;
	cell_p new_bodies = rewrite_lambda_body(cdr(bodies));
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

/* 
 (define fact (lambda (n) (if (eq n 0) 1 (_mul n (fact (_sub n 1))))))
 (define fact3 (fact 3))
 (fact fact3)

 (define fact (lambda (self n) (if (eq n 0) 1 (_mul n (clo-ref (clo-ref self 1) 0) (clo-ref self 1) (_sub n 1)))))
 (define fact3 ((clo-ref '(fact '(fact '(fact ...))) '(fact '(fact '(fact ...))) 0) 3))
 ((clo-ref '(fact '(fact '(fact ...))) 0) fact3)
*/
// 再帰して死ぬ
// アイディアとしては，関数を展開していく際にどの名前を使用したかを覚えておいて，循環させるようにするとか？ヤバそう．生成後のコードがグラフになるのでprintできなくなる
// 生成後のコードがグラフになる問題，closureに保存されているlistが循環するだけなので問題ないっぽい？わからん

/*
(define o '())
(define e (lambda (n) (if (eq n 0) 0 (o (_sub n 1)))))
(set! o (lambda (n) (if (eq n 1) 1 (e (_sub n 1)))))
(o 4)

(define o '())
(define e (lambda (self n) (if (eq n 0) 0 ((clo-ref (clo-ref self 1) 0) (clo-ref self 1) (_sub n 1)))))
(set! o (lambda (n) (if (eq n 1) 1 ((clo-ref (clo-ref self 1) 0) (clo-ref self 1) (_sub n 1)))))
((clo-ref '(o '(e '(o '(...)))) 0) '(o '(e '(o '(...)))) 4)

*/
// こんなのも「その場」じゃないと動かないけど再帰で死ぬ
/*
(define make-counter
 	(lambda (n)
		(define count (_sub n 1))
		(define λ0 (lambda () (set! count (_add count 1))))
		λ0))
(define counter3 (make-counter 3))
(define counter5 (make-counter 7))
(cons (counter3) (cons (counter3) (cons (counter5) (cons (counter5) (' ())))))
*/
// のようなコード（外側の環境の値を書き換えるようなコード）も死ぬ
// (set! count ...)を変換するが，変換後が変数にならない(clo-ref self なんか)になるため
// アイディアとして現在set!は変数への代入に限定されているため，set-car!で頑張って計算するとか？ ← 採用
// アイディアとして現在set!は変数への代入に限定されているが，これを式に対しても動くようにするとか？参照渡しが基本であるlispに於いては弊害のほうが多そうだけど.set-car!，set-cdr!がset!を用いて実装できるようになるね
//
// 「その場」戦略を採用するため，update-closureという関数を作る
// ナイーブに展開すると再帰して死ぬので，まず関数定義時にclosureのskeltonを作成し，update-closureで正しい値をskeltonに代入しつつ，コピーするという戦略を取る．
// これにより，展開先のclosureが更新されつつ，毎回呼び出されるときに新しい環境を作ることができる
// 本当に動くのかはわからない
// なぜ動くと思ったか
// set-car!での代入は式に対して行え，その式は今closureである．
// このインタプリタは変数にatomないしlistを束縛するが，closureは後者
// 名前で引かれるclosureのポインタは変化しない．
// 相互再帰するe,oという関数↑参照があったとき，最初（定義時）のe,oのclosureはどちらも不完全（eのlambda式に出てくるoはnilだし，oのlambda式に出てくるeの環境の中に入っているoは現状nilなので）
// しかし，呼び出されるときにupdate-closureで当該箇所を更新すると，set-car!によってlistが更新される．
// このとき，定義時のclosureがupdateされるが，定義時のclosureのポインタそのものは変化しない（重要）
// eをupdateするときには，「定義時のeのclosureの参照を持ったoのclosure」が再代入されるが，
// 「定義時のeのclosure」はset-car!によって更新されており，実際に呼び出されるときにはコピーされたものが使用され，
// oのclosureは今完全になっているので問題ない．
// でもこれoだけをupdateしても「eのclosure」は定義時のままだから死ぬわこれ
// o自体をupdateしてもその変更が伝わらないと意味がない
//
// やっぱりクロージャ専用の機構を導入するしかないのでは？多分一番簡単に書ける
//
// いや，なんとかなる気がする
// (lambda env body)って書くと実引数リストがenvに束縛されることを利用する
// 実引数がv1 …vnの形をしているとき，env v1 …vnの形にする
// すると，(car env)に環境，(cdr env)に局所変数リストが入るが，envのアドレス自体は変わらない
// 変数にset!で代入していたやつはset-car!を使う（実際は何かの関数でラップする）
// そして，(lambda …) の出現を(cons (lambda …) env) とすればよい．
// bodyに出現する束縛変数の値は，envのn番目のcarを用いる．
// set!の書き換え先として出現する束縛変数は，envのn番目を用い，set-car!とする．
// 同様に，bodyに出現する自由変数は，その変数がどのフレームで束縛されているかを調べ，そのレベルだけ(car env)とする
// envのアドレス自体は変わらないので，set-car!して更新すれば更新後の環境がペアになる
// このとき，
// (e1 e2 …en)は((car e1) (cdr e1) e2 … en)とする
// 多分，↑の問題全てが解決する．
// そのために結局rewrite_defineが必要．defineはクソ．
// ↑これは嘘で，rewrite_defineするとまたlambdaが出てきてしまうので，defineを発見したら（defineの列の後に式の列を仮定），最初にenvのcdrをnilのリストで書き換えてset!する．
// すなわち局所変数のための記憶領域を確保して代入する作業をする．
// 確認のため上記の例を全て手書きで書き換える．
// 効率を考えてdefineを平たくするようにrewrite_defineを書き換えたほうがいい
// 動いたので実装する
/*
((lambda ()
 (define fact (lambda (n) (if (eq n 0) 1 (_mul n (fact (_sub n 1))))))
 (define fact3 (fact 3))
 (fact fact3)
))

(define cdnr (lambda (l n) (if (eq n 0) l (cdnr (cdr l) (_sub n 1)))))
(define car-cdnr (lambda (l n) (if (eq n 0) (car l) (car-cdnr (cdr l) (_sub n 1)))))
(define l (lambda env
(set-car! (cdnr env 1) (cons (lambda env (if (eq (car-cdnr env 1) 0) 1 (_mul (car-cdnr env 1) ((car (car-cdnr (car env) 1)) (cdr (car-cdnr (car env) 1)) (_sub (car-cdnr env 1) 1))))) env))
(set-car! (cdnr env 2) ((car (car-cdnr env 1)) (cdr (car-cdnr env 1)) 3))
((car (car-cdnr env 1)) (cdr (car-cdnr env 1)) (car-cdnr env 2))))
(car-cdnr '(0 1 2 3 4) 0)
(l 'env '() '())


(define odd '())
(define even (lambda (n) (if (eq n 0) 0 (odd (_sub n 1)))))
(set! odd (lambda (n) (if (eq n 0) 1 (even (_sub n 1)))))
(odd 101)

(define cdnr (lambda (l n) (if (eq n 0) l (cdnr (cdr l) (_sub n 1)))))
(define car-cdnr (lambda (l n) (if (eq n 0) (car l) (car-cdnr (cdr l) (_sub n 1)))))
(define l (lambda env
	(set-car! (cdnr env 1) (cons (lambda env (if (eq (car-cdnr env 1) 0) 0 ((car (car-cdnr (car env) 2)) (cdr (car-cdnr (car env) 2)) (_sub (car-cdnr env 1) 1)))) env))
	(set-car! (cdnr env 2) (cons (lambda env (if (eq (car-cdnr env 1) 0) 1 ((car (car-cdnr (car env) 1)) (cdr (car-cdnr (car env) 1)) (_sub (car-cdnr env 1) 1)))) env))
	((car (car-cdnr env 2)) (cdr (car-cdnr env 2)) 101)))
(l 'env '() '())


(define make-counter (lambda (n) (define count (_sub n 1)) (cons (lambda () (set! count (_add count 1))) (lambda () (set! count 0)))))
(define counter3 (make-counter 3))
(define counter5 (make-counter 7))
(cons ((car counter3)) (cons ((car counter5)) (cons ((cdr counter3)) (cons ((car counter3)) (cons ((car counter5)) '())))))

(define cdnr (lambda (l n) (if (eq n 0) l (cdnr (cdr l) (_sub n 1)))))
(define car-cdnr (lambda (l n) (if (eq n 0) (car l) (car-cdnr (cdr l) (_sub n 1)))))
(define l (lambda env
	(set-car! (cdnr env 1)
		(cons (lambda env
			(set-car! (cdnr env 2) (_sub (car-cdnr env 1) 1))
			(set-car! (cdnr env 3) (cons (lambda env (set-car! (cdnr (car env) 2) (_add (car-cdnr (car env) 2) 1))) env))
			(set-car! (cdnr env 4) (cons (lambda env (set-car! (cdnr (car env) 2) 0)) env))
			(cons (car-cdnr env 3) (car-cdnr env 4))) env))
	(set-car! (cdnr env 2) ((car (car-cdnr env 1)) (cdr (car-cdnr env 1)) 3 '() '() '()))
	(set-car! (cdnr env 3) ((car (car-cdnr env 1)) (cdr (car-cdnr env 1)) 7 '() '() '()))
	(cons ((car (car (car-cdnr env 2))) (cdr (car (car-cdnr env 2))))
	      (cons ((car (car (car-cdnr env 3))) (cdr (car (car-cdnr env 3))))
	            (cons ((car (cdr (car-cdnr env 2))) (cdr (cdr (car-cdnr env 2))))
	                  (cons ((car (car (car-cdnr env 2))) (cdr (car (car-cdnr env 2))))
	                        (cons ((car (car (car-cdnr env 3))) (cdr (car (car-cdnr env 3))))
	                              '())))))))

(l 'env '() '() '())

*/
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

// TODO: 関数の場所を考える．公開，非公開やファイルなど．
cell_p lambda_to_closure(cell_p lambda, cell_p frame) {
}

cell_p to_closure(cell_p root) {
	if(!root) return NULL;
	switch(cty(root)) {
		case ATOM: {
		}
		case NUMBER: {
		}
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_predefined[i](root)) {
				}
			}
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

	cell_p global_frame = cons(make_new_env(app4(str_to_atom("a0"), str_to_atom("a1"), str_to_atom("a2"), str_to_atom("a3")), app4(int_to_atom(1), int_to_atom(10), int_to_atom(100), int_to_atom(1000)), NULL), NULL);

	cell_p body = parse_body();
	cell_p ast = print_cell(make_lambda(NULL, body));
	puts("\n--- print_cell ast ---");
	print_list(ast);
	puts("\n--- print_list ast ---");
	//print_list(collect_free_vars(ast));
	//puts("\n--- print_list collect_free_vars(ast) ---");
	print_cell(eval(cons(ast, NULL), global_frame));
	puts("\n--- eval ast ---");
	cell_p copied_ast = make_lambda(NULL, rewrite_lambda_body(copy(body, -1)));
	print_list(copied_ast);
	puts("\n--- rewrite_lambda ---");
	print_cell(eval(cons(copied_ast, NULL), global_frame));
	puts("\n--- eval copied_body ---");
	ast = rewrite_define(ast, NULL);
	print_list(ast);
	puts("\n--- rewrite_define ast ---");
	print_cell(eval(cons(ast, NULL), global_frame));
	puts("\n--- eval rewrite_define(ast) ---");
	cell_p ast2 = to_cps(ast, make_lambda(cons(str_to_atom("x"), NULL), cons(app2(str_to_atom("x"), make_lambda(cons(str_to_atom("x"), NULL), cons(str_to_atom("x"), NULL))), NULL)));
	print_list(ast2);
	puts("\n--- to_cps ast2 ---");
	print_cell(eval(ast2, global_frame));
	puts("\n--- eval ast2 ---");
}

