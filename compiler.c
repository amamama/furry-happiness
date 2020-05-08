#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "pl.h"
#include "util.h"
#include "compiler.h"

// closure 変換のため，
// ((lambda args body) e1 ... en) すなわちlambda式が出現している部分式を
// →(define clo_n (lambda args body)) (clo_n e1 ...  en) そのlambda式が出現している環境でdefineし直して，clo_nで参照する
// ように変更する
// このとき，((lambda (_ f) (f)) (if 0 (define a 1) (define a 2)) (lambda (x) a)) が正しく動くことが望ましい
// ただしく動けば1が帰る
// 現状↑のケースはrewrite_defineで死ぬがdefineはlambdaのbody直下にしか現れないと仮定して良い

genvar(clo, "λ")

cell_p rewrite_lambda_aux(cell_p);
cell_p rewrite_lambda_list(cell_p root) {
	if(!root || !is(LIST, root)) return cons(root, NULL);
	cell_p new_list = rewrite_lambda_list(cdr(root));
	cell_p new_car = rewrite_lambda_aux(car(root));
	return cons(cons(car(new_car), car(new_list)), cdr(new_car)?append(cdr(new_car), cdr(new_list)):cdr(new_list));
}

//return cons(変換後の式，defineのリスト)
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
	cell_p define_begin = NULL;
	cell_p ret = NULL;
	for(; bodies; bodies = cdr(bodies)) {
		cell_p new_body_and_define = rewrite_lambda_aux(car(bodies));
		cell_p new_body = car(new_body_and_define);
		cell_p define_list = cdr(new_body_and_define);
		define_begin = append(define_begin, define_list);
		ret = append(ret, cons(new_body, NULL)); //TODO: cons，reverseを使うやつに書き換え
	}
	return append(define_begin, ret);
}

// cps変換のため，
// (lambda (args) (define v_1 e_1) ... (define v_n e_n) post_bodies)
// -> (lambda (args) (rewrite_define(<<<(lambda (v_1 ... v_n ) (set! v_1 e_1) (set! v_n e_n) post_bodies)>>>) '() ... '()))
// のようにする．
// これによって，ナイーブな前方参照や相互再帰ができなくなる．
/*
((lambda ()
	(define add5 (lambda (x) (_add (id 5) x)))
	(define id (lambda (x) x))
	(add5 10)
	))
*/
// 手動でset!すると動くのでこれで上記の問題はこれでなんとかする
// あとでマクロを作ることができればそれで解決する
/* ((lambda (id)
	(define add5 (lambda (x) (_add (id 5) x)))
	(define id (lambda (x) x))
	(add5 10)
	) (lambda (x) (_add x 3)))
*/

size_t index_of_atom(cell_p atom, cell_p list) {
	assert(list);
	if(is_same_atom(atom, car(list))) return 0;
	return 1 + index_of_atom(atom, cdr(list));
}

bool is_args(cell_p atom, cell_p args) {
	if(!args) return false;
	if(is(ATOM, args)) return is_same_atom(atom, args);
	if(is_same_atom(atom, car(args))) return true;
	return is_args(atom, cdr(args));
}

bool is_bound(cell_p atom, cell_p frame) {
	if(!frame) return false;
	if(is_args(atom, car(frame))) return true;
	return is_bound(atom, cdr(frame));
}

cell_p make_set_exp_list(cell_p begin, cell_p end) {
	if(begin == end) return cons(NULL, NULL);
	cell_p ret = make_set_exp_list(cdr(begin), end);
	cell_p var = car_cdnr(car(begin), 1);
	cell_p exp = car_cdnr(car(begin), 2);
	cell_p set_exp = app3(str_to_atom("set!"), var, exp);
	return cons(cons(var, car(ret)), cons(set_exp, cdr(ret)));
}

cell_p make_nil_list(cell_p set_exp_list, cell_p frame) {
	if(!set_exp_list) return NULL;
	cell_p var = car(set_exp_list);
	return cons(is_bound(var, frame)?var:nil, make_nil_list(cdr(set_exp_list), frame));
}
cell_p formal_args_to_list(cell_p args) {
	if(!args) return NULL;
	if(is(ATOM, args)) return cons(args, NULL);
	return cons(car(args), formal_args_to_list(cdr(args)));
}

cell_p destruct_lambda(cell_p root) {
	assert(is_lambda(root));
	cell_p args = car_cdnr(root, 1);
	cell_p body = cdr(cdr(root));
	cell_p define_begin = body;
	for(; body && (is_define(car(body))); body = cdr(body));
	cell_p ret = cons(args, cons(define_begin, cons(body, NULL)));
	for(; body && (!is_define(car(body))); body = cdr(body));
	if(body) {
		puts("=============");
		print_list(root);
		puts("\n=============");
	}
	assert(!body);
	return ret;
}

cell_p rewrite_define_aux(cell_p root, cell_p frame) {
	assert(is_lambda(root));
	cell_p destructed = destruct_lambda(root);
	cell_p args = car_cdnr(destructed, 0);
	cell_p new_frame = cons(args, frame);
	cell_p define_begin = car_cdnr(destructed, 1);
	cell_p define_end = car_cdnr(destructed, 2);
	if(define_begin == define_end) {
		for(cell_p body = define_end; body; body = cdr(body)) {
			car(body) = rewrite_define(car(body), new_frame);
		}
		return root;
	}
	
	cell_p set_exp_list = make_set_exp_list(define_begin, define_end);
	cell_p new_lambda = make_lambda(car(set_exp_list), append(cdr(set_exp_list), define_end));
	new_lambda = rewrite_define_aux(new_lambda, frame);
	cell_p nil_list = make_nil_list(car(set_exp_list), new_frame);
	car(define_begin) = cons(new_lambda, nil_list);
	cdr(define_begin) = NULL;
	return root;
}

cell_p rewrite_define(cell_p root, cell_p frame) {
	if(!root) return root;
	switch(cty(root)) {
		case ATOM:
		case NUMBER:
		return root;
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
					switch(i) {
						case K_lambda: {
							return rewrite_define_aux(root, frame);
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
				car(c) = rewrite_define(car(c), frame);
			}
			return root;
		} default: {
			assert(false);
		}
	}
}

genvar(cps, "継続")

#ifndef CPS2

// (lambda (args) bodies)
/* (cont
   (lambda (k0 args) to_cps(body1,
   (lambda (_) to_cps(body2,
   (lambda (_) to_cps(body...,
   (lambda (_) to_cps(bodyn, k0)))))))))
   */

cell_p body_to_cps(cell_p bodies, cell_p cont_var) {
	if(!cdr(bodies)) return to_cps(car(bodies), cont_var);
	cell_p new_lambda = make_lambda(cons(str_to_atom("_"), NULL), cons(body_to_cps(cdr(bodies), cont_var), NULL));
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
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p apply_cont = cons(car(cont_vars), cons(cont, cdr(cont_vars)));
	return apply_to_cps_aux(root, cont_vars, apply_cont);
}

cell_p predefined_to_cps(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = cdr(root); e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p predefined_cont = cons(cont, cons(cons(car(root), cont_vars), NULL));
	return apply_to_cps_aux(cdr(root), cont_vars, predefined_cont);
}

cell_p to_cps(cell_p root, cell_p cont) {
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			// v
			// (cont v)
			return cons(cont, cons(root, NULL));
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
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
							cell_p new_var0 = genvar_cps();
							cell_p new_var1 = genvar_cps();
							cell_p new_then_cls = to_cps(then_cls, new_var0);
							cell_p new_else_cls = to_cps(else_cls, new_var0);
							cell_p new_if = app4(str_to_atom("if"), new_var1, new_then_cls, new_else_cls);
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
							cell_p new_var = genvar_cps();
							cell_p body = cdr(cdr(root));
							cell_p new_args = cons(new_var, car_cdnr(root, 1));
							cell_p new_body = body_to_cps(body, new_var);
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
							cell_p new_var = genvar_cps();
							cell_p body = cons(cont, cons(cons(str_to_atom("set!"), cons(var, cons(new_var, NULL))), NULL));
							cell_p new_cont = make_lambda(cons(new_var, NULL), cons(body, NULL));
							 return to_cps(exp, new_cont);
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_predefined[i](root)) {
					return predefined_to_cps(root, cont);
				}
			}
			return apply_to_cps(root, cont);
		} default: {
			assert(false);
		}
	}
}

// closure 変換をする
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
// (e1 e2 …en)は(クロージャ適用 e1 e2 … en)とする
// 最初から(lambda args ...) や(lambda (head . tail) ...) みたいになっているlambda式はどうする？
//     env             env
//    /   \      →   /   \
// env'  args      env'   []
//                       /  \
//                    args  nil
//
// ((lambda args ...) 1 2 3 4)の場合
// ((lambda env ...) env 1 2 3 4)となり
// argsで参照していたところは(car-cdnr env 1)
// (set! args ...) → (set-car! (cdnr env 1) ...)
//
//     env             env
//    /   \      →   /   \
// env'   []      env'    []
//       /  \            /  \
//    head1 []        head1 []
//         /  \            /  \
//      head2 tail      head2 []
//                           /  \
//                        tail  nil
//
// ((lambda (head1 head2 . tail) ...) 1 2 3 4)の場合
// ((lambda env ...) env 1 2 3 4)となり
// head1で参照していたところは(car-cdnr env 1)，head2は(car-cdnr env 2)でいい？
// tailは(car-cdnr env 3)になるね
// (set! head1 ...) →(set-car! (cdnr env 1) ...)，
// (set! head2 ...) →(set-car! (cdnr env 2) ...)，
// (set! tail ...) →(set-cdr! (cdnr env 3) ...)
//
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
cell_p body_to_closure(cell_p bodies, cell_p frame) {
	if(!bodies) return NULL;
	cell_p new_body = to_closure(car(bodies), frame);
	return cons(new_body, body_to_closure(cdr(bodies), frame));
}

cell_p lambda_to_closure(cell_p lambda, cell_p frame) {
	//puts("======= before =============");
	//print_list(lambda);
	//puts("\n==========================");
	cell_p destructed = destruct_lambda(lambda);
	cell_p args = car_cdnr(destructed, 0);
	cell_p define_begin = car_cdnr(destructed, 1);
	cell_p body = car_cdnr(destructed, 2);
	assert(define_begin == body);
	
	cell_p new_frame = cons(formal_args_to_list(args), frame);
	size_t len_args = length(car(new_frame));
	cell_p preprocess = NULL;
	if(is_dotted_list(args)) {
		preprocess = append(preprocess, cons(app3(str_to_atom("move末尾to通常の引数の場所"), str_to_atom("環境"), int_to_atom(len_args)), NULL));
		preprocess = body_to_closure(preprocess, new_frame);
	}
	//puts("======= preprocess 1=============");
	//print_list(preprocess);
	//puts("\n==============================");

	cell_p new_body = append(preprocess, body_to_closure(body, new_frame));
	//puts("======= new_body =============");
	//print_list(new_body);
	//puts("\n==============================");

	cell_p ret = make_lambda(str_to_atom("環境"), new_body);
	//puts("======= after =============");
	//print_list(ret);
	//puts("\n==========================");
	return ret;
}

cell_p atom_to_closure(cell_p atom, cell_p frame) {
	assert(is(ATOM, atom));
	cell_p env = str_to_atom("環境");
	size_t idx = 0;
	size_t depth = 0;
	for(; frame; frame = cdr(frame)) {
		if(is_args(atom, car(frame))) {
			idx = 1 + index_of_atom(atom, car(frame));
			break;
		}
		depth++;
	}
	assert(frame);
	return app3(str_to_atom("変数の取得"), app3(str_to_atom("上の環境へ"), env, int_to_atom(depth)), int_to_atom(idx));
}

cell_p set_to_closure(cell_p root, cell_p frame) {
	cell_p var = car_cdnr(root, 1);
	cell_p exp = car_cdnr(root, 2);

	cell_p env = str_to_atom("環境");
	size_t idx = 0;
	size_t depth = 0;
	for(cell_p f = frame; f; f = cdr(f)) {
		if(is_args(var, car(f))) {
			idx = 1 + index_of_atom(var, car(f));
			break;
		}
		depth++;
	}

	return app3(str_to_atom("変数への代入"), app3(str_to_atom("to左辺値"), app3(str_to_atom("上の環境へ"), env, int_to_atom(depth)), int_to_atom(idx)), to_closure(exp, frame));
}

cell_p predefined_to_closure(cell_p root, cell_p frame) {
	for(cell_p c = cdr(root); c; c = cdr(c)) {
		car(c) = to_closure(car(c), frame);
	}
	return root;
}

cell_p to_closure(cell_p root, cell_p frame) {
	if(!root) return NULL;
	switch(cty(root)) {
		case ATOM: {
			//puts("======= ATOM: to_closure=============");
			//print_list(root);
			//puts("\n");
			//print_list(frame);
			//printf("\n============= %d =================\n", is_bound(root, frame));
			if(!is_bound(root, frame)) return root;
			return atom_to_closure(root, frame);
		}
		case NUMBER: {
			return root;
		}
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
					switch(i) {
						case K_q:
						case K_quote: {
							return root;
						} case K_if: {
							// (if cond t e)
							cell_p cond = car_cdnr(root, 1);
							cell_p then_cls = car_cdnr(root, 2);
							cell_p else_cls = car_cdnr(root, 3);
							cell_p new_cond = to_closure(cond, frame);
							cell_p new_then_cls = to_closure(then_cls, frame);
							cell_p new_else_cls = to_closure(else_cls, frame);
							cell_p new_if = app4(str_to_atom("if"), new_cond, new_then_cls, new_else_cls);
							return new_if;
						} case K_lambda: {
							// (lambda (args) bodies)
							return app3(str_to_atom("クロージャ作成"), lambda_to_closure(root, frame), str_to_atom("環境"));
						}
						case K_define: {
							// this case should not be appeared
							assert(false);
							return NULL;
						}
						case K_set: {
							// (set! v e)
							return set_to_closure(root, frame);
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_predefined[i](root)) {
					return predefined_to_closure(root, frame);
				}
			}
			for(cell_p c = root; c; c = cdr(c)) {
				car(c) = to_closure(car(c), frame);
			}
			//return cons(str_to_atom("クロージャ適用"), root);
			return root;
		} default: {
			assert(false);
		}
	}
}
#else

cell_p body_to_cps2(cell_p bodies, cell_p cont_var) {
	return app2(to_cps2(car(bodies)), cdr(bodies)?make_lambda(cons(str_to_atom("_"), NULL), cons(body_to_cps2(cdr(bodies), cont_var), NULL)):cont_var);
}

cell_p apply_to_cps_aux2(cell_p exp, cell_p cont_vars, cell_p cont) {
	cell_p body = cdr(exp)?apply_to_cps_aux2(cdr(exp), cdr(cont_vars), cont):cont;
	cell_p new_cont = make_lambda(cons(car(cont_vars), NULL), cons(body, NULL));
	return app2(to_cps2(car(exp)), new_cont);
}
cell_p apply_to_cps2(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = root; e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p apply_cont = cons(car(cont_vars), cons(cont, cdr(cont_vars)));
	return apply_to_cps_aux2(root, cont_vars, apply_cont);
}

cell_p predefined_to_cps2(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = cdr(root); e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p predefined_cont = cons(cont, cons(cons(car(root), cont_vars), NULL));
	return apply_to_cps_aux2(cdr(root), cont_vars, predefined_cont);
}

cell_p to_cps2(cell_p root) {
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			cell_p k = genvar_cps();
			return make_lambda(cons(k, NULL), cons(app2(k, root), NULL));
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
					switch(i) {
						case K_q:
						case K_quote: {
							cell_p k = genvar_cps();
							return make_lambda(cons(k, NULL), cons(app2(k, root), NULL));
						} case K_if: {
							cell_p cond = car_cdnr(root, 1);
							cell_p then_cls = car_cdnr(root, 2);
							cell_p else_cls = car_cdnr(root, 3);
							cell_p new_var0 = genvar_cps();
							cell_p new_var1 = genvar_cps();
							cell_p new_then_cls = to_cps(then_cls, new_var0);
							cell_p new_else_cls = to_cps(else_cls, new_var0);
							cell_p new_if = app4(str_to_atom("if"), new_var1, new_then_cls, new_else_cls);
							cell_p new_cont = make_lambda(cons(new_var1, NULL), cons(new_if, NULL));
							cell_p new_lambda = make_lambda(cons(new_var0, NULL), cons(to_cps(cond, new_cont), NULL));
							return new_lambda;
						} case K_lambda: {
							cell_p new_var = genvar_cps();
							cell_p body = cdr(cdr(root));
							cell_p new_args = cons(new_var, car_cdnr(root, 1));
							cell_p new_body = body_to_cps2(body, new_var);
							cell_p new_lambda = make_lambda(new_args, cons(new_body, NULL));
							cell_p k = genvar_cps();
							 return make_lambda(cons(k, NULL), cons(app2(k, new_lambda), NULL));
						}
						case K_define: {
							// this case should not be appeared
							assert(false);
							return NULL;
						}
						case K_set: {
							cell_p var = car_cdnr(root, 1);
							cell_p exp = car_cdnr(root, 2);
							cell_p new_var = genvar_cps();
							cell_p k = genvar_cps();
							cell_p body = cons(k, cons(cons(str_to_atom("set!"), cons(var, cons(new_var, NULL))), NULL));
							cell_p new_cont = make_lambda(cons(new_var, NULL), cons(body, NULL));

							cell_p new_lambda = make_lambda(cons(k, NULL), cons(app2(to_cps2(exp), new_cont), NULL));
							 return new_lambda;
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_predefined[i](root)) {
					cell_p k = genvar_cps();
					return make_lambda(cons(k, NULL), cons(predefined_to_cps2(root, k), NULL));
				}
			}
			cell_p k = genvar_cps();
			return make_lambda(cons(k, NULL), cons(apply_to_cps2(root, k), NULL));
		} default: {
			assert(false);
		}
	}
}


#endif
